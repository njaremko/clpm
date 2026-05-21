;;;; test/repl-eval-test.lisp - eval worker, output capture, conditions

(require :asdf)
(require :sb-bsd-sockets)

(let* ((this-file (or *load-truename* *load-pathname*))
       (test-dir (uiop:pathname-directory-pathname this-file))
       (repo-root (uiop:pathname-parent-directory-pathname test-dir)))
  (push repo-root asdf:*central-registry*))

(format t "Loading CLPM...~%")
(handler-case
    (asdf:load-system :clpm :verbose nil)
  (error (c)
    (format *error-output* "Failed to load CLPM: ~A~%" c)
    (sb-ext:exit :code 1)))
(format t "CLPM loaded.~%")

(defun fail (fmt &rest args)
  (apply #'format *error-output* (concatenate 'string "FAIL: " fmt "~%") args)
  (sb-ext:exit :code 1))

(defun assert-true (x fmt &rest args)
  (unless x (apply #'fail fmt args)))

(defun assert-eql (expected actual)
  (unless (eql expected actual)
    (fail "expected ~S, got ~S" expected actual)))

(defun assert-string= (expected actual)
  (unless (and (stringp actual) (string= expected actual))
    (fail "expected ~S, got ~S" expected actual)))

(defun lookup (object key)
  (when (and (consp object) (eq (car object) :object))
    (let ((cell (assoc key (cadr object) :test #'string=)))
      (and cell (cdr cell)))))

(defun array-items (json)
  (when (and (consp json) (eq (car json) :array))
    (cadr json)))

(defun with-daemon (fn)
  (let* ((tmp (format nil "/tmp/clpm-bridge-eval-test-~A.sock"
                      (random (expt 2 32))))
         (server-thread
           (sb-thread:make-thread
            (lambda ()
              (handler-case
                  (clpm.repl:start-server :socket-path tmp)
                (error (c)
                  (format *error-output* "daemon: ~A~%" c))))
            :name "test-bridge")))
    (unwind-protect
         (progn
           (loop for i from 0 below 50
                 while (not (probe-file tmp))
                 do (sleep 0.05))
           (unless (probe-file tmp)
             (fail "daemon failed to bind"))
           (funcall fn tmp))
      (handler-case (clpm.repl:send-request tmp "shutdown") (error () nil))
      (loop for i from 0 below 30
            while (sb-thread:thread-alive-p server-thread)
            do (sleep 0.05))
      (when (sb-thread:thread-alive-p server-thread)
        (ignore-errors (sb-thread:terminate-thread server-thread)))
      (ignore-errors (delete-file tmp)))))

(defun do-eval (sock form &optional package)
  (clpm.repl:send-request
   sock "eval"
   :params (list :object
                 (append (list (cons "form" form))
                         (when package (list (cons "package" package)))))))

;;; ----------------------------------------------------------------------------

(format t "Test: simple arithmetic returns value~%")
(with-daemon
    (lambda (sock)
      (let* ((resp (do-eval sock "(+ 1 2)"))
             (result (lookup resp "result")))
        (assert-true result "expected result, got ~S" resp)
        (assert-string= "3" (lookup result "value"))
        (assert-string= "" (lookup result "output"))
        (assert-true (integerp (lookup result "elapsed_ms"))
                     "expected elapsed_ms"))))
(format t "  OK~%")

(format t "Test: stdout capture~%")
(with-daemon
    (lambda (sock)
      (let* ((resp (do-eval sock "(progn (format t \"hi\") :done)"))
             (result (lookup resp "result")))
        (assert-true result "expected result, got ~S" resp)
        (assert-string= ":DONE" (lookup result "value"))
        (assert-string= "hi" (lookup result "output")))))
(format t "  OK~%")

(format t "Test: reader-error on malformed form~%")
(with-daemon
    (lambda (sock)
      (let* ((resp (do-eval sock "("))
             (err (lookup resp "error")))
        (assert-true err "expected error, got ~S" resp)
        (assert-string= "reader-error" (lookup err "code")))))
(format t "  OK~%")

(format t "Test: eval-error captures condition~%")
(with-daemon
    (lambda (sock)
      (let* ((resp (do-eval sock "(error \"boom\")"))
             (err (lookup resp "error"))
             (details (lookup err "details"))
             (conds (and details (array-items (lookup details "conditions")))))
        (assert-true err "expected error, got ~S" resp)
        (assert-string= "eval-error" (lookup err "code"))
        (assert-true (search "boom" (lookup err "message"))
                     "expected message to contain 'boom', got ~S" (lookup err "message"))
        (assert-true (and conds (>= (length conds) 1)) "expected conditions")
        (let* ((c0 (first conds))
               (restarts (array-items (lookup c0 "restarts"))))
          (assert-true (search "SIMPLE-ERROR" (lookup c0 "type"))
                       "expected SIMPLE-ERROR type, got ~S" (lookup c0 "type"))
          ;; v2 restart shape: a list of {name, report, interactive, args_arity}
          ;; objects. ABORT must appear by name.
          (assert-true (find "ABORT" restarts
                             :test (lambda (s r) (string= s (lookup r "name"))))
                       "expected ABORT restart, got ~S" restarts)
          (assert-true (consp (lookup c0 "backtrace"))
                       "expected backtrace array")))))
(format t "  OK~%")

(format t "Test: handled condition not in conditions list~%")
(with-daemon
    (lambda (sock)
      (let* ((resp (do-eval sock "(handler-case (error \"x\") (error () 1))"))
             (result (lookup resp "result"))
             (conds (and result (array-items (lookup result "conditions")))))
        (assert-true result "expected success, got ~S" resp)
        (assert-string= "1" (lookup result "value"))
        (assert-eql 0 (length conds)))))
(format t "  OK~%")

(format t "Test: read from *standard-input* returns EOF (no hang)~%")
(with-daemon
    (lambda (sock)
      (let* ((resp (do-eval sock "(read *standard-input* nil :eof)"))
             (result (lookup resp "result")))
        (assert-true result "expected result, got ~S" resp)
        (assert-string= ":EOF" (lookup result "value")))))
(format t "  OK~%")

(format t "Test: state persists across evals~%")
(with-daemon
    (lambda (sock)
      (let* ((r1 (do-eval sock "(defparameter *test-x* 41)"))
             (r2 (do-eval sock "(incf *test-x*)"))
             (r3 (do-eval sock "*test-x*")))
        (assert-true (lookup r1 "result") "1: ~S" r1)
        (assert-true (lookup r2 "result") "2: ~S" r2)
        (assert-string= "42" (lookup (lookup r3 "result") "value")))))
(format t "  OK~%")

(format t "Test: package override is per-call; persistent unchanged~%")
(with-daemon
    (lambda (sock)
      ;; package-name returns a string, so prin1 adds quotes.
      (let* ((r1 (do-eval sock "(package-name *package*)"))
             (r2 (do-eval sock "(package-name *package*)" "common-lisp"))
             (r3 (do-eval sock "(package-name *package*)")))
        (assert-string= "\"COMMON-LISP-USER\"" (lookup (lookup r1 "result") "value"))
        (assert-string= "\"COMMON-LISP\"" (lookup (lookup r2 "result") "value"))
        (assert-string= "\"COMMON-LISP-USER\"" (lookup (lookup r3 "result") "value"))
        ;; The `package' field in the result is the bare name though.
        (assert-string= "COMMON-LISP-USER" (lookup (lookup r1 "result") "package"))
        (assert-string= "COMMON-LISP" (lookup (lookup r2 "result") "package"))
        (assert-string= "COMMON-LISP-USER" (lookup (lookup r3 "result") "package")))))
(format t "  OK~%")

(format t "Test: in-package mutates persistent state~%")
(with-daemon
    (lambda (sock)
      (let* ((r1 (do-eval sock "(defpackage :clpm-bridge-test-pkg (:use :cl))"))
             (r2 (do-eval sock "(in-package :clpm-bridge-test-pkg)"))
             (r3 (do-eval sock "(package-name *package*)")))
        (assert-true (lookup r1 "result") "1: ~S" r1)
        (assert-true (lookup r2 "result") "2: ~S" r2)
        (assert-string= "CLPM-BRIDGE-TEST-PKG"
                        (lookup (lookup r3 "result") "package")))))
(format t "  OK~%")

(format t "Test: persistent package is used while reading future forms~%")
(with-daemon
    (lambda (sock)
      (do-eval sock "(defpackage :clpm-bridge-read-pkg (:use :cl))")
      (do-eval sock "(in-package :clpm-bridge-read-pkg)")
      (let* ((def (do-eval sock "(defun package-read-target () :ok)"))
             (probe (do-eval
                     sock
                     "(list (not (null (fboundp 'clpm-bridge-read-pkg::package-read-target)))
                            (not (null (fboundp 'cl-user::package-read-target))))"))
             (result (lookup probe "result")))
        (assert-true (lookup def "result") "defun failed: ~S" def)
        (assert-string= "(T NIL)"
                        (lookup result "value")))))
(format t "  OK~%")

(format t "Test: redefinition tracked~%")
(with-daemon
    (lambda (sock)
      (do-eval sock "(defun bridge-test-foo () 1)")
      (let* ((resp (clpm.repl:send-request sock "list-redefinitions"))
             (result (lookup resp "result"))
             (entries (array-items (lookup result "entries"))))
        (assert-true (some (lambda (e)
                             (and (string= "defun" (lookup e "kind"))
                                  (string= "BRIDGE-TEST-FOO" (lookup e "name"))))
                           entries)
                     "expected BRIDGE-TEST-FOO redef entry, got ~S" entries))))
(format t "  OK~%")

(format t "Test: set-package method~%")
(with-daemon
    (lambda (sock)
      (let* ((r1 (clpm.repl:send-request
                  sock "set-package"
                  :params (list :object (list (cons "name" "common-lisp")))))
             (r2 (clpm.repl:send-request sock "current-package")))
        (assert-string= "COMMON-LISP" (lookup (lookup r1 "result") "package"))
        (assert-string= "COMMON-LISP" (lookup (lookup r2 "result") "package")))))
(format t "  OK~%")

(format t "Test: describe method~%")
(with-daemon
    (lambda (sock)
      (let* ((resp (clpm.repl:send-request
                    sock "describe"
                    :params (list :object (list (cons "symbol" "car")
                                                (cons "package" "common-lisp")))))
             (result (lookup resp "result"))
             (output (lookup result "output")))
        (assert-true (and (stringp output) (search "CAR" output))
                     "expected description containing CAR, got ~S" output))))
(format t "  OK~%")

(format t "Test: reset clears redefinition log~%")
(with-daemon
    (lambda (sock)
      (do-eval sock "(defun bridge-test-bar () 2)")
      (let ((before (array-items
                     (lookup (lookup (clpm.repl:send-request
                                      sock "list-redefinitions")
                                     "result")
                             "entries"))))
        (assert-true (plusp (length before)) "expected entries before reset"))
      (clpm.repl:send-request sock "reset")
      (let ((after (array-items
                    (lookup (lookup (clpm.repl:send-request
                                     sock "list-redefinitions")
                                    "result")
                            "entries"))))
        (assert-eql 0 (length after)))))
(format t "  OK~%")

(format t "~%REPL eval tests PASSED!~%")
(sb-ext:exit :code 0)
