;;;; test/repl-intro-test.lisp - introspection RPCs.
;;;;
;;;; Covers BRIDGE_V2 #140 (apropos), #141 (documentation), #142 (arglist),
;;;; #143 (complete-symbol), #144 (package-info), #145 (class-info),
;;;; #146 (function-info), #147 (disassemble), #148 (describe-system).

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

(defun lookup (object key)
  (when (and (consp object) (eq (car object) :object))
    (cdr (assoc key (cadr object) :test #'string=))))

(defun array-items (a)
  (when (and (consp a) (eq (car a) :array))
    (cadr a)))

(defun with-daemon (fn)
  (let* ((sock (format nil "/tmp/clpm-rb-intro-~A.sock" (random (expt 2 32))))
         (thread (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (clpm.repl:start-server :socket-path sock)
                      (error (c) (format *error-output* "daemon: ~A~%" c))))
                  :name "test-bridge-intro")))
    (unwind-protect
         (progn
           (loop for i from 0 below 50
                 while (not (probe-file sock))
                 do (sleep 0.05))
           (assert-true (probe-file sock) "daemon never started")
           (funcall fn sock))
      (handler-case (clpm.repl:send-request sock "shutdown")
        (error () nil))
      (loop for i from 0 below 30
            while (sb-thread:thread-alive-p thread)
            do (sleep 0.05))
      (when (sb-thread:thread-alive-p thread)
        (ignore-errors (sb-thread:terminate-thread thread)))
      (ignore-errors (delete-file sock)))))

(defun do-rpc (sock method params)
  (clpm.repl:send-request sock method
                                  :params (list :object params)))

;;; ----------------------------------------------------------------------------
;;; #140 apropos: search for FORMAT.

(format t "Test: apropos finds FORMAT~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (do-rpc sock "apropos" (list (cons "pattern" "format"))))
           (entries (array-items (lookup (lookup resp "result") "entries"))))
      (assert-true entries "no entries: ~S" resp)
      (assert-true (find "FORMAT" entries
                          :test (lambda (s e) (string= s (lookup e "name"))))
                   "FORMAT not in entries"))))
(format t "  apropos OK~%")

;;; ----------------------------------------------------------------------------
;;; #141 documentation: CL:CAR has docstring text.

(format t "Test: documentation lookup is well-formed~%")
(with-daemon
  (lambda (sock)
    ;; Pick a symbol we *define* to guarantee a docstring exists.
    (clpm.repl:send-request
     sock "eval"
     :params (list :object
                   (list (cons "form"
                               "(defun intro-test-fn () \"hello docs\" 1)"))))
    (let* ((resp (do-rpc sock "documentation"
                          (list (cons "symbol" "intro-test-fn")
                                (cons "package" "common-lisp-user")
                                (cons "type" "function"))))
           (doc (lookup (lookup resp "result") "doc")))
      (assert-true (and (stringp doc) (string= "hello docs" doc))
                   "expected `hello docs', got ~S" doc))))
(format t "  documentation OK~%")

;;; ----------------------------------------------------------------------------
;;; #142 arglist: MAPCAR takes function + list arguments.

(format t "Test: arglist of MAPCAR~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (do-rpc sock "arglist"
                          (list (cons "symbol" "mapcar")
                                (cons "package" "CL"))))
           (al (lookup (lookup resp "result") "arglist")))
      (assert-true (and (stringp al)
                        (search "FUNCTION" al :test #'char-equal))
                   "expected FUNCTION in arglist, got ~S" al))))
(format t "  arglist OK~%")

;;; ----------------------------------------------------------------------------
;;; #143 complete-symbol: prefix MAP finds MAPCAR, MAPC, MAP, etc.

(format t "Test: complete-symbol PREFIX=MAP~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (do-rpc sock "complete-symbol"
                          (list (cons "prefix" "map")
                                (cons "package" "CL"))))
           (candidates (array-items
                        (lookup (lookup resp "result") "candidates"))))
      (assert-true (find "MAPCAR" candidates :test #'string=)
                   "MAPCAR missing: ~S" candidates))))
(format t "  complete-symbol OK~%")

;;; ----------------------------------------------------------------------------
;;; #144 package-info: COMMON-LISP has many exports.

(format t "Test: package-info CL~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (do-rpc sock "package-info" (list (cons "name" "CL"))))
           (result (lookup resp "result")))
      (assert-true result "no result: ~S" resp)
      (assert-true (>= (lookup result "export_count") 700)
                   "CL should have many exports, got ~A"
                   (lookup result "export_count")))))
(format t "  package-info OK~%")

;;; ----------------------------------------------------------------------------
;;; #145 class-info: STANDARD-CLASS has supers and a precedence list.

(format t "Test: class-info STANDARD-CLASS~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (do-rpc sock "class-info"
                          (list (cons "name" "standard-class")
                                (cons "package" "CL"))))
           (result (lookup resp "result"))
           (precedence (array-items (lookup result "precedence"))))
      (assert-true precedence "no precedence list: ~S" resp)
      (assert-true (find "STANDARD-OBJECT" precedence :test #'string=)
                   "STANDARD-OBJECT not in precedence list: ~S"
                   precedence))))
(format t "  class-info OK~%")

;;; ----------------------------------------------------------------------------
;;; #146 function-info: CAR is a function, has a docstring.

(format t "Test: function-info CL:CAR~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (do-rpc sock "function-info"
                          (list (cons "symbol" "car")
                                (cons "package" "CL"))))
           (result (lookup resp "result")))
      (assert-true result "no result")
      (assert-true (stringp (lookup result "documentation"))
                   "CAR should have docstring")
      (assert-true (not (lookup result "macro_p"))
                   "CAR should not be a macro"))))
(format t "  function-info OK~%")

;;; ----------------------------------------------------------------------------
;;; #147 disassemble: CL:CAR yields non-empty output.

(format t "Test: disassemble CL:CAR~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (do-rpc sock "disassemble"
                          (list (cons "symbol" "car")
                                (cons "package" "CL"))))
           (output (lookup (lookup resp "result") "output")))
      (assert-true (and (stringp output) (plusp (length output)))
                   "empty disassembly"))))
(format t "  disassemble OK~%")

;;; ----------------------------------------------------------------------------
;;; #148 describe-system: clpm itself is loaded; we can introspect it.

(format t "Test: describe-system clpm~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (do-rpc sock "describe-system" (list (cons "name" "clpm"))))
           (result (lookup resp "result")))
      (assert-true result "no result: ~S" resp)
      (assert-true (string-equal "clpm" (lookup result "name"))
                   "wrong name: ~S" (lookup result "name")))))
(format t "  describe-system OK~%")

(format t "~%REPL introspection tests PASSED!~%")
(sb-ext:exit :code 0)
