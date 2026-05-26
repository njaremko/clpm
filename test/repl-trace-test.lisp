;;;; test/repl-trace-test.lisp - trace RPCs.
;;;;
;;;; Covers BRIDGE_V2 #160 (trace), #161 (untrace), #162 (list-traced),
;;;; and rejects eval-shaped timing/profile aliases.

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
  (let* ((sock (format nil "/tmp/clpm-rb-trace-~A.sock" (random (expt 2 32))))
         (thread (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (clpm.repl:start-server :socket-path sock)
                      (error (c) (format *error-output* "daemon: ~A~%" c))))
                  :name "test-bridge-trace")))
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
;;; #160-#161-#162: trace / list-traced / untrace.

(format t "Test: trace + list-traced + untrace~%")
(with-daemon
  (lambda (sock)
    ;; Define a target function.
    (do-rpc sock "eval"
            (list (cons "form" "(defun trace-target (x) (1+ x))")))
    (let ((tr (do-rpc sock "trace"
                       (list (cons "symbols"
                                    (list :array (list "trace-target")))))))
      (assert-true (lookup tr "result") "trace returned error: ~S" tr))
    (let* ((lst (do-rpc sock "list-traced" nil))
           (entries (array-items (lookup (lookup lst "result") "entries"))))
      (assert-true (find "TRACE-TARGET" entries
                          :test (lambda (s e) (search s e :test #'char-equal)))
                   "list-traced should mention TRACE-TARGET: ~S" entries))
    (let* ((call (do-rpc sock "eval"
                         (list (cons "form" "(trace-target 41)"))))
           (result (lookup call "result"))
           (output (and result (lookup result "output"))))
      (assert-true result "traced call returned error: ~S" call)
      (assert-true (and (stringp output)
                        (search "TRACE-TARGET" output :test #'char-equal))
                   "traced call did not emit trace output: ~S" output))
    (let ((un (do-rpc sock "untrace"
                       (list (cons "symbols"
                                    (list :array (list "trace-target")))))))
      (assert-true (lookup un "result") "untrace returned error: ~S" un))))
(format t "  trace/untrace OK~%")

(format t "Test: trace survives function redefinition without stale untrace~%")
(with-daemon
  (lambda (sock)
    (do-rpc sock "eval"
            (list (cons "form" "(defun trace-redef-target () :old)")))
    (let ((tr (do-rpc sock "trace"
                       (list (cons "symbols"
                                    (list :array
                                          (list "trace-redef-target")))))))
      (assert-true (lookup tr "result") "trace returned error: ~S" tr))
    (let ((redef (do-rpc sock "eval"
                         (list (cons "form"
                                     "(defun trace-redef-target () :new)")))))
      (assert-true (lookup redef "result") "redefinition failed: ~S" redef))
    (let* ((call (do-rpc sock "eval"
                         (list (cons "form" "(trace-redef-target)"))))
           (result (lookup call "result"))
           (output (and result (lookup result "output"))))
      (assert-true result "traced redefined call failed: ~S" call)
      (assert-true (string= ":NEW" (lookup result "value"))
                   "redefined function did not run: ~S" result)
      (assert-true (and (stringp output)
                        (search "TRACE-REDEF-TARGET" output
                                :test #'char-equal))
                   "trace vanished after redefinition: ~S" output))
    (let ((un (do-rpc sock "untrace"
                       (list (cons "symbols"
                                    (list :array
                                          (list "trace-redef-target")))))))
      (assert-true (lookup un "result") "untrace returned error: ~S" un))
    (let* ((call (do-rpc sock "eval"
                         (list (cons "form" "(trace-redef-target)"))))
           (result (lookup call "result")))
      (assert-true result "untraced redefined call failed: ~S" call)
      (assert-true (string= ":NEW" (lookup result "value"))
                   "untrace restored stale function: ~S" result))))
(format t "  trace redefinition OK~%")

;;; ----------------------------------------------------------------------------
;;; Eval-shaped timing/profile aliases are not public RPCs.

(format t "Test: eval-shaped timing/profile aliases are rejected~%")
(with-daemon
  (lambda (sock)
    (dolist (method '("time-eval" "profile-eval"))
      (let* ((resp (do-rpc sock method
                           (list (cons "form" "(loop for i from 0 below 1000 sum i)"))))
             (err (lookup resp "error")))
        (assert-true err "expected ~A to be rejected, got: ~S" method resp)
        (assert-true (search "unknown method" (lookup err "message"))
                     "wrong ~A error: ~S" method err)))))
(format t "  eval alias rejection OK~%")

(format t "~%REPL trace tests PASSED!~%")
(sb-ext:exit :code 0)
