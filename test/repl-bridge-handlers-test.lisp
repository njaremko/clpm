;;;; test/repl-bridge-handlers-test.lisp - declarative handlers + break.
;;;;
;;;; Covers BRIDGE_V2 #115 (break enters debugger), #117 (--handlers).

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

(defun assert-equal-string (expected actual)
  (unless (and (stringp actual) (string= expected actual))
    (fail "expected ~S, got ~S" expected actual)))

(defun lookup (object key)
  (when (and (consp object) (eq (car object) :object))
    (cdr (assoc key (cadr object) :test #'string=))))

(defun with-daemon (fn)
  (let* ((sock (format nil "/tmp/clpm-rb-handlers-~A.sock" (random (expt 2 32))))
         (thread (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (clpm.repl-bridge:start-server :socket-path sock)
                      (error (c) (format *error-output* "daemon: ~A~%" c))))
                  :name "test-bridge-handlers")))
    (unwind-protect
         (progn
           (loop for i from 0 below 50
                 while (not (probe-file sock))
                 do (sleep 0.05))
           (assert-true (probe-file sock) "daemon never started")
           (funcall fn sock))
      (handler-case (clpm.repl-bridge:send-request sock "shutdown")
        (error () nil))
      (loop for i from 0 below 30
            while (sb-thread:thread-alive-p thread)
            do (sleep 0.05))
      (when (sb-thread:thread-alive-p thread)
        (ignore-errors (sb-thread:terminate-thread thread)))
      (ignore-errors (delete-file sock)))))

;;; ----------------------------------------------------------------------------
;;; #117 acceptance: (/ 1 0) recovered by USE-VALUE 999 declaratively.

(format t "Test: --handlers redirects division-by-zero to USE-VALUE 999~%")
(with-daemon
  (lambda (sock)
    (let* ((handlers (list :array
                           (list (list :object
                                       (list (cons "type" "division-by-zero")
                                             (cons "restart" "use-value")
                                             (cons "args" (list :array
                                                                (list "999"))))))))
           (resp (clpm.repl-bridge:send-request
                  sock "eval"
                  :params (list :object
                                (list (cons "form"
                                            "(restart-case (/ 1 0)
                                               (use-value (v) v))")
                                      (cons "handlers" handlers)))))
           (result (lookup resp "result")))
      (assert-true result "expected success, got ~S" resp)
      (assert-equal-string "999" (lookup result "value")))))
(format t "  declarative USE-VALUE OK~%")

;;; ----------------------------------------------------------------------------
;;; #115: `(break)` in --debug mode emits debugger-entered; CONTINUE resumes.

(format t "Test: (break) under --debug enters debugger, CONTINUE resumes~%")
(with-daemon
  (lambda (sock)
    (let* ((conn (clpm.repl-bridge:open-connection sock))
           (eval-id 15)
           (entered nil))
      (unwind-protect
           (let ((resp
                   (clpm.repl-bridge:send-on-connection
                    conn "eval"
                    :id eval-id
                    :params (list :object
                                  (list (cons "form"
                                              "(progn
                                                 (defparameter *brk-x* 1)
                                                 (break \"look\")
                                                 (incf *brk-x*))")
                                        (cons "debug" t)))
                    :on-event
                    (lambda (frame)
                      (when (string= "debugger-entered" (lookup frame "event"))
                        (setf entered t)
                        (clpm.repl-bridge.compat:make-thread
                         (lambda ()
                           (clpm.repl-bridge::%write-line-json
                            (clpm.repl-bridge::connection-stream conn)
                            (clpm.repl-bridge::%json-object
                             "id" eval-id
                             "method" "debug-continue")))
                         :name "test-handler-resumer"))
                      nil))))
             (assert-true entered "never entered debugger from break")
             (let ((result (lookup resp "result")))
               (assert-true result "no terminal result: ~S" resp)
               (assert-equal-string "2" (lookup result "value"))))
        (clpm.repl-bridge:close-connection conn)))))
(format t "  break + continue OK~%")

(format t "~%REPL-bridge handlers tests PASSED!~%")
(sb-ext:exit :code 0)
