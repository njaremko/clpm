;;;; test/repl-trace-test.lisp - trace / time / profile.
;;;;
;;;; Covers BRIDGE_V2 #160 (trace), #161 (untrace), #162 (list-traced),
;;;; #163 (time-eval), #164 (profile-eval).

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
    (let ((un (do-rpc sock "untrace"
                       (list (cons "symbols"
                                    (list :array (list "trace-target")))))))
      (assert-true (lookup un "result") "untrace returned error: ~S" un))))
(format t "  trace/untrace OK~%")

;;; ----------------------------------------------------------------------------
;;; #163: time-eval reports real_ms / bytes_consed for a small form.

(format t "Test: time-eval returns timing fields~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (do-rpc sock "time-eval"
                          (list (cons "form" "(loop for i from 0 below 1000 sum i)"))))
           (result (lookup resp "result"))
           (timing (lookup result "timing")))
      (assert-true result "no result: ~S" resp)
      (assert-true (integerp (lookup timing "real_ms"))
                   "real_ms not integer")
      (assert-true (integerp (lookup timing "bytes_consed"))
                   "bytes_consed not integer"))))
(format t "  time-eval OK~%")

;;; ----------------------------------------------------------------------------
;;; #164: profile-eval returns a raw report.

(format t "Test: profile-eval returns a profile entry~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (do-rpc sock "profile-eval"
                          (list (cons "form"
                                      "(loop for i from 0 below 10000 sum i)")
                                (cons "top" 5))))
           (result (lookup resp "result"))
           (entries (array-items (lookup (lookup result "profile") "entries"))))
      (assert-true result "no result: ~S" resp)
      (assert-true entries "no profile entries"))))
(format t "  profile-eval OK~%")

(format t "~%REPL trace tests PASSED!~%")
(sb-ext:exit :code 0)
