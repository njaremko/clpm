;;;; test/repl-bridge-interrupt-test.lisp - interrupt mid-eval

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

(defun assert-string= (expected actual)
  (unless (and (stringp actual) (string= expected actual))
    (fail "expected ~S, got ~S" expected actual)))

(defun lookup (object key)
  (when (and (consp object) (eq (car object) :object))
    (let ((cell (assoc key (cadr object) :test #'string=)))
      (and cell (cdr cell)))))

(defun with-daemon (fn)
  (let* ((tmp (format nil "/tmp/clpm-bridge-int-test-~A.sock"
                      (random (expt 2 32))))
         (server-thread
           (sb-thread:make-thread
            (lambda ()
              (handler-case
                  (clpm.repl-bridge:start-server :socket-path tmp)
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
      (handler-case (clpm.repl-bridge:send-request tmp "shutdown") (error () nil))
      (loop for i from 0 below 30
            while (sb-thread:thread-alive-p server-thread)
            do (sleep 0.05))
      (when (sb-thread:thread-alive-p server-thread)
        (ignore-errors (sb-thread:terminate-thread server-thread)))
      (ignore-errors (delete-file tmp)))))

(format t "Test: explicit interrupt unblocks the worker~%")
(with-daemon
    (lambda (sock)
      ;; Launch a long eval in a separate thread.
      (let* ((response-box (sb-concurrency:make-mailbox))
             (eval-thread
               (sb-thread:make-thread
                (lambda ()
                  (let ((r (clpm.repl-bridge:send-request
                            sock "eval"
                            :params (list :object
                                          (list (cons "form" "(loop)"))))))
                    (sb-concurrency:send-message response-box r)))
                :name "eval-driver")))
        ;; Wait a moment, send interrupt, then expect the eval to return.
        (sleep 0.2)
        (let* ((int-resp (clpm.repl-bridge:send-request sock "interrupt")))
          (assert-true (lookup int-resp "result")
                       "interrupt response should be success, got ~S" int-resp))
        (let ((eval-resp
                (sb-concurrency:receive-message-no-hang response-box))
              (deadline (+ (get-internal-real-time)
                           (* 5 internal-time-units-per-second))))
          (loop while (and (null eval-resp)
                           (< (get-internal-real-time) deadline))
                do (sleep 0.05)
                   (setf eval-resp
                         (sb-concurrency:receive-message-no-hang response-box)))
          (assert-true eval-resp "eval thread didn't return within 5s")
          (let ((err (lookup eval-resp "error")))
            (assert-true err "expected error after interrupt, got ~S" eval-resp)
            (assert-string= "interrupted" (lookup err "code"))))
        (when (sb-thread:thread-alive-p eval-thread)
          (ignore-errors (sb-thread:terminate-thread eval-thread))))
      ;; Daemon is still responsive after interrupt.
      (let* ((r (clpm.repl-bridge:send-request sock "ping")))
        (assert-true (lookup r "result")
                     "daemon should still answer ping after interrupt, got ~S"
                     r))))
(format t "  OK~%")

(format t "~%REPL-bridge interrupt tests PASSED!~%")
(sb-ext:exit :code 0)
