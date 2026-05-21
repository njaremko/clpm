;;;; test/repl-caps-test.lisp - output and request size caps

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
  (let* ((tmp (format nil "/tmp/clpm-bridge-caps-~A.sock" (random (expt 2 32))))
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
           (funcall fn tmp))
      (handler-case (clpm.repl:send-request tmp "shutdown") (error () nil))
      (loop for i from 0 below 30
            while (sb-thread:thread-alive-p server-thread)
            do (sleep 0.05))
      (when (sb-thread:thread-alive-p server-thread)
        (ignore-errors (sb-thread:terminate-thread server-thread)))
      (ignore-errors (delete-file tmp)))))

(format t "Test: output truncation after 1 MB~%")
(with-daemon
    (lambda (sock)
      ;; Print 2 MB of `x`. Should be truncated to 1 MB.
      (let* ((resp (clpm.repl:send-request
                    sock "eval"
                    :params (list :object
                                  (list (cons "form"
                                              "(progn (dotimes (i 2000000) (write-char #\\x)) :done)")))))
             (result (lookup resp "result"))
             (out (and result (lookup result "output"))))
        (assert-true result "expected result, got ~S" resp)
        (assert-true (stringp out) "expected output string")
        (assert-true (<= (length out) (* 1024 1024))
                     "output should be capped at 1 MB, got ~D" (length out))
        (assert-true (eq t (lookup result "truncated"))
                     "expected truncated flag set, result=~S" result))))
(format t "  OK~%")

(format t "Test: oversized request rejected with protocol-error~%")
(with-daemon
    (lambda (sock)
      ;; Connect raw and send a line longer than 64 KB.
      (let* ((s (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
        (sb-bsd-sockets:socket-connect s sock)
        (let ((stream (sb-bsd-sockets:socket-make-stream
                       s :input t :output t :buffering :line
                         :external-format :utf-8
                         :element-type 'character)))
          ;; Write a 70 KB line of garbage.
          (write-char #\{ stream)
          (dotimes (i 70000) (write-char #\x stream))
          (write-char #\Newline stream)
          (force-output stream)
          (let* ((line (read-line stream nil nil))
                 (resp (and line (clpm.io.json:read-json-from-string line)))
                 (err (lookup resp "error")))
            (assert-true err "expected error response, got ~S" resp)
            (assert-string= "protocol-error" (lookup err "code"))
            (assert-true (search "exceeds" (lookup err "message"))
                         "expected size mention in message: ~S"
                         (lookup err "message"))))
        (ignore-errors (sb-bsd-sockets:socket-close s)))
      ;; Daemon still responsive.
      (let ((r (clpm.repl:send-request sock "ping")))
        (assert-true (lookup r "result")
                     "daemon should still respond after rejecting oversized req"))))
(format t "  OK~%")

(format t "~%REPL caps tests PASSED!~%")
(sb-ext:exit :code 0)
