;;;; test/repl-bridge-server-test.lisp - in-process daemon round-trip

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
  (unless (string= expected actual)
    (fail "expected ~S, got ~S" expected actual)))

(defun lookup (object key)
  (when (and (consp object) (eq (car object) :object))
    (let ((cell (assoc key (cadr object) :test #'string=)))
      (and cell (cdr cell)))))

;;; ---- helper: start the daemon on a tmp socket -----------------------------

(defun with-daemon (fn)
  "Spin a daemon on a tmp socket in a thread; call FN with the path; tear down."
  (let* ((tmp (format nil "/tmp/clpm-bridge-test-~A.sock"
                      (random (expt 2 32))))
         (server-thread
           (sb-thread:make-thread
            (lambda ()
              (handler-case
                  (clpm.repl-bridge:start-server :socket-path tmp)
                (error (c)
                  (format *error-output* "daemon: ~A~%" c))))
            :name "test-bridge-daemon")))
    (unwind-protect
         (progn
           ;; Wait for the socket to appear (daemon binds first).
           (loop for i from 0 below 50
                 while (not (probe-file tmp))
                 do (sleep 0.05))
           (unless (probe-file tmp)
             (fail "daemon failed to bind ~A within 2.5s" tmp))
           (funcall fn tmp))
      (handler-case
          (clpm.repl-bridge:send-request tmp "shutdown")
        (error () nil))
      ;; Give the daemon a moment to wind down.
      (loop for i from 0 below 20
            while (sb-thread:thread-alive-p server-thread)
            do (sleep 0.05))
      (when (sb-thread:thread-alive-p server-thread)
        (ignore-errors (sb-thread:terminate-thread server-thread)))
      (ignore-errors (delete-file tmp)))))

;;; ---- tests ---------------------------------------------------------------

(format t "Test: socket is mode 0600 after bind~%")
(with-daemon
    (lambda (sock-path)
      ;; sb-posix:stat returns an sb-posix:stat struct.
      (let* ((mode (sb-posix:stat-mode (sb-posix:stat sock-path)))
             ;; Mask off the file-type bits to compare permission bits only.
             (perms (logand mode #o777)))
        (assert-eql #o600 perms))))
(format t "  OK~%")

(format t "Test: ping round-trip~%")
(with-daemon
    (lambda (sock-path)
      (let* ((resp (clpm.repl-bridge:send-request sock-path "ping" :id 1))
             (result (lookup resp "result")))
        (assert-true (consp resp) "expected JSON object response, got ~S" resp)
        (assert-eql 1 (lookup resp "id"))
        (assert-true result "missing `result` in ~S" resp)
        (assert-true (integerp (lookup result "pid")) "expected pid")
        (assert-true (stringp (lookup result "lisp")) "expected lisp string"))))
(format t "  OK~%")

(format t "Test: malformed JSON returns protocol-error~%")
(with-daemon
    (lambda (sock-path)
      ;; Connect raw and send garbage.
      (let* ((s (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
        (sb-bsd-sockets:socket-connect s sock-path)
        (let ((stream (sb-bsd-sockets:socket-make-stream
                       s :input t :output t :buffering :line
                         :external-format :utf-8
                         :element-type 'character)))
          (write-string "not json at all" stream)
          (write-char #\Newline stream)
          (force-output stream)
          (let* ((line (read-line stream nil nil))
                 (resp (and line (clpm.io.json:read-json-from-string line)))
                 (err (lookup resp "error")))
            (assert-true err "expected error response, got ~S" resp)
            (assert-string= "protocol-error" (lookup err "code"))))
        (ignore-errors (sb-bsd-sockets:socket-close s)))))
(format t "  OK~%")

(format t "Test: unknown method returns protocol-error~%")
(with-daemon
    (lambda (sock-path)
      (let* ((resp (clpm.repl-bridge:send-request sock-path "no-such-method" :id 7))
             (err (lookup resp "error")))
        (assert-true err "expected error, got ~S" resp)
        (assert-eql 7 (lookup resp "id"))
        (assert-string= "protocol-error" (lookup err "code")))))
(format t "  OK~%")

(format t "Test: connect to absent socket returns :no-daemon~%")
(let ((result (clpm.repl-bridge:send-request
               "/tmp/clpm-bridge-DOES-NOT-EXIST.sock"
               "ping"
               :connect-timeout 1)))
  (assert-eql :no-daemon result))
(format t "  OK~%")

(format t "~%REPL-bridge server tests PASSED!~%")
(sb-ext:exit :code 0)
