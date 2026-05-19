;;;; test/repl-bridge-pipeline-test.lisp - v2 protocol: persistent connection
;;;;
;;;; v2 #101: a single connection can carry multiple requests back-to-back
;;;; without reopening. Verifies the daemon's read loop survives malformed
;;;; lines and resumes service.

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

(defun lookup (object key)
  (when (and (consp object) (eq (car object) :object))
    (cdr (assoc key (cadr object) :test #'string=))))

(defun with-daemon (fn)
  (let* ((sock (format nil "/tmp/clpm-rb-pipe-~A.sock" (random (expt 2 32))))
         (thread (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (clpm.repl-bridge:start-server :socket-path sock)
                      (error (c) (format *error-output* "daemon: ~A~%" c))))
                  :name "test-bridge")))
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
;;; #101 acceptance: three ping requests on one connection.

(format t "Test: three pings on a single persistent connection~%")
(with-daemon
  (lambda (sock)
    (let ((conn (clpm.repl-bridge:open-connection sock)))
      (assert-true (not (eq conn :no-daemon)) "open-connection :no-daemon")
      (unwind-protect
           (dotimes (i 3)
             (let ((resp (clpm.repl-bridge:send-on-connection
                          conn "ping" :id (1+ i))))
               (assert-true (lookup resp "result")
                            "ping #~D did not return result: ~S" i resp)
               (assert-eql (1+ i) (lookup resp "id"))))
        (clpm.repl-bridge:close-connection conn)))))
(format t "  pipelining OK~%")

;;; ----------------------------------------------------------------------------
;;; State persists across requests on the same connection (and across the
;;; close+reopen too, since the daemon's worker is global).

(format t "Test: state persists across pipelined evals~%")
(with-daemon
  (lambda (sock)
    (let ((conn (clpm.repl-bridge:open-connection sock)))
      (unwind-protect
           (progn
             (clpm.repl-bridge:send-on-connection
              conn "eval"
              :params (list :object
                            (list (cons "form" "(defparameter *pipe-x* 41)")))
              :id 10)
             (let ((resp (clpm.repl-bridge:send-on-connection
                          conn "eval"
                          :params (list :object (list (cons "form" "(1+ *pipe-x*)")))
                          :id 11)))
               (let ((result (lookup resp "result")))
                 (assert-true (and (stringp (lookup result "value"))
                                   (string= "42" (lookup result "value")))
                              "expected 42, got ~S" result))))
        (clpm.repl-bridge:close-connection conn)))))
(format t "  state persistence OK~%")

;;; ----------------------------------------------------------------------------
;;; Malformed line: daemon writes an error frame but keeps the connection
;;; alive so the *next* request still works.

(format t "Test: malformed JSON does not close the connection~%")
(with-daemon
  (lambda (sock)
    (let ((conn (clpm.repl-bridge:open-connection sock)))
      (unwind-protect
           (let ((stream (clpm.repl-bridge::connection-stream conn)))
             ;; Send a non-JSON line directly.
             (write-string "not valid json{{" stream)
             (write-char #\Newline stream)
             (force-output stream)
             (let* ((line (read-line stream nil nil))
                    (resp (and line
                               (clpm.io.json:read-json-from-string line))))
               (assert-true (lookup resp "error")
                            "expected an error frame, got ~S" resp))
             ;; Now send a valid request: should still get serviced.
             (let ((resp (clpm.repl-bridge:send-on-connection
                          conn "ping" :id 99)))
               (assert-true (lookup resp "result")
                            "daemon broke after malformed line: ~S" resp)
               (assert-eql 99 (lookup resp "id"))))
        (clpm.repl-bridge:close-connection conn)))))
(format t "  recovery from malformed line OK~%")

(format t "~%REPL-bridge pipeline tests PASSED!~%")
(sb-ext:exit :code 0)
