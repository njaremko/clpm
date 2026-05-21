;;;; test/repl-stream-test.lisp - streamed stdout/stderr events
;;;;
;;;; Acceptance for BRIDGE_V2 #103: a form that prints multiple lines with
;;;; a sleep between each yields N stdout events before the terminal frame.

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
  (let* ((sock (format nil "/tmp/clpm-rb-stream-~A.sock" (random (expt 2 32))))
         (thread (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (clpm.repl:start-server :socket-path sock)
                      (error (c) (format *error-output* "daemon: ~A~%" c))))
                  :name "test-bridge-stream")))
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

;;; ----------------------------------------------------------------------------
;;; Print N lines with a brief sleep between each; collect stdout events.

(format t "Test: --stream emits per-line stdout events as the eval runs~%")
(with-daemon
  (lambda (sock)
    (let* ((events (make-array 0 :fill-pointer 0 :adjustable t))
           (params (list :object
                         (list (cons "form"
                                     "(progn
                                        (dotimes (i 5)
                                          (format t \"line ~D~%\" i)
                                          (force-output)
                                          (sleep 0.02))
                                        :done)")
                               (cons "stream" t))))
           (resp (clpm.repl:send-request
                  sock "eval"
                  :params params
                  :on-event
                  (lambda (frame)
                    (vector-push-extend frame events)
                    nil)))
           (result (lookup resp "result")))
      (assert-true result "no terminal result frame: ~S" resp)
      ;; The worker prints 5 lines; we expect at least 5 stdout events.
      (let ((stdouts (loop for f across events
                           when (string= "stdout" (lookup f "event"))
                           collect f)))
        (assert-true (>= (length stdouts) 5)
                     "expected >=5 stdout events, got ~D: ~S"
                     (length stdouts) (map 'list #'identity events)))
      ;; The terminal frame should still carry the *full* captured output.
      (let ((output (lookup result "output")))
        (assert-true (and (stringp output)
                          (= 5 (count #\Newline output)))
                     "expected 5 newlines in captured output, got ~S" output)))))
(format t "  streaming OK~%")

;;; ----------------------------------------------------------------------------
;;; Streaming and terminal capture are observations of the same bounded prefix.

(format t "Test: --stream events obey the output cap~%")
(with-daemon
  (lambda (sock)
    (let* ((limit clpm.repl::+max-output-bytes+)
           (chunks '())
           (resp (clpm.repl:send-request
                  sock "eval"
                  :params (list :object
                                (list
                                 (cons "form"
                                       (format nil
                                               "(progn
                                                  (write-string
                                                   (make-string ~D
                                                                :initial-element #\\x))
                                                  :done)"
                                               (+ limit 128)))
                                 (cons "stream" t)))
                  :on-event
                  (lambda (frame)
                    (when (string= "stdout" (lookup frame "event"))
                      (push (lookup frame "data") chunks))
                    nil)))
           (result (lookup resp "result"))
           (streamed (apply #'concatenate 'string (nreverse chunks)))
           (output (lookup result "output")))
      (assert-true result "no terminal result frame: ~S" resp)
      (assert-true (<= (length streamed) limit)
                   "streamed ~D bytes, limit is ~D"
                   (length streamed) limit)
      (assert-true (string= streamed output)
                   "streamed output and terminal output diverged: ~D vs ~D"
                   (length streamed) (length output))
      (assert-true (lookup result "truncated")
                   "expected truncated flag in ~S" result))))
(format t "  streaming output cap OK~%")

;;; ----------------------------------------------------------------------------
;;; Without --stream, no events are emitted (v1 contract preserved).

(format t "Test: without --stream, exactly one terminal frame arrives~%")
(with-daemon
  (lambda (sock)
    (let* ((events (make-array 0 :fill-pointer 0 :adjustable t))
           (resp (clpm.repl:send-request
                  sock "eval"
                  :params (list :object
                                (list (cons "form"
                                            "(progn (format t \"hi~%\") :ok)")))
                  :on-event
                  (lambda (frame)
                    (vector-push-extend frame events)
                    nil))))
      (assert-true (lookup resp "result") "no terminal frame: ~S" resp)
      (assert-true (zerop (length events))
                   "expected zero events, got ~D" (length events)))))
(format t "  v1 contract preserved (no events) OK~%")

(format t "~%REPL streaming tests PASSED!~%")
(sb-ext:exit :code 0)
