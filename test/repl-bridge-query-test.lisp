;;;; test/repl-bridge-query-test.lisp - bidirectional `query' I/O.
;;;;
;;;; BRIDGE_V2 #104 acceptance: when `eval --query-interactive' reads from
;;;; *standard-input* / *query-io*, the daemon emits `event:query', blocks
;;;; the worker, and resumes the read after the client posts
;;;; `query-response' on the same id.

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
  (let* ((sock (format nil "/tmp/clpm-rb-query-~A.sock" (random (expt 2 32))))
         (thread (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (clpm.repl-bridge:start-server :socket-path sock)
                      (error (c) (format *error-output* "daemon: ~A~%" c))))
                  :name "test-bridge-query")))
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
;;; Acceptance test: an eval that does (read-line) round-trips through the
;;; client.

(format t "Test: read-line inside --query-interactive eval round-trips~%")
(with-daemon
  (lambda (sock)
    (let* ((conn (clpm.repl-bridge:open-connection sock))
           (eval-id 7))
      (assert-true (not (eq conn :no-daemon)) "open-connection :no-daemon")
      (unwind-protect
           (let ((response
                   (clpm.repl-bridge:send-on-connection
                    conn "eval"
                    :id eval-id
                    :params (list :object
                                  (list (cons "form"
                                              "(read-line *standard-input*)")
                                        (cons "query_interactive" t)))
                    :on-event
                    (lambda (frame)
                      ;; First frame should be event:query.
                      (when (string= "query" (lookup frame "event"))
                        ;; Reply on the same id with the answer.
                        (clpm.repl-bridge.compat:make-thread
                         (lambda ()
                           (clpm.repl-bridge::%write-line-json
                            (clpm.repl-bridge::connection-stream conn)
                            (clpm.repl-bridge::%json-object
                             "id" eval-id
                             "method" "query-response"
                             "params" (clpm.repl-bridge::%json-object
                                       "value" "hello there"))))
                         :name "test-query-replier"))
                      nil))))
             (let ((result (lookup response "result")))
               (assert-true result "no terminal result: ~S" response)
               (assert-equal-string "\"hello there\""
                                    (lookup result "value"))))
        (clpm.repl-bridge:close-connection conn)))))
(format t "  query-response round-trip OK~%")

;;; ----------------------------------------------------------------------------
;;; If the client sends a query-response for an id that isn't waiting, the
;;; daemon responds with protocol-error rather than silently dropping it.

(format t "Test: query-response for unknown id is rejected~%")
(with-daemon
  (lambda (sock)
    (let ((conn (clpm.repl-bridge:open-connection sock)))
      (unwind-protect
           (let ((stream (clpm.repl-bridge::connection-stream conn)))
             (clpm.repl-bridge::%write-line-json
              stream
              (clpm.repl-bridge::%json-object
               "id" 999
               "method" "query-response"
               "params" (clpm.repl-bridge::%json-object "value" "hi")))
             (let* ((line (read-line stream nil nil))
                    (frame (and line
                                (clpm.io.json:read-json-from-string line)))
                    (err (and frame (lookup frame "error"))))
               (assert-true err "expected error frame, got ~S" frame)
               (assert-equal-string "protocol-error"
                                    (lookup err "code"))))
        (clpm.repl-bridge:close-connection conn)))))
(format t "  unmatched query-response rejected OK~%")

(format t "~%REPL-bridge query tests PASSED!~%")
(sb-ext:exit :code 0)
