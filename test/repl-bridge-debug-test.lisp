;;;; test/repl-bridge-debug-test.lisp - interactive debug session.
;;;;
;;;; Covers BRIDGE_V2 #111 (eval --debug enters debugger), #112
;;;; (debug-eval-in-frame), #113 (debug-invoke-restart with args),
;;;; #114 (debug-abort).

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

(defun array-items (array)
  (when (and (consp array) (eq (car array) :array))
    (cadr array)))

(defun frame-index-named (debugger-event name)
  (let* ((condition (lookup debugger-event "condition"))
         (frames (array-items (lookup condition "backtrace"))))
    (loop for frame in frames
          for obj = (and (consp frame) (eq (car frame) :object) frame)
          for fname = (lookup obj "name")
          for index = (lookup obj "i")
          when (and (stringp fname)
                    (search name fname :test #'char-equal)
                    (integerp index))
            return index)))

(defun with-daemon (fn)
  (let* ((sock (format nil "/tmp/clpm-rb-debug-~A.sock" (random (expt 2 32))))
         (thread (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (clpm.repl-bridge:start-server :socket-path sock)
                      (error (c) (format *error-output* "daemon: ~A~%" c))))
                  :name "test-bridge-debug")))
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

(defun send-on-thread (conn message)
  "Send MESSAGE on CONN's stream from a background thread (so the caller
can keep reading event frames)."
  (clpm.repl-bridge.compat:make-thread
   (lambda ()
     (handler-case
         (clpm.repl-bridge::%write-line-json
          (clpm.repl-bridge::connection-stream conn)
          message)
       (error () nil)))
   :name "test-debug-replier"))

(defun send-raw (conn message)
  (clpm.repl-bridge::%write-line-json
   (clpm.repl-bridge::connection-stream conn)
   message))

(defun read-raw-frame (conn)
  (let ((line (read-line (clpm.repl-bridge::connection-stream conn) nil nil)))
    (and line (clpm.io.json:read-json-from-string line))))

;;; ----------------------------------------------------------------------------
;;; #111+#113: eval --debug enters debugger; USE-VALUE 0 produces value 0.

(format t "Test: USE-VALUE 0 recovers from (/ 1 0)~%")
(with-daemon
  (lambda (sock)
    (let* ((conn (clpm.repl-bridge:open-connection sock))
           (eval-id 11)
           (entered nil))
      (unwind-protect
           (let ((resp
                   (clpm.repl-bridge:send-on-connection
                    conn "eval"
                    :id eval-id
                    :params (list :object
                                  (list (cons "form"
                                              "(restart-case (/ 1 0)
                                                 (use-value (v) v))")
                                        (cons "debug" t)))
                    :on-event
                    (lambda (frame)
                      (when (string= "debugger-entered" (lookup frame "event"))
                        (setf entered t)
                        (send-on-thread
                         conn
                         (clpm.repl-bridge::%json-object
                          "id" eval-id
                          "method" "debug-invoke-restart"
                          "params" (clpm.repl-bridge::%json-object
                                    "name" "use-value"
                                    "args" (list :array (list "0"))))))
                      nil))))
             (assert-true entered "debugger-entered event was never received")
             (let ((result (lookup resp "result")))
               (assert-true result "no terminal result: ~S" resp)
               (assert-equal-string "0" (lookup result "value"))))
        (clpm.repl-bridge:close-connection conn)))))
(format t "  USE-VALUE recovery OK~%")

;;; ----------------------------------------------------------------------------
;;; #114: debug-abort produces a terminal error frame.

(format t "Test: debug-abort lets the condition unwind~%")
(with-daemon
  (lambda (sock)
    (let* ((conn (clpm.repl-bridge:open-connection sock))
           (eval-id 14))
      (unwind-protect
           (let ((resp
                   (clpm.repl-bridge:send-on-connection
                    conn "eval"
                    :id eval-id
                    :params (list :object
                                  (list (cons "form" "(error \"abort me\")")
                                        (cons "debug" t)))
                    :on-event
                    (lambda (frame)
                      (when (string= "debugger-entered" (lookup frame "event"))
                        (send-on-thread
                         conn
                         (clpm.repl-bridge::%json-object
                          "id" eval-id
                          "method" "debug-abort")))
                      nil))))
             (let ((err (lookup resp "error")))
               (assert-true err "expected error frame, got ~S" resp)
               (assert-equal-string "eval-error" (lookup err "code"))))
        (clpm.repl-bridge:close-connection conn)))))
(format t "  debug-abort OK~%")

;;; ----------------------------------------------------------------------------
;;; #112: debug-eval-in-frame uses the frame's lexical environment.

(format t "Test: debug-eval-in-frame sees live frame variables~%")
(with-daemon
  (lambda (sock)
    (let* ((conn (clpm.repl-bridge:open-connection sock))
           (eval-id 12)
           (frame-result nil))
      (unwind-protect
           (let ((resp
                   (clpm.repl-bridge:send-on-connection
                    conn "eval"
                    :id eval-id
                    :params (list :object
                                  (list (cons "form"
                                              "(progn
                                                 (declaim (optimize (debug 3)
                                                                    (safety 3)
                                                                    (speed 0)))
                                                 (defun rb-debug-frame-target (x)
                                                   (error \"x=~A\" x))
                                                 (rb-debug-frame-target 7))")
                                        (cons "debug" t)))
                    :on-event
                    (lambda (frame)
                      (cond
                        ((string= "debugger-entered" (lookup frame "event"))
                         (let ((frame-index
                                 (frame-index-named frame
                                                    "RB-DEBUG-FRAME-TARGET")))
                           (assert-true frame-index
                                        "could not find user frame in ~S"
                                        frame)
                           (send-on-thread
                            conn
                            (clpm.repl-bridge::%json-object
                             "id" eval-id
                             "method" "debug-eval-in-frame"
                             "params" (clpm.repl-bridge::%json-object
                                       "frame" frame-index
                                       "form" "(* x 2)")))))
                        ((string= "frame-eval-result" (lookup frame "event"))
                         (setf frame-result frame)
                         (send-on-thread
                          conn
                          (clpm.repl-bridge::%json-object
                           "id" eval-id
                           "method" "debug-abort"))))
                      nil))))
             (declare (ignore resp))
             (assert-true frame-result
                          "never got a frame-eval-result event")
             (assert-equal-string "14" (lookup frame-result "value")))
        (clpm.repl-bridge:close-connection conn)))))
(format t "  debug-eval-in-frame OK~%")

;;; ----------------------------------------------------------------------------
;;; Debug sessions are server-owned, not connection-owned.

(format t "Test: debug session survives discovery connection close~%")
(with-daemon
  (lambda (sock)
    (let* ((conn (clpm.repl-bridge:open-connection sock))
           (eval-id 15))
      (unwind-protect
           (progn
             (send-raw
              conn
              (clpm.repl-bridge::%json-object
               "id" eval-id
               "method" "eval"
               "params" (clpm.repl-bridge::%json-object
                         "form" "(progn
                                    (declaim (optimize (debug 3)
                                                       (safety 3)
                                                       (speed 0)))
                                    (defun rb-debug-owned-session-target (x)
                                      (error \"x=~A\" x))
                                    (rb-debug-owned-session-target 7))"
                         "debug" t)))
             (let* ((entered (read-raw-frame conn))
                    (session (lookup entered "session"))
                    (frame-index
                      (and (string= "debugger-entered"
                                    (lookup entered "event"))
                           (frame-index-named
                            entered "RB-DEBUG-OWNED-SESSION-TARGET"))))
               (unless (integerp session)
                 (send-raw
                  conn
                  (clpm.repl-bridge::%json-object
                   "id" eval-id
                   "method" "debug-abort"))
                 (fail "debugger event missing server session id: ~S"
                       entered))
               (assert-true frame-index
                            "could not find user frame in ~S"
                            entered)
               (clpm.repl-bridge:close-connection conn)
               (setf conn nil)
               (let* ((frame-resp
                        (clpm.repl-bridge:send-request
                         sock "debug-eval-in-frame"
                         :params (clpm.repl-bridge::%json-object
                                  "session" session
                                  "frame" frame-index
                                  "form" "(* x 2)")))
                      (frame-result (lookup frame-resp "result")))
                 (assert-true frame-result
                              "expected frame eval result, got ~S"
                              frame-resp)
                 (assert-equal-string "14"
                                      (lookup frame-result "value")))
               (let* ((abort-resp
                        (clpm.repl-bridge:send-request
                         sock "debug-abort"
                         :params (clpm.repl-bridge::%json-object
                                  "session" session)))
                      (abort-result (lookup abort-resp "result")))
                 (assert-true abort-result
                              "expected abort result, got ~S"
                              abort-resp)
                 (assert-equal-string "aborted"
                                      (lookup abort-result "outcome")))))
        (when conn
          (clpm.repl-bridge:close-connection conn))))))
(format t "  server-owned debug session OK~%")

(format t "Test: fresh restart resumes original continuation~%")
(with-daemon
  (lambda (sock)
    (let* ((conn (clpm.repl-bridge:open-connection sock))
           (eval-id 16))
      (unwind-protect
           (progn
             (send-raw
              conn
              (clpm.repl-bridge::%json-object
               "id" eval-id
               "method" "eval"
               "params" (clpm.repl-bridge::%json-object
                         "form" "(restart-case (/ 1 0)
                                   (use-value (v) v))"
                         "debug" t)))
             (let* ((entered (read-raw-frame conn))
                    (session (lookup entered "session")))
               (assert-true (integerp session)
                            "debugger event missing session id: ~S"
                            entered)
               (let* ((restart-resp
                        (clpm.repl-bridge:send-request
                         sock "debug-invoke-restart"
                         :params (clpm.repl-bridge::%json-object
                                  "session" session
                                  "name" "use-value"
                                  "args" (list :array (list "42")))))
                      (restart-result (lookup restart-resp "result")))
                 (assert-true restart-result
                              "expected restart result, got ~S"
                              restart-resp)
                 (assert-equal-string "restart-invoked"
                                      (lookup restart-result "outcome")))
               (let* ((terminal
                         (loop for frame = (read-raw-frame conn)
                               do (assert-true frame
                                               "connection closed before terminal result")
                               when (lookup frame "result")
                                 return frame))
                      (result (lookup terminal "result")))
                 (assert-true result
                              "expected resumed eval result, got ~S"
                              terminal)
                 (assert-equal-string "42"
                                      (lookup result "value")))))
        (clpm.repl-bridge:close-connection conn)))))
(format t "  fresh restart OK~%")

(format t "Test: bad restart arg keeps debug session live~%")
(with-daemon
  (lambda (sock)
    (let* ((conn (clpm.repl-bridge:open-connection sock))
           (eval-id 17))
      (unwind-protect
           (progn
             (send-raw
              conn
              (clpm.repl-bridge::%json-object
               "id" eval-id
               "method" "eval"
               "params" (clpm.repl-bridge::%json-object
                         "form" "(restart-case (/ 1 0)
                                   (use-value (v) v))"
                         "debug" t)))
             (let* ((entered (read-raw-frame conn))
                    (session (lookup entered "session")))
               (assert-true (integerp session)
                            "debugger event missing session id: ~S"
                            entered)
               (let* ((bad-resp
                        (clpm.repl-bridge:send-request
                         sock "debug-invoke-restart"
                         :params (clpm.repl-bridge::%json-object
                                  "session" session
                                  "name" "use-value"
                                  "args" (list :array (list "(")))))
                      (bad-error (lookup bad-resp "error")))
                 (assert-true bad-error
                              "expected bad restart arg error, got ~S"
                              bad-resp))
               (let* ((sessions-resp
                        (clpm.repl-bridge:send-request
                         sock "list-debug-sessions"))
                      (sessions (array-items
                                 (lookup (lookup sessions-resp "result")
                                         "sessions"))))
                 (assert-true
                  (find session sessions
                        :key (lambda (entry) (lookup entry "session"))
                        :test #'eql)
                  "session vanished after bad restart arg: ~S"
                  sessions-resp))
               (let* ((restart-resp
                        (clpm.repl-bridge:send-request
                         sock "debug-invoke-restart"
                         :params (clpm.repl-bridge::%json-object
                                  "session" session
                                  "name" "use-value"
                                  "args" (list :array (list "42")))))
                      (restart-result (lookup restart-resp "result")))
                 (assert-true restart-result
                              "expected restart result, got ~S"
                              restart-resp)
                 (assert-equal-string "restart-invoked"
                                      (lookup restart-result "outcome")))
               (let* ((terminal
                         (loop for frame = (read-raw-frame conn)
                               do (assert-true frame
                                               "connection closed before terminal result")
                               when (lookup frame "result")
                                 return frame))
                      (result (lookup terminal "result")))
                 (assert-true result
                              "expected resumed eval result, got ~S"
                              terminal)
                 (assert-equal-string "42"
                                      (lookup result "value")))))
        (clpm.repl-bridge:close-connection conn)))))
(format t "  bad restart arg recovery OK~%")

(format t "Test: fresh debug action diagnoses ambiguous sessions~%")
(with-daemon
  (lambda (sock)
    (let ((conn-a (clpm.repl-bridge:open-connection sock))
          (conn-b (clpm.repl-bridge:open-connection sock)))
      (unwind-protect
           (progn
             (send-raw
              conn-a
              (clpm.repl-bridge::%json-object
               "id" 18
               "method" "eval"
               "params" (clpm.repl-bridge::%json-object
                         "form" "(error \"debug A\")"
                         "worker" "a"
                         "debug" t)))
             (send-raw
              conn-b
              (clpm.repl-bridge::%json-object
               "id" 19
               "method" "eval"
               "params" (clpm.repl-bridge::%json-object
                         "form" "(error \"debug B\")"
                         "worker" "b"
                         "debug" t)))
             (let* ((entered-a (read-raw-frame conn-a))
                    (entered-b (read-raw-frame conn-b))
                    (session-a (lookup entered-a "session"))
                    (session-b (lookup entered-b "session")))
               (assert-true (integerp session-a)
                            "first debugger event missing session: ~S"
                            entered-a)
               (assert-true (integerp session-b)
                            "second debugger event missing session: ~S"
                            entered-b)
               (let* ((ambiguous
                        (clpm.repl-bridge:send-request sock "debug-abort"))
                      (err (lookup ambiguous "error"))
                      (message (lookup err "message")))
                 (assert-true (and (stringp message)
                                   (search "multiple active debug sessions"
                                           message))
                              "expected ambiguous-session error, got ~S"
                              ambiguous))
               (clpm.repl-bridge:send-request
                sock "debug-abort"
                :params (clpm.repl-bridge::%json-object
                         "session" session-a))
               (clpm.repl-bridge:send-request
                sock "debug-abort"
                :params (clpm.repl-bridge::%json-object
                         "session" session-b))))
        (clpm.repl-bridge:close-connection conn-a)
        (clpm.repl-bridge:close-connection conn-b)))))
(format t "  ambiguous debug action OK~%")

(format t "Test: shutdown resolves kept debug sessions~%")
(let* ((sock (format nil "/tmp/clpm-rb-debug-shutdown-~A.sock"
                     (random (expt 2 32))))
       (thread (sb-thread:make-thread
                (lambda ()
                  (handler-case
                      (clpm.repl-bridge:start-server :socket-path sock)
                    (error (c)
                      (format *error-output* "daemon: ~A~%" c))))
                :name "test-bridge-debug-shutdown")))
  (unwind-protect
       (progn
         (loop for i from 0 below 50
               while (not (probe-file sock))
               do (sleep 0.05))
         (assert-true (probe-file sock) "daemon never started")
         (let ((conn (clpm.repl-bridge:open-connection sock)))
           (unwind-protect
                (progn
                  (send-raw
                   conn
                   (clpm.repl-bridge::%json-object
                    "id" 18
                    "method" "eval"
                    "params" (clpm.repl-bridge::%json-object
                              "form" "(error \"shutdown should abort me\")"
                              "debug" t)))
                  (let* ((entered (read-raw-frame conn))
                         (session (lookup entered "session")))
                    (assert-true (integerp session)
                                 "debugger event missing session id: ~S"
                                 entered)
                    (clpm.repl-bridge:send-request sock "shutdown")
                    (loop for i from 0 below 60
                          while (sb-thread:thread-alive-p thread)
                          do (sleep 0.05))
                    (assert-true (not (sb-thread:thread-alive-p thread))
                                 "server did not stop with active debug session")))
             (clpm.repl-bridge:close-connection conn))))
    (when (sb-thread:thread-alive-p thread)
      (ignore-errors (sb-thread:terminate-thread thread)))
    (ignore-errors (delete-file sock))))
(format t "  shutdown with kept debug session OK~%")

(format t "~%REPL-bridge debug tests PASSED!~%")
(sb-ext:exit :code 0)
