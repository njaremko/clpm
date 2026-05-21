;;;; test/repl-event-log-test.lisp - .clpm/repl.log JSON lines

(require :asdf)
(require :sb-posix)

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

(defun read-log-events (path)
  "Return a list of parsed JSON event objects from PATH."
  (let ((raw (uiop:read-file-string path)))
    (loop for start = 0 then (1+ end)
          for end = (position #\Newline raw :start start)
          while end
          when (> end start)
            collect (clpm.io.json:read-json-from-string (subseq raw start end)))))

(defun event-field (obj field)
  (when (and (consp obj) (eq (car obj) :object))
    (cdr (assoc field (cadr obj) :test #'string=))))

(defun event-type (obj)
  (event-field obj "event"))

(defun make-short-temp-dir ()
  "Create a short temp directory so Unix-domain socket paths stay portable."
  (let* ((template (namestring
                    (merge-pathnames "rbXXXXXX" (uiop:temporary-directory))))
         (dir (sb-posix:mkdtemp template)))
    (uiop:ensure-directory-pathname dir)))

(defmacro with-short-temp-dir ((var) &body body)
  `(let ((,var (make-short-temp-dir)))
     (unwind-protect
          (progn ,@body)
       (ignore-errors
         (uiop:delete-directory-tree ,var :validate t)))))

(with-short-temp-dir (tmp)
  (let* ((sock (namestring (merge-pathnames "rb.sock" tmp)))
         (log (namestring (merge-pathnames "rb.log" tmp))))

    (format t "Test: daemon writes structured events~%")
    (let ((srv (sb-thread:make-thread
                (lambda ()
                  (clpm.repl:start-server :socket-path sock
                                                 :log-path log))
                :name "test-serve")))
      ;; Wait for the daemon to start.
      (loop for i from 0 below 50
            while (not (probe-file sock))
            do (sleep 0.1))
      (assert-true (probe-file sock) "daemon never came up")

      ;; Make a few requests.
      (clpm.repl:send-request sock "ping")
      (clpm.repl:send-request sock "eval"
                                     :params (list :object (list (cons "form" "(+ 1 2)"))))
      ;; Trigger a request that errors out at the protocol layer.
      (clpm.repl:send-request sock "no-such-method")

      ;; Shutdown.
      (clpm.repl:send-request sock "shutdown")
      (loop for i from 0 below 50
            while (sb-thread:thread-alive-p srv)
            do (sleep 0.1))
      (when (sb-thread:thread-alive-p srv)
        (ignore-errors (sb-thread:terminate-thread srv))))

    (format t "  daemon stopped, inspecting log~%")
    (let ((events (read-log-events log)))
      (assert-true (plusp (length events))
                   "log is empty: ~A" log)
      ;; First event should be `start' with the pid+transport fields.
      (let ((first (first events)))
        (assert-true (string= "start" (event-type first))
                     "first event is not `start': ~S" first)
        (assert-true (integerp (event-field first "pid"))
                     "start event has no pid")
        (assert-true (stringp (event-field first "transport"))
                     "start event has no transport")
        (assert-true (stringp (event-field first "path"))
                     "start event has no path"))
      ;; jq-able: each event-line is a complete JSON object with a `ts' and
      ;; `event' field.
      (dolist (e events)
        (assert-true (stringp (event-field e "ts"))
                     "event missing `ts': ~S" e)
        (assert-true (stringp (event-field e "event"))
                     "event missing `event': ~S" e))
      ;; We expect at least: start, accept (>=3), request (>=3), response (>=3),
      ;; shutdown, stop.
      (let ((types (mapcar #'event-type events)))
        (assert-true (member "start" types :test #'string=)
                     "no start event")
        (assert-true (member "accept" types :test #'string=)
                     "no accept event")
        (assert-true (member "request" types :test #'string=)
                     "no request event")
        (assert-true (member "response" types :test #'string=)
                     "no response event")
        (assert-true (member "shutdown" types :test #'string=)
                     "no shutdown event")
        (assert-true (member "stop" types :test #'string=)
                     "no stop event"))
      ;; Find the response for the unknown-method request: it should carry an
      ;; `error` field with the protocol-error code.
      (let ((bad-resp
              (find-if (lambda (e)
                         (and (string= "response" (event-type e))
                              (let ((m (event-field e "method")))
                                (and (stringp m)
                                     (string= m "no-such-method")))
                              (event-field e "error")))
                       events)))
        (assert-true bad-resp "no response event for unknown-method request")
        (assert-true (string= "protocol-error" (event-field bad-resp "error"))
                     "expected protocol-error code, got ~S"
                     (event-field bad-resp "error"))))
    (format t "  events OK (~D entries)~%"
            (length (read-log-events log)))))

;;; --------------------------------------------------------------------------
;;; Rotation: when an existing log file exceeds the cap, the NEXT write
;;; rotates it to PATH.1 and starts a fresh file at PATH.
;;; --------------------------------------------------------------------------

(with-short-temp-dir (tmp)
  (let* ((log (namestring (merge-pathnames "rb.log" tmp)))
         (rotated (concatenate 'string log ".1")))
    (format t "Test: log rotation~%")
    ;; Pre-populate a log file just over the 10 MB cap.
    (with-open-file (s log :direction :output :if-exists :supersede
                           :if-does-not-exist :create
                           :external-format :utf-8)
      ;; Write a header so the rotated file is recognizable.
      (write-string (make-string (+ (* 10 1024 1024) 1024) :initial-element #\X) s))
    (let* ((open-log (funcall (find-symbol "%OPEN-EVENT-LOG"
                                            (find-package "CLPM.REPL"))
                              log)))
      (assert-true open-log "open-event-log returned NIL on a valid path")
      ;; Trigger a write; the post-write check should rotate.
      (funcall (find-symbol "%LOG-EVENT" (find-package "CLPM.REPL"))
               open-log "trigger")
      ;; Close so the OS commits state.
      (funcall (find-symbol "%CLOSE-EVENT-LOG" (find-package "CLPM.REPL"))
               open-log))
    (assert-true (probe-file rotated) "rotated file ~A is missing" rotated)
    (assert-true (probe-file log) "post-rotation log ~A is missing" log)
    ;; Fresh log should be much smaller than 10 MB.
    (let ((len (with-open-file (s log :direction :input) (file-length s))))
      (assert-true (< len (* 1024 1024))
                   "post-rotation log too large: ~D bytes" len))
    (format t "  rotation OK (rotated=~A bytes, fresh=~A bytes)~%"
            (with-open-file (s rotated :direction :input) (file-length s))
            (with-open-file (s log :direction :input) (file-length s)))))

(format t "~%REPL event-log tests PASSED!~%")
(sb-ext:exit :code 0)
