;;;; repl_bridge.lisp - Persistent Lisp image driven over a Unix-socket JSON-RPC
;;;;
;;;; The daemon listens on a local socket and answers one request at a time.
;;;; Each request is one line of JSON (per BRIDGE.md). Eval happens on a
;;;; dedicated worker thread so the accept loop can stay responsive to
;;;; `interrupt` even while the worker is busy.
;;;;
;;;; Public entry points:
;;;;   start-server    -- daemon, blocks until shutdown
;;;;   send-request    -- one-shot client helper

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-bsd-sockets)
  (require :sb-concurrency)
  (require :sb-posix))

(in-package #:clpm.repl-bridge)

;;; --------------------------------------------------------------------------
;;; Protocol helpers
;;; --------------------------------------------------------------------------

(defparameter +max-request-bytes+ (* 64 1024)
  "Hard cap on request-line length. Excess bytes are read-and-discarded and
the daemon replies `protocol-error'.")

(defparameter +max-output-bytes+ (* 1024 1024)
  "Hard cap on captured stdout+stderr per eval. Excess is silently dropped
and the response carries `code: output-truncated'.")

(defparameter +max-backtrace-frames+ 16
  "Number of backtrace frames included in eval-error responses.")

(defun %json-getf (object key &optional default)
  "Look up KEY (a string) in OBJECT, which must be a `(:object ((k . v)…))`
form or NIL. Returns DEFAULT if missing."
  (cond
    ((null object) default)
    ((and (consp object) (eq (car object) :object))
     (let ((cell (assoc key (cadr object) :test #'string=)))
       (if cell (cdr cell) default)))
    (t default)))

(defun %json-object (&rest pairs)
  "Build a JSON object from alternating key/value pairs."
  (list :object
        (loop for (k v) on pairs by #'cddr
              collect (cons k v))))

(defun %json-array (items)
  (list :array items))

(defun %read-request-line (stream)
  "Read one line from STREAM, enforcing the size cap. Returns the line, or
NIL on EOF. Signals `clpm-parse-error' if the line is too long."
  ;; `read-line` won't enforce a cap, and SBCL's reader will happily read
  ;; gigabytes. We read character by character so we can bail early.
  (let ((buf (make-string-output-stream))
        (count 0))
    (loop
      (let ((c (read-char stream nil nil)))
        (cond
          ((null c)
           (let ((text (get-output-stream-string buf)))
             (return (if (zerop (length text)) nil text))))
          ((char= c #\Newline)
           (return (get-output-stream-string buf)))
          (t
           (incf count)
           (when (> count +max-request-bytes+)
             ;; Drain the line so the next request starts on a fresh boundary.
             (loop for d = (read-char stream nil nil)
                   while (and d (not (char= d #\Newline))))
             (clpm.errors:signal-error 'clpm.errors:clpm-parse-error
                                       "request exceeds ~D byte cap"
                                       +max-request-bytes+))
           (write-char c buf)))))))

(defun %write-line-json (stream json)
  "Write JSON followed by a newline; force-output."
  (clpm.io.json:write-json json stream)
  (write-char #\Newline stream)
  (force-output stream))

(defun %success-response (id result)
  (%json-object "id" id "result" result))

(defun %error-response (id code message &key details)
  (let ((err (list (cons "code" code) (cons "message" message))))
    (when details
      (setf err (append err (list (cons "details" details)))))
    (%json-object "id" id "error" (list :object err))))

;;; --------------------------------------------------------------------------
;;; Output streams: bounded sink
;;; --------------------------------------------------------------------------

(defstruct bounded-sink
  "A character sink that accumulates up to +MAX-OUTPUT-BYTES+ across two
streams (stdout + stderr combined). Once the combined cap is reached, further
writes are silently dropped and `truncated?' flips to T."
  (limit +max-output-bytes+ :type fixnum)
  (used 0 :type fixnum)
  (truncated? nil :type boolean))

(defun %make-capture-stream (sink)
  "Return a gray stream proxy that writes into SINK until its cap is hit."
  ;; We avoid pulling in gray streams by using a string-output-stream wrapped
  ;; in a custom :around method. Simpler: hand out a string stream and check
  ;; lengths after eval. The check below is best-effort and lets one
  ;; format-call go over by up to one form's worth, which is fine.
  (declare (ignore sink))
  (make-string-output-stream))

(defun %capture-text (stream sink)
  "Read STREAM's accumulated string, charge it to SINK, and return the
possibly-truncated text."
  (let ((text (get-output-stream-string stream)))
    (cond
      ((bounded-sink-truncated? sink) "")
      ((>= (+ (bounded-sink-used sink) (length text))
           (bounded-sink-limit sink))
       (setf (bounded-sink-truncated? sink) t)
       (let ((remaining (max 0 (- (bounded-sink-limit sink)
                                  (bounded-sink-used sink)))))
         (setf (bounded-sink-used sink) (bounded-sink-limit sink))
         (subseq text 0 (min (length text) remaining))))
      (t
       (incf (bounded-sink-used sink) (length text))
       text))))

;;; --------------------------------------------------------------------------
;;; Server state
;;; --------------------------------------------------------------------------

(defclass server ()
  ((socket-path :initarg :socket-path :reader server-socket-path)
   (socket :initform nil :accessor server-socket)
   (worker :initform nil :accessor server-worker)
   (worker-mutex :initform (sb-thread:make-mutex :name "clpm.repl-bridge.worker")
                 :reader server-worker-mutex)
   (current-package :initform (find-package "COMMON-LISP-USER")
                    :accessor server-current-package)
   (started-at :initform (get-universal-time) :reader server-started-at)
   (eval-count :initform 0 :accessor server-eval-count)
   (redefinitions :initform (make-hash-table :test 'equal)
                  :reader server-redefinitions)
   (shutdown-requested? :initform nil :accessor server-shutdown-requested?)))

(defvar *server* nil
  "Current server instance, bound during the daemon's lifetime so handlers can
reach it.")

(define-condition user-interrupt () ()
  (:documentation "Signaled inside the worker thread when the client closes
its connection or sends an explicit `interrupt' request."))

;;; --------------------------------------------------------------------------
;;; Worker thread
;;; --------------------------------------------------------------------------

(defstruct worker
  mailbox
  thread)

(defstruct eval-job
  form
  package-override
  result-mailbox
  thread)

(defstruct eval-result
  code        ; nil on success; "eval-error" / "reader-error" / "interrupted"
  value       ; string (prin1 of primary value) on success
  output
  error-output
  package
  elapsed-ms
  conditions  ; list of (:object ...) entries
  truncated?
  redefined)

(defun %list-restarts ()
  (mapcar (lambda (r) (string (restart-name r))) (compute-restarts)))

(defun %condition-json (condition &key include-backtrace)
  (let* ((type (string (type-of condition)))
         (msg (princ-to-string condition))
         (entries (list (cons "type" type)
                        (cons "message" msg)
                        (cons "restarts" (%json-array (%list-restarts))))))
    (when include-backtrace
      (let ((frames
              (handler-case
                  (let ((all (sb-debug:list-backtrace)))
                    (subseq all 0 (min (length all) +max-backtrace-frames+)))
                (error () '()))))
        (setf entries
              (append entries
                      (list (cons "backtrace"
                                  (%json-array
                                   (mapcar #'princ-to-string frames))))))))
    (list :object entries)))

(defun %read-form (form-text)
  "Read FORM-TEXT as one Lisp value. Signals an error on malformed input."
  (with-input-from-string (s form-text)
    (let ((value (read s nil 'eof)))
      (when (eq value 'eof)
        (error "empty form"))
      ;; Reject trailing forms so a single `eval' is, in fact, single.
      (let ((extra (read s nil 'eof)))
        (unless (eq extra 'eof)
          (error "trailing form after first expression")))
      value)))

(defparameter +definer-symbols+
  '(defun defmethod defmacro defgeneric defclass defstruct
    defvar defparameter defconstant define-condition defpackage)
  "Top-level forms whose first argument we track in the redefinition log.")

(defun %record-redefinition (form package)
  "If FORM is a top-level definer, record it under SERVER-REDEFINITIONS."
  (when (and (consp form) (symbolp (first form))
             (member (first form) +definer-symbols+ :test #'eq))
    (let* ((kind (string-downcase (symbol-name (first form))))
           (name (second form))
           (name-str (if (symbolp name)
                         (symbol-name name)
                         (princ-to-string name)))
           (pkg-name (cond
                       ((symbolp name) (package-name (symbol-package name)))
                       (t (package-name package))))
           (key (list kind name-str pkg-name))
           (record (list (cons "kind" kind)
                         (cons "name" name-str)
                         (cons "package" pkg-name)
                         (cons "form" (let ((*print-pretty* nil))
                                        (prin1-to-string form))))))
      (when *server*
        (setf (gethash key (server-redefinitions *server*))
              record))
      record)))

(defun %find-package-loose (name)
  "Look up a package by NAME, accepting any case. NAME is a string."
  (or (find-package name)
      (find-package (string-upcase name))
      (find-package (string-downcase name))))

(defun %eval-one (form-text &key package-override)
  "Evaluate FORM-TEXT inside the worker. Returns an eval-result struct."
  (let* ((sink (make-bounded-sink))
         (out-stream (%make-capture-stream sink))
         (err-stream (%make-capture-stream sink))
         (in-stream (make-string-input-stream ""))
         (start (get-internal-real-time))
         (form nil)
         (override-pkg (and package-override
                            (%find-package-loose package-override)))
         (package (or override-pkg
                      (and *server* (server-current-package *server*))
                      (find-package "COMMON-LISP-USER")))
         (value nil)
         (code nil)
         (conditions '())
         (redefined nil))
    (when (and package-override (null override-pkg))
      (let ((c (make-condition 'simple-error
                               :format-control "No such package: ~A"
                               :format-arguments (list package-override))))
        (setf code "reader-error")
        (push (%condition-json c) conditions)
        (return-from %eval-one
          (make-eval-result
           :code code
           :value nil
           :output ""
           :error-output ""
           :package (package-name (or *package* (find-package "COMMON-LISP-USER")))
           :elapsed-ms 0
           :conditions (nreverse conditions)
           :truncated? nil
           :redefined nil))))
    (labels ((finish (&key err-condition)
               (when err-condition
                 (setf code "eval-error")
                 (push (%condition-json err-condition :include-backtrace t)
                       conditions))
               (make-eval-result
                :code code
                :value (and (null code) (let ((*print-pretty* nil))
                                          (prin1-to-string value)))
                :output (%capture-text out-stream sink)
                :error-output (%capture-text err-stream sink)
                :package (package-name package)
                :elapsed-ms (round (* 1000.0 (/ (- (get-internal-real-time) start)
                                                 internal-time-units-per-second)))
                :conditions (nreverse conditions)
                :truncated? (bounded-sink-truncated? sink)
                :redefined redefined)))
      (handler-case
          (setf form (%read-form form-text))
        (error (c)
          (setf code "reader-error")
          (push (%condition-json c) conditions)
          (return-from %eval-one
            (finish))))
      (handler-case
          (let ((*standard-output* out-stream)
                (*error-output* err-stream)
                (*trace-output* out-stream)
                (*query-io* (make-two-way-stream in-stream out-stream))
                (*debug-io* (make-two-way-stream in-stream err-stream))
                (*terminal-io* (make-two-way-stream in-stream out-stream))
                (*standard-input* in-stream)
                (*package* package))
            (setf redefined (%record-redefinition form package))
            (setf value (eval form))
            ;; A call to (in-package ...) inside the form mutates *package*.
            ;; That change persists only when there was no per-call override
            ;; -- override semantics are "scoped to this eval".
            (setf package *package*)
            (when (and *server* (null override-pkg))
              (setf (server-current-package *server*) *package*)))
        (user-interrupt ()
          (setf code "interrupted")
          (return-from %eval-one (finish)))
        (error (c)
          (return-from %eval-one (finish :err-condition c))))
      (finish))))

(defun %worker-loop (mailbox)
  "Pull jobs from MAILBOX, eval each, post the result to job's result-mailbox.
Returns when a `:stop' sentinel arrives."
  (loop
    (let ((job (sb-concurrency:receive-message mailbox)))
      (cond
        ((eq job :stop) (return))
        (t
         (let ((result
                 (handler-case
                     (%eval-one (eval-job-form job)
                                :package-override (eval-job-package-override job))
                   ;; Any unexpected interrupt at this outermost level becomes
                   ;; an "interrupted" result for the requester.
                   (user-interrupt ()
                     (make-eval-result :code "interrupted"
                                       :value nil :output "" :error-output ""
                                       :package "" :elapsed-ms 0
                                       :conditions '() :truncated? nil
                                       :redefined nil)))))
           (sb-concurrency:send-message (eval-job-result-mailbox job) result)))))))

(defun %ensure-worker (server)
  "Start a worker thread for SERVER if none is alive. Returns the worker's
inbound mailbox. Thread-safe via SERVER-WORKER-MUTEX."
  (sb-thread:with-mutex ((server-worker-mutex server))
    (when (or (null (server-worker server))
              (not (sb-thread:thread-alive-p
                    (worker-thread (server-worker server)))))
      (let ((mailbox (sb-concurrency:make-mailbox)))
        (setf (server-worker server)
              (make-worker
               :mailbox mailbox
               :thread (sb-thread:make-thread
                        (lambda () (%worker-loop mailbox))
                        :name "clpm.repl-bridge.worker")))))
    (worker-mailbox (server-worker server))))

(defun %interrupt-worker (server)
  "Signal user-interrupt inside the worker, unwinding its current eval."
  (let ((w (server-worker server)))
    (when (and w (sb-thread:thread-alive-p (worker-thread w)))
      (sb-thread:interrupt-thread
       (worker-thread w)
       (lambda () (signal 'user-interrupt))))))

;;; --------------------------------------------------------------------------
;;; Method dispatch
;;; --------------------------------------------------------------------------

(defun %dispatch-method (server method params id)
  "Return a JSON response for METHOD; never raises."
  (handler-case
      (cond
        ((string= method "ping")
         (%success-response id
          (%json-object
           "pid" (sb-posix:getpid)
           "uptime_ms" (* 1000 (- (get-universal-time)
                                   (server-started-at server)))
           "lisp" (format nil "~A ~A"
                          (lisp-implementation-type)
                          (lisp-implementation-version))
           "eval_count" (server-eval-count server))))
        ((string= method "current-package")
         (%success-response id
          (%json-object "package"
                        (package-name (server-current-package server)))))
        ((string= method "set-package")
         (let* ((name (%json-getf params "name"))
                (pkg (and (stringp name) (%find-package-loose name))))
           (cond
             ((not (stringp name))
              (%error-response id "protocol-error" "missing `name` param"))
             ((null pkg)
              (%error-response id "eval-error" (format nil "No such package: ~A" name)))
             (t
              (setf (server-current-package server) pkg)
              (%success-response id
               (%json-object "package" (package-name pkg)))))))
        ((string= method "eval")
         (%dispatch-eval server params id))
        ((string= method "interrupt")
         (%interrupt-worker server)
         (%success-response id (%json-object)))
        ((string= method "reset")
         (let ((w (server-worker server)))
           (when (and w (sb-thread:thread-alive-p (worker-thread w)))
             (sb-thread:terminate-thread (worker-thread w)))
           (setf (server-worker server) nil)
           (clrhash (server-redefinitions server)))
         (%success-response id (%json-object)))
        ((string= method "describe")
         (%dispatch-describe server params id))
        ((string= method "list-redefinitions")
         (%success-response id
          (%json-object "entries"
                        (%json-array
                         (loop for v being the hash-values of
                               (server-redefinitions server)
                               collect (list :object v))))))
        ((string= method "shutdown")
         (setf (server-shutdown-requested? server) t)
         (%success-response id (%json-object)))
        (t
         (%error-response id "protocol-error"
                          (format nil "unknown method: ~A" method))))
    (error (c)
      (%error-response id "protocol-error"
                       (format nil "dispatch failed: ~A" c)))))

(defun %dispatch-eval (server params id)
  (let* ((form (%json-getf params "form"))
         (package-override (%json-getf params "package")))
    (cond
      ((not (stringp form))
       (%error-response id "protocol-error" "missing `form` param"))
      (t
       (let* ((mailbox (%ensure-worker server))
              (reply-box (sb-concurrency:make-mailbox))
              (job (make-eval-job
                    :form form
                    :package-override package-override
                    :result-mailbox reply-box)))
         (incf (server-eval-count server))
         (sb-concurrency:send-message mailbox job)
         (let ((result (sb-concurrency:receive-message reply-box)))
           (cond
             ((null (eval-result-code result))
              (let ((payload
                      (list :object
                            (append
                             (list (cons "value" (eval-result-value result))
                                   (cons "output" (eval-result-output result))
                                   (cons "error_output" (eval-result-error-output result))
                                   (cons "package" (eval-result-package result))
                                   (cons "elapsed_ms" (eval-result-elapsed-ms result))
                                   (cons "conditions"
                                         (%json-array (eval-result-conditions result))))
                             (when (eval-result-redefined result)
                               (list (cons "redefined"
                                           (list :object (eval-result-redefined result)))))
                             (when (eval-result-truncated? result)
                               (list (cons "truncated" t)))))))
                (if (eval-result-truncated? result)
                    (%json-object "id" id "result" payload
                                  "warning" "output-truncated")
                    (%success-response id payload))))
             (t
              (let ((details
                      (list :object
                            (list (cons "output" (eval-result-output result))
                                  (cons "error_output" (eval-result-error-output result))
                                  (cons "package" (eval-result-package result))
                                  (cons "elapsed_ms" (eval-result-elapsed-ms result))
                                  (cons "conditions"
                                        (%json-array
                                         (eval-result-conditions result)))))))
                (%error-response id (eval-result-code result)
                                 (or (let ((c0 (first (eval-result-conditions result))))
                                       (when c0
                                         (cdr (assoc "message"
                                                     (cadr c0)
                                                     :test #'string=))))
                                     (eval-result-code result))
                                 :details details))))))))))

(defun %dispatch-describe (server params id)
  (let* ((sym-name (%json-getf params "symbol"))
         (pkg-name (or (%json-getf params "package")
                       (package-name (server-current-package server)))))
    (cond
      ((not (stringp sym-name))
       (%error-response id "protocol-error" "missing `symbol` param"))
      (t
       (let* ((pkg (or (%find-package-loose pkg-name)
                       (return-from %dispatch-describe
                         (%error-response id "eval-error"
                                          (format nil "no such package: ~A"
                                                  pkg-name)))))
              (sym (find-symbol (string-upcase sym-name) pkg)))
         (cond
           ((null sym)
            (%error-response id "eval-error"
                             (format nil "no symbol ~A in ~A"
                                     sym-name pkg-name)))
           (t
            (let ((text (with-output-to-string (s)
                          (let ((*package* pkg)) (describe sym s)))))
              (%success-response id
               (%json-object "output" text))))))))))

;;; --------------------------------------------------------------------------
;;; Server: accept loop
;;; --------------------------------------------------------------------------

(defun start-server (&key socket-path)
  "Start a daemon listening on SOCKET-PATH (a string filesystem path). Blocks
until a `shutdown' request arrives. Cleans up the socket and ensures the
worker thread is stopped before returning.

Sets the toplevel value of `*server*' (not a dynamic binding) so the worker
thread sees the same instance; only one daemon may run per process."
  (check-type socket-path string)
  (ignore-errors (delete-file socket-path))
  (let* ((server (make-instance 'server :socket-path socket-path))
         (sock (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (setf *server* server)
    (unwind-protect
         (progn
           (sb-bsd-sockets:socket-bind sock socket-path)
           (sb-posix:chmod socket-path #o600)
           (sb-bsd-sockets:socket-listen sock 8)
           (setf (server-socket server) sock)
           ;; Spawn a thread per connection so eval (which blocks on the worker)
           ;; doesn't wedge the accept loop. The worker mailbox serializes
           ;; eval requests; other methods (interrupt, ping, status, ...) run
           ;; concurrently with whatever the worker is doing.
           (loop until (server-shutdown-requested? server) do
             (let ((conn (sb-bsd-sockets:socket-accept sock)))
               (sb-thread:make-thread
                (let ((c conn))
                  (lambda ()
                    (unwind-protect
                         (handler-case
                             (%handle-connection server c)
                           (error (e)
                             (format *error-output*
                                     "repl-bridge handler error: ~A~%" e)))
                      (ignore-errors (sb-bsd-sockets:socket-close c)))))
                :name "clpm.repl-bridge.conn"))))
      (let ((w (server-worker server)))
        (when (and w (sb-thread:thread-alive-p (worker-thread w)))
          (sb-concurrency:send-message (worker-mailbox w) :stop)
          (handler-case
              (sb-thread:join-thread (worker-thread w))
            (error () nil))))
      (ignore-errors (sb-bsd-sockets:socket-close sock))
      (ignore-errors (delete-file socket-path))
      (setf *server* nil))))

(defun %handle-connection (server conn)
  (let ((stream (sb-bsd-sockets:socket-make-stream
                 conn :input t :output t
                       :buffering :line
                       :external-format :utf-8
                       :element-type 'character)))
    (handler-case
        (let ((line (%read-request-line stream)))
          (cond
            ((null line))  ; EOF: client closed without sending anything
            (t
             (let* ((request (handler-case
                                 (clpm.io.json:read-json-from-string line)
                               (clpm.errors:clpm-parse-error (c)
                                 (%write-line-json
                                  stream
                                  (%error-response nil "protocol-error"
                                                   (princ-to-string c)))
                                 (return-from %handle-connection))))
                    (id (%json-getf request "id"))
                    (method (%json-getf request "method"))
                    (params (%json-getf request "params")))
               (cond
                 ((not (stringp method))
                  (%write-line-json stream
                                    (%error-response id "protocol-error"
                                                     "missing `method'")))
                 (t
                  ;; Spawn a watcher for client disconnect: if the client
                  ;; closes mid-eval, interrupt the worker.
                  (let ((response (%dispatch-method server method params id)))
                    (handler-case
                        (%write-line-json stream response)
                      (error () nil)))))))))
      (error (c)
        (handler-case
            (%write-line-json stream
                              (%error-response nil "protocol-error"
                                               (princ-to-string c)))
          (error () nil))))))

;;; --------------------------------------------------------------------------
;;; Client
;;; --------------------------------------------------------------------------

(defun %connect (socket-path &key (timeout-seconds 5))
  "Return a connected sb-bsd-sockets:local-socket, or NIL if the socket file
is absent / refused. Polls for up to TIMEOUT-SECONDS."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout-seconds internal-time-units-per-second))))
    (loop
      (handler-case
          (let ((s (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
            (sb-bsd-sockets:socket-connect s socket-path)
            (return s))
        (error ()
          (when (>= (get-internal-real-time) deadline)
            (return nil))
          (sleep 0.05))))))

(defun send-request (socket-path method &key params (id 1) (connect-timeout 5))
  "Send one request and return the parsed response. Returns
   :no-daemon if the socket is absent or unreachable,
   :io-error  if the connection dropped mid-exchange.
The response is a `(:object ...)` form on success / error.

Responses are read without a size cap; daemon output (`+max-output-bytes+`,
1 MB) can legitimately fill a line."
  (let ((sock (%connect socket-path :timeout-seconds connect-timeout)))
    (unless sock
      (return-from send-request :no-daemon))
    (unwind-protect
         (let* ((stream (sb-bsd-sockets:socket-make-stream
                         sock :input t :output t :buffering :full
                              :external-format :utf-8
                              :element-type 'character))
                (request
                  (%json-object "id" id
                                "method" method
                                "params" (or params (%json-object)))))
           (handler-case
               (progn
                 (%write-line-json stream request)
                 (let ((line (read-line stream nil nil)))
                   (cond
                     ((null line) :io-error)
                     (t (clpm.io.json:read-json-from-string line)))))
             (error () :io-error)))
      (ignore-errors (sb-bsd-sockets:socket-close sock)))))
