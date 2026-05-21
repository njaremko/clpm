;;;; repl.lisp - Persistent Lisp image driven over a Unix-socket JSON-RPC
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
  #+sbcl (require :sb-bsd-sockets)
  #+sbcl (require :sb-concurrency)
  #+sbcl (require :sb-posix)
  #+sbcl (require :sb-introspect))

(in-package #:clpm.repl)

;;; --------------------------------------------------------------------------
;;; Protocol helpers
;;; --------------------------------------------------------------------------

(defparameter +max-request-bytes+ (* 64 1024)
  "Hard cap on request-line length. Excess bytes are read-and-discarded and
the daemon replies `protocol-error'.")

(defparameter +slow-eval-threshold-ms+ 1000
  "Evals taking longer than this in real time get a `slow-eval' entry
in the event log so a future operator can find pathological forms.")

(defparameter +heartbeat-interval-seconds+ 30
  "How often a long-running streaming eval emits an `event:heartbeat'
frame.  Lets a client tell `still running' from `daemon dead'.")

(defparameter +max-output-bytes+ (* 1024 1024)
  "Hard cap on captured stdout+stderr per eval. Excess is silently dropped
and the eval result carries `truncated: true'.")

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

(defstruct terminal-response
  "Internal terminal response. JSON rendering happens only at the wire
boundary so handlers do not need to hand-build the outer `{id,result/error}'
frame."
  id
  kind
  result
  code
  message
  details)

(defun %terminal-response-json (response)
  (ecase (terminal-response-kind response)
    (:success
     (%json-object "id" (terminal-response-id response)
                   "result" (terminal-response-result response)))
    (:error
     (let ((err (list (cons "code" (terminal-response-code response))
                      (cons "message" (terminal-response-message response)))))
       (when (terminal-response-details response)
         (setf err (append err
                           (list (cons "details"
                                       (terminal-response-details response))))))
       (%json-object "id" (terminal-response-id response)
                     "error" (list :object err))))))

(defun %wire-json (frame)
  (cond
    ((terminal-response-p frame) (%terminal-response-json frame))
    (t frame)))

(defun %terminal-response-error-code (response)
  (etypecase response
    (null nil)
    (terminal-response
     (ecase (terminal-response-kind response)
       (:success nil)
       (:error (terminal-response-code response))))))

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
  (clpm.io.json:write-json (%wire-json json) stream)
  (write-char #\Newline stream)
  (force-output stream))

(defun %success-response (id result)
  (make-terminal-response :id id :kind :success :result result))

(defun %error-response (id code message &key details)
  (make-terminal-response :id id
                          :kind :error
                          :code code
                          :message message
                          :details details))

;;; --------------------------------------------------------------------------
;;; Output streams: bounded sink
;;; --------------------------------------------------------------------------

(defstruct bounded-sink
  "A character sink that accumulates up to +MAX-OUTPUT-BYTES+ across two
streams (stdout + stderr combined). Once the combined cap is reached, further
writes are silently dropped and `truncated?' flips to T."
  (limit +max-output-bytes+ :type fixnum)
  (used 0 :type fixnum)
  (truncated? nil :type boolean)
  (mutex (clpm.repl.compat:make-mutex
          :name "clpm.repl.bounded-sink")))

(defun %bounded-sink-accept-string (sink string start end)
  "Return the prefix of STRING[start,end) still admitted by SINK.
Charges the accepted characters immediately so capture is bounded while
the user code writes, not after evaluation unwinds."
  (let* ((end (or end (length string)))
         (len (- end start)))
    (cond
      ((<= len 0) "")
      (t
       (clpm.repl.compat:with-mutex ((bounded-sink-mutex sink))
         (let ((remaining (- (bounded-sink-limit sink)
                             (bounded-sink-used sink))))
           (cond
             ((<= remaining 0)
              (setf (bounded-sink-truncated? sink) t)
              "")
             ((<= len remaining)
              (incf (bounded-sink-used sink) len)
              (subseq string start end))
             (t
              (setf (bounded-sink-used sink) (bounded-sink-limit sink)
                    (bounded-sink-truncated? sink) t)
              (subseq string start (+ start remaining))))))))))

#+sbcl
(defclass bounded-output-stream (sb-gray:fundamental-character-output-stream)
  ((sink :initarg :sink :reader bounded-output-stream-sink)
   (full :initform (make-string-output-stream)
         :reader bounded-output-stream-full)
   (mutex :initform (clpm.repl.compat:make-mutex
                     :name "clpm.repl.bounded-output")
          :reader bounded-output-stream-mutex)))

#+sbcl
(defmethod sb-gray:stream-write-char ((s bounded-output-stream) ch)
  (let ((accepted (%bounded-sink-accept-string
                   (bounded-output-stream-sink s)
                   (string ch)
                   0
                   1)))
    (when (plusp (length accepted))
      (clpm.repl.compat:with-mutex
          ((bounded-output-stream-mutex s))
        (write-string accepted (bounded-output-stream-full s)))))
  ch)

#+sbcl
(defmethod sb-gray:stream-write-string ((s bounded-output-stream) string
                                         &optional (start 0) end)
  (let ((accepted (%bounded-sink-accept-string
                   (bounded-output-stream-sink s)
                   string
                   start
                   (or end (length string)))))
    (when (plusp (length accepted))
      (clpm.repl.compat:with-mutex
          ((bounded-output-stream-mutex s))
        (write-string accepted (bounded-output-stream-full s)))))
  string)

#+sbcl
(defmethod sb-gray:stream-line-column ((s bounded-output-stream))
  (declare (ignore s))
  nil)

#+sbcl
(defmethod sb-gray:stream-finish-output ((s bounded-output-stream))
  (declare (ignore s))
  nil)

#+sbcl
(defmethod sb-gray:stream-force-output ((s bounded-output-stream))
  (declare (ignore s))
  nil)

#+sbcl
(defun bounded-output-stream-final-text (s)
  (clpm.repl.compat:with-mutex
      ((bounded-output-stream-mutex s))
    (get-output-stream-string (bounded-output-stream-full s))))

(defun %make-capture-stream (sink)
  "Return a gray stream proxy that writes into SINK until its cap is hit."
  #+sbcl
  (make-instance 'bounded-output-stream :sink sink)
  #-sbcl
  (progn
    (declare (ignore sink))
    (make-string-output-stream)))

	(defun %capture-text (stream sink)
	  "Read STREAM's accumulated string, charge it to SINK, and return the
	possibly-truncated text."
	  (cond
	    #+sbcl
	    ((typep stream 'bounded-output-stream)
	     (bounded-output-stream-final-text stream))
	    #+sbcl
	    ((typep stream 'streaming-output-stream)
	     (streaming-output-stream-final-text stream))
	    (t
     ;; Portable fallback: implementations without Gray streams still get
     ;; bounded terminal output, though not bounded during the write itself.
     (let ((text (get-output-stream-string stream)))
       (cond
         ((bounded-sink-truncated? sink) "")
         ((> (+ (bounded-sink-used sink) (length text))
             (bounded-sink-limit sink))
          (setf (bounded-sink-truncated? sink) t)
          (let ((remaining (max 0 (- (bounded-sink-limit sink)
                                     (bounded-sink-used sink)))))
            (setf (bounded-sink-used sink) (bounded-sink-limit sink))
            (subseq text 0 (min (length text) remaining))))
         (t
          (incf (bounded-sink-used sink) (length text))
          text))))))

;;; --------------------------------------------------------------------------
;;; Streaming output: a Gray stream that mirrors writes into an
;;; accumulating capture buffer *and* emits chunks as `event: stdout' /
;;; `event: stderr' frames to the request context. Used when the client
;;; sets `--stream' on eval.
;;; --------------------------------------------------------------------------

#+sbcl
(defclass streaming-output-stream (sb-gray:fundamental-character-output-stream)
  ((ctx :initarg :ctx :reader streaming-output-stream-ctx)
   (channel :initarg :channel :reader streaming-output-stream-channel)
   (sink :initarg :sink :reader streaming-output-stream-sink)
   (buffer :initform (make-array 0 :element-type 'character
                                   :fill-pointer 0 :adjustable t)
           :reader streaming-output-stream-buffer)
   (mutex :initform (clpm.repl.compat:make-mutex
                     :name "clpm.repl.stream-buf")
          :reader streaming-output-stream-mutex)
   (full :initform (make-string-output-stream)
         :reader streaming-output-stream-full)
   (flush-bytes :initarg :flush-bytes :initform 4096
                :reader streaming-output-stream-flush-bytes)))

#+sbcl
(defparameter +stream-flush-min-bytes+ 4096)

#+sbcl
(defun %streaming-flush (s)
  "Drain S's buffer and emit a single `event' frame with that chunk.
Called holding the stream's mutex."
  (let ((b (streaming-output-stream-buffer s)))
    (when (plusp (length b))
      (let ((chunk (subseq b 0 (length b))))
        (setf (fill-pointer b) 0)
        (%emit-event (streaming-output-stream-ctx s)
                     (streaming-output-stream-channel s)
                     "data" chunk)))))

#+sbcl
(defmethod sb-gray:stream-write-char ((s streaming-output-stream) ch)
  (let ((accepted (%bounded-sink-accept-string
                   (streaming-output-stream-sink s)
                   (string ch)
                   0
                   1)))
    (when (plusp (length accepted))
      (clpm.repl.compat:with-mutex ((streaming-output-stream-mutex s))
        (let ((accepted-char (schar accepted 0)))
          (vector-push-extend accepted-char (streaming-output-stream-buffer s))
          (write-char accepted-char (streaming-output-stream-full s))
          (when (>= (length (streaming-output-stream-buffer s))
                    (streaming-output-stream-flush-bytes s))
            (%streaming-flush s))
          (when (char= accepted-char #\Newline)
            (%streaming-flush s))))))
  ch)

#+sbcl
(defmethod sb-gray:stream-write-string ((s streaming-output-stream) string
                                         &optional (start 0) end)
  (let ((accepted (%bounded-sink-accept-string
                   (streaming-output-stream-sink s)
                   string
                   start
                   (or end (length string)))))
    (when (plusp (length accepted))
      (clpm.repl.compat:with-mutex ((streaming-output-stream-mutex s))
        (loop for i from 0 below (length accepted)
              do (vector-push-extend (schar accepted i)
                                     (streaming-output-stream-buffer s)))
        (write-string accepted (streaming-output-stream-full s))
        (when (>= (length (streaming-output-stream-buffer s))
                  (streaming-output-stream-flush-bytes s))
          (%streaming-flush s))
        ;; Flush after any newline lands.
        (when (find #\Newline accepted)
          (%streaming-flush s)))))
  string)

#+sbcl
(defmethod sb-gray:stream-line-column ((s streaming-output-stream))
  (declare (ignore s))
  nil)

#+sbcl
(defmethod sb-gray:stream-finish-output ((s streaming-output-stream))
  (clpm.repl.compat:with-mutex ((streaming-output-stream-mutex s))
    (%streaming-flush s)))

#+sbcl
(defmethod sb-gray:stream-force-output ((s streaming-output-stream))
  (clpm.repl.compat:with-mutex ((streaming-output-stream-mutex s))
    (%streaming-flush s)))

#+sbcl
(defun streaming-output-stream-final-text (s)
  "Drain S's residual buffer and return the full captured text. Idempotent
once called; subsequent reads return the empty string."
  (clpm.repl.compat:with-mutex ((streaming-output-stream-mutex s))
    (%streaming-flush s))
  (get-output-stream-string (streaming-output-stream-full s)))

;;; --------------------------------------------------------------------------
;;; Bidirectional `query' input stream. When the form reads from
;;; *standard-input* / *query-io*, we emit `event: query' and block the
;;; worker on a mailbox until the client posts `query-response'.
;;; --------------------------------------------------------------------------

#+sbcl
(defclass query-input-stream (sb-gray:fundamental-character-input-stream)
  ((ctx :initarg :ctx :reader query-input-stream-ctx)
   ;; Per-stream mailbox the connection handler pushes query-response values
   ;; into; the stream's read methods block here.
   (mailbox :initarg :mailbox :reader query-input-stream-mailbox)
   (buffer :initform (make-array 0 :element-type 'character
                                   :fill-pointer 0 :adjustable t)
           :reader query-input-stream-buffer)
   (buffer-pos :initform 0 :accessor query-input-stream-buffer-pos)
   (eof? :initform nil :accessor query-input-stream-eof?)))

(defun %query-refill (s)
  "If the buffer is empty, emit a query event and wait for response.
The response is appended to the buffer with a trailing newline so a
single (read-line) returns the answer."
  (when (and (>= (query-input-stream-buffer-pos s)
                 (length (query-input-stream-buffer s)))
             (not (query-input-stream-eof? s)))
    (%emit-event (query-input-stream-ctx s) "query")
    (let ((reply (clpm.repl.compat:receive-message
                  (query-input-stream-mailbox s))))
      (cond
        ((eq reply :eof) (setf (query-input-stream-eof? s) t))
        ((stringp reply)
         (let ((b (query-input-stream-buffer s)))
           (loop for ch across reply do (vector-push-extend ch b))
           (vector-push-extend #\Newline b)))
        (t (setf (query-input-stream-eof? s) t))))))

#+sbcl
(defmethod sb-gray:stream-read-char ((s query-input-stream))
  (%query-refill s)
  (cond
    ((query-input-stream-eof? s) :eof)
    (t (prog1 (aref (query-input-stream-buffer s)
                    (query-input-stream-buffer-pos s))
         (incf (query-input-stream-buffer-pos s))))))

#+sbcl
(defmethod sb-gray:stream-unread-char ((s query-input-stream) ch)
  (declare (ignore ch))
  (decf (query-input-stream-buffer-pos s))
  nil)

#+sbcl
(defmethod sb-gray:stream-listen ((s query-input-stream))
  (< (query-input-stream-buffer-pos s)
     (length (query-input-stream-buffer s))))

;;; --------------------------------------------------------------------------
;;; Transport: Unix socket vs. TCP loopback
;;;
;;; Unix sockets are the preferred transport. Both transports authenticate
;;; with a 32-hex-char shared token: Unix writes it beside the socket, TCP
;;; writes it into `.clpm/repl.port' with the port. The project_root field is
;;; identity, not authority; the token is the endpoint capability.
;;; --------------------------------------------------------------------------

(defstruct transport
  "Listener configuration shared between server and client.

KIND is :unix or :tcp.
PATH is the filesystem path the transport advertises:
  - :unix -> the Unix-domain socket file (mode 0600)
  - :tcp  -> the `.clpm/repl.port' file containing `<port>~%<token>~%'
TOKEN is a 32-hex-char shared secret.
LISTENER and PORT are filled in once the listener is opened."
  (kind :unix :type (member :unix :tcp))
  (path "" :type string)
  (token nil :type (or null string))
  (listener nil)
  (port nil :type (or null (integer 0 65535))))

(defun %default-transport-kind ()
  "Pick the right transport for the host. Unix everywhere except Windows."
  (if (and (find-package "UIOP/OS")
           (fboundp (find-symbol "OS-WINDOWS-P" "UIOP/OS"))
           (funcall (find-symbol "OS-WINDOWS-P" "UIOP/OS")))
      :tcp
      :unix))

(defun %random-token ()
  "Return a 32-hex-char shared secret used for transport authentication.
The token is generated from `clpm.platform:secure-random-bytes' (which reads
from /dev/urandom on Unix), so guessing it costs 2^128."
  (clpm.crypto.sha256:bytes-to-hex
   (clpm.platform:secure-random-bytes 16)))

(defun %constant-string= (a b)
  "String equality that doesn't short-circuit on the first mismatch. Used for
token comparison so a remote attacker can't reconstruct the token byte-by-byte
from response timings. Returns NIL on length mismatch (also constant-time
within the longer string)."
  (declare (type (or null string) a b))
  (cond
    ((or (null a) (null b)) (eq a b))
    ((/= (length a) (length b)) nil)
    (t
     (let ((diff 0))
       (declare (type fixnum diff))
       (loop for i fixnum from 0 below (length a)
             do (setf diff (logior diff (logxor (char-code (char a i))
                                                (char-code (char b i))))))
       (zerop diff)))))

(defun %write-endpoint-capability-file (path writer)
  "Write PATH through a temporary file and atomic rename.

Endpoint capability files live under project directories. Renaming a fresh
regular file over PATH replaces a symlink at PATH instead of writing through it
into another project's endpoint state."
  (let* ((target (namestring (pathname path)))
         (tmp (format nil "~A.tmp.~D.~A"
                      target
                      (clpm.repl.compat:getpid)
                      (%random-token))))
    (unwind-protect
         (progn
           (ensure-directories-exist target)
           (with-open-file (s tmp :direction :output
                                  :if-exists :error
                                  :if-does-not-exist :create
                                  :external-format :utf-8)
             (funcall writer s))
           #+sbcl (sb-posix:chmod tmp #o600)
           #+sbcl (sb-posix:rename tmp target)
           #-sbcl (rename-file tmp target)
           #+sbcl (sb-posix:chmod target #o600))
      (ignore-errors (delete-file tmp)))))

(defun %write-port-file (path port token)
  (%write-endpoint-capability-file
   path
   (lambda (s)
     (format s "~D~%~A~%" port token))))

(defun %unix-token-path (socket-path)
  "Return the token-file path paired with SOCKET-PATH."
  (concatenate 'string socket-path ".token"))

(defun %write-token-file (path token)
  (%write-endpoint-capability-file
   path
   (lambda (s)
     (format s "~A~%" token))))

(defun %read-token-file (path)
  "Read a 32-hex endpoint token from PATH, or signal an error."
  (with-open-file (s path :direction :input :external-format :utf-8)
    (let ((line (read-line s nil nil)))
      (unless (and (stringp line) (= 32 (length line)))
        (error "Invalid token file ~A" path))
      (string-trim '(#\Space #\Newline #\Return) line))))

(defun %read-port-file (path)
  "Return (values port token) from PATH, or signal an error."
  (with-open-file (s path :direction :input :external-format :utf-8)
    (let* ((port-line (read-line s nil nil))
           (token-line (read-line s nil nil))
           (port (and port-line (parse-integer (string-trim '(#\Space #\Newline #\Return) port-line)
                                                :junk-allowed nil))))
      (unless (and (integerp port) (stringp token-line))
        (error "Invalid port file ~A" path))
      (values port (string-trim '(#\Space #\Newline #\Return) token-line)))))

(defun %open-listener (transport)
  "Open and bind the listening socket described by TRANSPORT. Stores the
socket (and, for :tcp, the bound port) back into the struct. For :tcp, also
writes the port file."
  (ecase (transport-kind transport)
    (:unix
     (let ((sock (make-instance 'sb-bsd-sockets:local-socket :type :stream))
           (token (or (transport-token transport)
                      (setf (transport-token transport) (%random-token)))))
       (ignore-errors (delete-file (transport-path transport)))
       (%write-token-file (%unix-token-path (transport-path transport)) token)
       (sb-bsd-sockets:socket-bind sock (transport-path transport))
       #+sbcl
       (handler-case (sb-posix:chmod (transport-path transport) #o600)
         (error () nil))
       (sb-bsd-sockets:socket-listen sock 8)
       (setf (transport-listener transport) sock)))
    (:tcp
     (let ((sock (make-instance 'sb-bsd-sockets:inet-socket
                                :type :stream :protocol :tcp)))
       ;; Bind to localhost on a kernel-assigned ephemeral port.
       (sb-bsd-sockets:socket-bind sock #(127 0 0 1) 0)
       (multiple-value-bind (addr port) (sb-bsd-sockets:socket-name sock)
         (declare (ignore addr))
         (setf (transport-port transport) port))
       (sb-bsd-sockets:socket-listen sock 8)
       (setf (transport-listener transport) sock)
       (%write-port-file (transport-path transport)
                         (transport-port transport)
                         (or (transport-token transport)
                             (setf (transport-token transport)
                                   (%random-token)))))))
  transport)

(defun %close-listener (transport)
  "Close the listening socket and remove the advertise file."
  (when (transport-listener transport)
    (ignore-errors (sb-bsd-sockets:socket-close (transport-listener transport)))
    (setf (transport-listener transport) nil))
  (ignore-errors (delete-file (transport-path transport)))
  (when (eq (transport-kind transport) :unix)
    (ignore-errors (delete-file (%unix-token-path (transport-path transport))))))

(defun %connect-transport (kind path &key (timeout-seconds 5))
  "Open a connected stream socket. For :unix, PATH is the socket path. For
:tcp, PATH is the port file path; the port and token are read from it.
Returns (values socket token).

Polls for up to TIMEOUT-SECONDS so an autostart parent can race the daemon."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout-seconds internal-time-units-per-second))))
    (loop
      (handler-case
          (return
            (ecase kind
              (:unix
               (let ((token (%read-token-file (%unix-token-path path)))
                     (s (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
                 (sb-bsd-sockets:socket-connect s path)
                 (values s token)))
              (:tcp
               (multiple-value-bind (port token) (%read-port-file path)
                 (let ((s (make-instance 'sb-bsd-sockets:inet-socket
                                         :type :stream :protocol :tcp)))
                   (sb-bsd-sockets:socket-connect s #(127 0 0 1) port)
                   (values s token))))))
        (error ()
          (when (>= (get-internal-real-time) deadline)
            (return (values nil nil)))
          (sleep 0.05))))))

(defun %infer-transport-kind (path)
  "Guess transport kind from PATH's extension: `.port' -> :tcp, else :unix."
  (cond
    ((and (>= (length path) 5)
          (string= ".port" path :start2 (- (length path) 5)))
     :tcp)
    (t :unix)))

;;; --------------------------------------------------------------------------
;;; Server state
;;; --------------------------------------------------------------------------

(defclass server ()
  ((socket-path :initarg :socket-path :reader server-socket-path)
   (project-root :initarg :project-root :initform nil :reader server-project-root)
   (initial-package :initarg :initial-package
                    :initform (find-package "COMMON-LISP-USER")
                    :reader server-initial-package)
   (transport :initarg :transport :initform nil :accessor server-transport)
   (socket :initform nil :accessor server-socket)
   (workers :initform (make-hash-table :test 'equal)
            :reader server-workers)
   (workers-mutex :initform (clpm.repl.compat:make-mutex :name "clpm.repl.workers")
                  :reader server-workers-mutex)
   (concurrent-counter :initform 0 :accessor server-concurrent-counter)
   (started-at :initform (get-universal-time) :reader server-started-at)
   (eval-count :initform 0 :accessor server-eval-count)
   ;; method-name -> (cons total-count error-count). Used by `ping' to
   ;; surface a per-method histogram, and by request logging.
   (method-counts :initform (make-hash-table :test 'equal)
                  :reader server-method-counts)
   (method-counts-mutex :initform (clpm.repl.compat:make-mutex
                                   :name "clpm.repl.counts")
                        :reader server-method-counts-mutex)
   (recent-error-count :initform 0 :accessor server-recent-error-count)
   (inspectors :initform (make-hash-table :test 'equal)
               :reader server-inspectors)
   (inspectors-mutex :initform (clpm.repl.compat:make-mutex
                                :name "clpm.repl.inspectors")
                     :reader server-inspectors-mutex)
   (inspector-counter :initform 0 :accessor server-inspector-counter)
   (watches :initform (make-hash-table :test 'eql)
            :reader server-watches)
   (watches-mutex :initform (clpm.repl.compat:make-mutex
                             :name "clpm.repl.watches")
                  :reader server-watches-mutex)
   (watch-counter :initform 0 :accessor server-watch-counter)
   (traces :initform (make-hash-table :test 'eq)
           :reader server-traces)
   (debug-sessions :initform (make-hash-table :test 'eql)
                   :reader server-debug-sessions)
   (debug-sessions-mutex :initform (clpm.repl.compat:make-mutex
                                    :name "clpm.repl.debug-sessions")
                         :reader server-debug-sessions-mutex)
   (debug-session-counter :initform 0 :accessor server-debug-session-counter)
   (shutdown-requested? :initform nil :accessor server-shutdown-requested?)
   (event-log :initform nil :accessor server-event-log)))

(defvar *active-project-server-roots* '()
  "Project roots whose REPL daemons are active in this Lisp process.")

(defvar *active-project-server-roots-mutex*
  (clpm.repl.compat:make-mutex :name "clpm.repl.project-roots"))

(defvar *reserved-project-server-root* nil
  "Project root reserved by the current daemon-start dynamic extent.")

(defun %project-root-string-p (project-root)
  (and (stringp project-root) (plusp (length project-root))))

(defun %register-active-project-server (project-root)
  "Register PROJECT-ROOT as the only project daemon in this Lisp image."
  (when (%project-root-string-p project-root)
    (clpm.repl.compat:with-mutex (*active-project-server-roots-mutex*)
      (when *active-project-server-roots*
        (error "A project REPL daemon is already active in this Lisp image; start each project daemon in a separate Lisp process for isolation."))
      (push project-root *active-project-server-roots*)
      t)))

(defun %unregister-active-project-server (project-root)
  (when (%project-root-string-p project-root)
    (clpm.repl.compat:with-mutex (*active-project-server-roots-mutex*)
      (setf *active-project-server-roots*
            (remove project-root *active-project-server-roots*
                    :test #'string=)))))

(defun %reserved-project-server-root-p (project-root)
  (and (%project-root-string-p project-root)
       (%project-root-string-p *reserved-project-server-root*)
       (string= project-root *reserved-project-server-root*)))

(defun call-with-project-server-reservation (project-root thunk)
  "Run THUNK after reserving PROJECT-ROOT as this Lisp image's daemon owner."
  (cond
    ((not (%project-root-string-p project-root))
     (funcall thunk))
    (t
     (let ((registered nil))
       (unwind-protect
            (progn
              (setf registered (%register-active-project-server project-root))
              (let ((*reserved-project-server-root* project-root))
                (funcall thunk)))
         (when registered
           (%unregister-active-project-server project-root)))))))

(defun %server-default-pathname-defaults (server)
  "Return the pathname defaults that belong to SERVER's project image."
  (let ((project-root (and server (server-project-root server))))
    (if (and (stringp project-root) (plusp (length project-root)))
        (uiop:ensure-directory-pathname project-root)
        *default-pathname-defaults*)))

(defparameter +project-package-prefix+ "CLPM.REPL.USER."
  "Prefix for private project REPL packages.")

(defun %project-package-name ()
  "Return a fresh private package name for a project daemon."
  (loop for name = (concatenate 'string +project-package-prefix+ (%random-token))
        unless (find-package name)
          return name))

(defun %project-package-p (package)
  (and package
       (let ((name (package-name package))
             (prefix +project-package-prefix+))
         (and (>= (length name) (length prefix))
              (string= prefix name :end2 (length prefix))))))

(defun %project-initial-package (project-root)
  "Return the default package for a project daemon.

Raw in-process test servers that do not carry PROJECT-ROOT keep CL-USER.
Project daemons get a private package whose printed name is never part of
the public protocol."
  (cond
    ((and (stringp project-root) (plusp (length project-root)))
     (make-package (%project-package-name) :use '("COMMON-LISP")))
    (t
     (find-package "COMMON-LISP-USER"))))

(defun %delete-project-initial-package (server)
  "Delete SERVER's CLPM-owned project package after daemon shutdown."
  (let ((project-root (and server (server-project-root server)))
        (package (and server (server-initial-package server))))
    (when (and (stringp project-root)
               package
               (%project-package-p package))
      (ignore-errors (delete-package package)))))

(defun %current-server-binding ()
  (and (boundp '*server*) (symbol-value '*server*)))

(defun %public-package-name (package &optional server)
  "Return the protocol-visible name for PACKAGE in SERVER."
  (let ((server (or server (%current-server-binding))))
    (cond
      ((and server
            (server-project-root server)
            package
            (eq package (server-initial-package server)))
       "COMMON-LISP-USER")
      (package (package-name package))
      (t "COMMON-LISP-USER"))))

(defun %public-package-list-name (package &optional server)
  "Return PACKAGE's public name, or NIL when another project owns it."
  (let ((server (or server (%current-server-binding))))
    (cond
      ((and server
            (server-project-root server)
            (%project-package-p package)
            (not (eq package (server-initial-package server))))
       nil)
      (t (%public-package-name package server)))))

(defun %replace-substrings (text old new)
  "Return TEXT with every OLD substring replaced by NEW."
  (cond
    ((or (not (stringp text))
         (not (stringp old))
         (zerop (length old)))
     text)
    (t
     (with-output-to-string (out)
       (loop with start = 0
             for pos = (search old text :start2 start)
             do (cond
                  (pos
                   (write-string text out :start start :end pos)
                   (write-string new out)
                   (setf start (+ pos (length old))))
                  (t
                   (write-string text out :start start)
                   (return))))))))

(defun %public-package-text (text &optional server)
  "Rewrite SERVER's private package name in protocol-visible TEXT."
  (let* ((server (or server (%current-server-binding)))
         (package (and server (server-initial-package server)))
         (private-name (and (%project-package-p package)
                            (package-name package))))
    (if (and private-name (stringp text))
        (%replace-substrings text private-name
                             (%public-package-name package server))
        text)))

(defun %resolve-package-for-server (server name)
  "Resolve public package NAME in SERVER's namespace."
  (cond
    ((and server
          (server-project-root server)
          (stringp name)
          (or (string-equal name "COMMON-LISP-USER")
              (string-equal name "CL-USER")))
     (server-initial-package server))
    ((stringp name)
     (%find-package-loose name))
    (t nil)))

(defun %project-root-fingerprint (project-root)
  "Return an opaque stable fingerprint for PROJECT-ROOT, or NIL.

The protocol uses this as a same-project proof without exposing the
canonical project path in public ping output."
  (when (and (stringp project-root) (plusp (length project-root)))
    (clpm.crypto.sha256:bytes-to-hex
     (clpm.crypto.sha256:sha256 project-root))))

(defun %server-project-fingerprint (server)
  (%project-root-fingerprint (and server (server-project-root server))))

(defparameter +max-log-bytes+ (* 10 1024 1024)
  "Rotate `.clpm/repl.log' once it grows past this many bytes.")

(defstruct event-log
  (path "" :type string)
  (stream nil)
  (bytes-written 0 :type unsigned-byte)
  (mutex (clpm.repl.compat:make-mutex :name "clpm.repl.log")))

(defun %rfc3339-now ()
  "Return the current time as an RFC-3339 / ISO-8601 string in UTC, e.g.
`2026-05-19T18:23:45Z'. Second precision is good enough for an event log."
  (multiple-value-bind (sec min hr day mon yr)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            yr mon day hr min sec)))

(defun %open-event-log (path)
  "Open PATH for append, returning an `event-log' struct or NIL on failure.
Pre-loads the current size so rotation tracking starts in the right place."
  (handler-case
      (let* ((stream (open path :direction :output
                                :if-exists :append
                                :if-does-not-exist :create
                                :external-format :utf-8))
             (existing (ignore-errors (file-length stream))))
        (make-event-log :path path
                        :stream stream
                        :bytes-written (or existing 0)))
    (error () nil)))

(defun %close-event-log (log)
  (when (and log (event-log-stream log))
    (ignore-errors (close (event-log-stream log)))
    (setf (event-log-stream log) nil)))

(defun %rotate-event-log (log)
  "Close the current stream, rename PATH -> PATH.1, reopen at PATH. Called
with LOG-MUTEX held."
  (let ((path (event-log-path log)))
    (ignore-errors (close (event-log-stream log)))
    (let ((rotated (concatenate 'string path ".1")))
      (ignore-errors (delete-file rotated))
      (ignore-errors (rename-file path rotated)))
    (let ((s (handler-case
                 (open path :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
               (error () nil))))
      (setf (event-log-stream log) s
            (event-log-bytes-written log) 0))))

(defun %log-event (log event &rest plist)
  "Append one JSON-line event to LOG (an event-log or NIL). PLIST is a
property list of additional fields (symbol keys are downcased; string keys
pass through). Safe to call concurrently from any thread; safe when LOG
is NIL."
  (when (and log (event-log-stream log))
    (let* ((extra (loop for (k v) on plist by #'cddr
                        collect (cons (cond
                                        ((stringp k) k)
                                        ((symbolp k) (string-downcase (symbol-name k)))
                                        (t (princ-to-string k)))
                                      v)))
           (entry (list :object
                        (append (list (cons "ts" (%rfc3339-now))
                                      (cons "event" event))
                                extra)))
           (line (with-output-to-string (b)
                   (clpm.io.json:write-json entry b)
                   (write-char #\Newline b))))
      (clpm.repl.compat:with-mutex ((event-log-mutex log))
        (when (event-log-stream log)
          (handler-case
              (progn
                (write-string line (event-log-stream log))
                (force-output (event-log-stream log)))
            (error () nil))
          (incf (event-log-bytes-written log) (length line))
          (when (> (event-log-bytes-written log) +max-log-bytes+)
            (%rotate-event-log log)))))))

(defvar *server* nil
  "Current server instance for the executing daemon thread.

This is a special binding, not process identity. A host Lisp may run more
than one project daemon, so every accept/connection/worker thread must bind
it to the server that owns that thread.")

(defstruct local-trace-entry
  original
  servers)

(defvar *local-trace-registry* (make-hash-table :test 'eq)
  "Process-global wrapper registry keyed by function symbol.

Each entry may be enabled for several SERVER instances. The installed
wrapper emits only when the dynamically-bound `*server*' is one of those
servers, so foreground daemons hosted in one Lisp image do not observe each
other's trace state.")

(defvar *local-trace-registry-mutex*
  (clpm.repl.compat:make-mutex :name "clpm.repl.local-trace"))

(define-condition user-interrupt () ()
  (:documentation "Signaled inside the worker thread when the client closes
its connection or sends an explicit `interrupt' request."))

(define-condition resource-exhausted ()
  ((kind :initarg :kind :reader resource-exhausted-kind)
   (limit :initarg :limit :reader resource-exhausted-limit)
   (observed :initarg :observed :reader resource-exhausted-observed))
  (:documentation "Signaled inside the worker by the cap-watcher timer
when an eval crosses its `max-real-ms' or `max-cons-bytes' budget.
KIND is one of :real-ms or :cons-bytes."))

;;; --------------------------------------------------------------------------
;;; Worker thread
;;; --------------------------------------------------------------------------

(defstruct repl-history
  star star-star star-star-star
  plus plus-plus plus-plus-plus
  slash slash-slash slash-slash-slash)

(defstruct worker
  "One eval thread. Most clients only ever touch the worker named
\"default\"; named / concurrent workers (#170-#173) give the LLM a
sandboxed slot to run an experiment in without polluting the main
session's package, history, or redefinition log.

STATE transitions are advisory (lock-free): the worker thread writes,
the connection threads read for `list-workers'. The default worker is
created on first eval and never removed; concurrent workers (one-shot)
are destroyed automatically when their eval completes.

RESTARTED? is set by `%ensure-worker' when reviving a worker whose
thread died unexpectedly. The next eval response surfaces this as a
`worker_restarted: true' field so clients can see that history,
package, and redefinition state may have been lost."
  (name "default" :type string)
  server
  mailbox
  thread
  (state :idle :type keyword)
  (package (find-package "COMMON-LISP-USER"))
  last-eval-id
  (started-at (get-universal-time) :type integer)
  (last-active-at (get-universal-time) :type integer)
  (history (make-repl-history))
  (redefinitions (make-hash-table :test 'equal))
  current-job
  (concurrent? nil :type boolean)
  (restarted? nil :type boolean))

(defvar *current-worker* nil
  "Worker struct of the currently-executing eval, bound by `%worker-loop'.
Read by `%eval-one' and `%record-redefinition' so per-worker state
lands in the right bucket.")

(defparameter +default-worker-name+ "default")

(defstruct eval-job
  form
  package-override
  ;; Plist of v2 opt-ins parsed out of the request params. Examples:
  ;;   :stream t       -- emit `event:stdout' / `event:stderr' chunks
  ;;   :query-interactive t -- support bidirectional `query' for reads
  ;;   :debug t        -- pause in the debugger on error (see #111)
  ;;   :print-length N -- bind *print-length* for the duration
  ;;   ...
  options
  ;; Request context used for streaming events and (in :debug) for
  ;; pulling debugger actions out of the wire.
  ctx
  ;; Mailbox the connection thread pushes `query-response' values into.
  query-mailbox
  ;; Mailbox the connection thread pushes `debug-*' actions into when
  ;; the worker has entered an interactive debug session.
  debug-mailbox
  ;; The currently-active debug-session (if any); set by the handler-bind
  ;; that enters the debugger, cleared when the session unwinds.
  debug-session
  result-mailbox
  thread)

(defstruct debug-session
  "Live, non-unwound debugging state. CONDITION and RESTARTS are captured
inside the handler-bind so invoke-restart still has a valid target.
FRAMES is a snapshot of the call stack at entry; index 0 is the most
recent call (closest to the error)."
  ;; Server-owned identity. NIL until the session is registered.
  id
  worker-name
  (entered-at (get-universal-time) :type integer)
  condition
  restarts
  ;; A vector of frame objects -- sb-di:frame on SBCL, or NIL on impls
  ;; that don't expose them.
  frames
  ;; Pre-rendered JSON describing the condition (same shape %condition-json
  ;; produces). Cached so each debug-eval-in-frame event doesn't recompute.
  json-condition
  ;; The eval-job this session belongs to; the action mailbox lives there.
  job)

(defun %register-debug-session (server session)
  "Give SESSION a server-owned id and make it addressable by later RPCs."
  (clpm.repl.compat:with-mutex ((server-debug-sessions-mutex server))
    (let ((id (incf (server-debug-session-counter server))))
      (setf (debug-session-id session) id
            (gethash id (server-debug-sessions server)) session)
      id)))

(defun %unregister-debug-session (server session)
  "Remove SESSION from the server-owned debug-session table."
  (let ((id (and session (debug-session-id session))))
    (when id
      (clpm.repl.compat:with-mutex
          ((server-debug-sessions-mutex server))
        (remhash id (server-debug-sessions server))))))

(defun %find-debug-session (server id)
  "Return the active debug session named by integer ID, or NIL."
  (when (integerp id)
    (clpm.repl.compat:with-mutex
        ((server-debug-sessions-mutex server))
      (gethash id (server-debug-sessions server)))))

(defun %all-debug-sessions (server)
  "Snapshot every active server-owned debug session."
  (clpm.repl.compat:with-mutex
      ((server-debug-sessions-mutex server))
    (loop for session being the hash-values of (server-debug-sessions server)
          collect session)))

(defun %abort-debug-session (server session reason)
  "Ask SESSION to unwind if it is still parked in the debugger loop."
  (let* ((job (and session (debug-session-job session)))
         (mailbox (and job (eval-job-debug-mailbox job))))
    (when mailbox
      (%log-event (server-event-log server) "debug-session-abort"
                  "session" (debug-session-id session)
                  "worker" (debug-session-worker-name session)
                  "reason" reason)
      (clpm.repl.compat:send-message mailbox (list :abort nil)))))

(defun %debug-session-json (session)
  (%json-object
   "session" (debug-session-id session)
   "worker" (debug-session-worker-name session)
   "entered_at_unix" (debug-session-entered-at session)
   "condition" (debug-session-json-condition session)))

(defstruct eval-result
  code                  ; nil on success; "eval-error" / "reader-error" / "interrupted"
  ;; Each value SBCL returned, prin1'd to a string. The first is the
  ;; "primary value"; the v1 `value' field aliases this for back-compat.
  (values nil :type list)
  output
  error-output
  package
  elapsed-ms
  conditions             ; list of (:object ...) entries for ERROR conditions
  signaled-conditions    ; list for handled / signaled-but-not-errored ones
  truncated?
  redefined
  history                ; alist (("*" . val-string) ("**" . ...) ("/" . arr) ...)
  ;; Records of declarative --handler specs that matched the condition's
  ;; type but whose restart wasn't available. Without this, "no handler
  ;; matched" and "handler matched but its restart isn't bound here" are
  ;; observationally identical, which makes --handler impossible to debug.
  handler-attempts)

(defun %list-restarts ()
  "Bare list of restart names (string form). Kept for v1 compatibility."
  (mapcar (lambda (r) (string (restart-name r))) (compute-restarts)))

(defun %restart-arity (restart)
  "Best-effort arity for the restart's body lambda-list (how many
values INVOKE-RESTART will consume). Returns either a non-negative
INTEGER -- meaning \"exactly N positional arguments\" -- or the
keyword :VARIADIC, meaning \"SBCL exposes the restart through a &REST
wrapper and the true body arity isn't observable, so any number is
plausible\" (this happens for `restart-case' clauses with multiple
positional parameters).

The :interactive thunk is a distinct contract -- it takes zero args
and returns a list -- so its arity is the wrong observation to
report here. We probe the restart's body function via sb-kernel
internals, falling back to standard-restart conventions."
  (let* ((body-fn (handler-case
                      (sb-kernel::restart-function restart)
                    (error () nil)))
         (name (and (restart-name restart)
                    (symbol-name (restart-name restart))))
         (lambda-arity
           (lambda (lambda-list)
             (let ((required 0)
                   (variadic nil))
               (dolist (s lambda-list)
                 (cond
                   ((member s '(&optional &key &aux))
                    (return))
                   ((eq s '&rest)
                    (setf variadic t)
                    (return))
                   ((symbolp s) (incf required))))
               (if variadic :variadic required)))))
    (cond
      (body-fn
       (let ((ll (handler-case (sb-introspect:function-lambda-list body-fn)
                   (error () nil))))
         (if ll (funcall lambda-arity ll) 0)))
      ((member name '("USE-VALUE" "STORE-VALUE" "RETURN-VALUE")
               :test #'string=) 1)
      (t 0))))

(defun %restart-json (restart)
  "Rich serialization for one restart object."
  (let ((name (restart-name restart)))
    (list :object
          (list (cons "name" (and name (symbol-name name)))
                (cons "report" (handler-case
                                   (with-output-to-string (s)
                                     (let ((rf (sb-kernel::restart-report-function restart)))
                                       (cond
                                         (rf (funcall rf s))
                                         (name (format s "~A" name))
                                         (t (format s "<unnamed restart>")))))
                                 (error () "<restart report failed>")))
                (cons "interactive"
                      (and (sb-kernel::restart-interactive-function restart) t))
                (cons "args_arity"
                      ;; INTEGER for an exact required-arg count, or
                      ;; the string "variadic" when SBCL's restart
                      ;; wrapper hides a multi-arg restart-case clause
                      ;; behind &REST. JSON has no way to express that
                      ;; gracefully other than as a tag string.
                      (let ((a (handler-case (%restart-arity restart)
                                 (error () 0))))
                        (case a
                          (:variadic "variadic")
                          (t a))))))))

(defun %condition-slot-values (condition)
  "Capture the slot values of CONDITION as a list of (slot-name . prin1)
pairs. Uses MOP to find slots; falls back to an empty list when the
implementation doesn't expose them."
  (handler-case
      (let* ((class (class-of condition))
             (slots
               #+sbcl (sb-mop:class-slots class)
               #-sbcl nil))
        (loop for slot in slots
              for slot-name = #+sbcl (sb-mop:slot-definition-name slot)
                              #-sbcl nil
              when (and slot-name (slot-boundp condition slot-name))
              collect (cons (symbol-name slot-name)
                            (%safe-prin1 (slot-value condition slot-name)))))
    (error () nil)))

#+sbcl
(defun %frame-vars (frame)
  "Best-effort capture of FRAME's lexical variables as an alist of
(name . prin1'd value). May return NIL when SBCL has optimized vars
into the void."
  (handler-case
      (let ((debug-fun (sb-di:frame-debug-fun frame))
            (vars '()))
        (when debug-fun
          (sb-di:do-debug-fun-vars (v debug-fun)
            (let ((name (sb-di:debug-var-symbol v)))
              (when name
                (push (cons (symbol-name name)
                            (handler-case
                                (%safe-prin1
                                 (sb-di:debug-var-value v frame))
                              (error () "<unavailable>")))
                      vars)))))
        (nreverse vars))
    (error () nil)))

#+sbcl
(defun %frame-name (frame)
  (handler-case
      (let* ((debug-fun (sb-di:frame-debug-fun frame))
             (name (sb-di:debug-fun-name debug-fun)))
        (cond
          ((null name) "?")
          ((symbolp name) (symbol-name name))
          (t (princ-to-string name))))
    (error () "?")))

#+sbcl
(defun %frame-args (frame)
  "Best-effort prin1'd argument list. The lambda list returned by
sb-di:debug-fun-lambda-list is a sequence whose elements are either
debug-var objects (for required args) or keyword-tagged lists (for
optional / keyword / rest). We render every debug-var we can value, in
order, and stop at the first non-required element."
  (handler-case
      (let* ((debug-fun (sb-di:frame-debug-fun frame))
             (lambda-list (sb-di:debug-fun-lambda-list debug-fun)))
        (loop for item in lambda-list
              while (sb-di:debug-var-p item)
              collect (handler-case
                          (%safe-prin1
                           (sb-di:debug-var-value item frame))
                        (error () "<unavailable>"))))
    (error () nil)))

#+sbcl
(defun %frame-source (frame)
  "Source location \"file:line\" for the frame, or NIL if SBCL didn't
record one."
  (handler-case
      (let* ((debug-fun (sb-di:frame-debug-fun frame))
             (source (and debug-fun
                          (sb-di:debug-fun-name debug-fun)
                          (handler-case
                              (sb-introspect:find-definition-source
                               (fdefinition (sb-di:debug-fun-name debug-fun)))
                            (error () nil))))
             (pathname (and source
                            (sb-introspect:definition-source-pathname source)))
             (form-path (and source
                             (sb-introspect:definition-source-form-path source))))
        (when pathname
          (format nil "~A~@[:~A~]"
                  (namestring pathname)
                  (and (consp form-path) (first form-path)))))
    (error () nil)))

#+sbcl
(defun %frames-json (frames &key (max +max-backtrace-frames+))
  "Render FRAMES as structured JSON with indices that refer back to FRAMES."
  (loop for frame in frames
        for i from 0
        while (< i max)
        collect (list :object
                      (list (cons "i" i)
                            (cons "name" (%frame-name frame))
                            (cons "args" (%json-array
                                          (mapcar #'identity
                                                  (or (%frame-args frame) '()))))
                            (cons "source" (%frame-source frame))
                            (cons "vars"
                                  (list :object (%frame-vars frame)))))))

#+sbcl
(defun %structured-backtrace (&key (max +max-backtrace-frames+))
  "Walk the SBCL call stack into a list of frame plists. Bounded by MAX
so a runaway condition can't drown the response."
  (handler-case
      (let ((frames '())
            (i 0))
        (sb-debug:map-backtrace
         (lambda (frame)
           (when (< i max)
             (push (list :object
                         (list (cons "i" i)
                               (cons "name" (%frame-name frame))
                               (cons "args" (%json-array
                                             (mapcar #'identity
                                                     (or (%frame-args frame) '()))))
                               (cons "source" (%frame-source frame))
                               (cons "vars"
                                     (list :object (%frame-vars frame)))))
                   frames)
             (incf i))))
        (nreverse frames))
    (error () nil)))

(defun %condition-json (condition &key include-backtrace frames)
  "Rich JSON for CONDITION: type, message, report, slot values, full
restart objects, and (when INCLUDE-BACKTRACE) a structured frame walk
with names, args, source locations, and locals. Falls back to plain
strings on every internal failure -- a buggy print-object on a slot
must not block the eval response."
  (let* ((type (string (type-of condition)))
         (msg (%public-package-text
               (handler-case (princ-to-string condition)
                 (error () "<condition message unavailable>"))))
         (report (%public-package-text
                  (handler-case
                      (with-output-to-string (s)
                        (let ((*print-pretty* t))
                          (format s "~A" condition)))
                    (error () msg))))
         (slot-values (%condition-slot-values condition))
         (restarts (mapcar #'%restart-json (compute-restarts condition)))
         (entries
           (list (cons "type" type)
                 (cons "message" msg)
                 (cons "report" report)
                 (cons "slot_values" (list :object slot-values))
                 (cons "restarts" (%json-array restarts)))))
    (when include-backtrace
      (let ((frames (or #+sbcl (and frames (%frames-json frames))
                        #+sbcl (%structured-backtrace)
                        ;; Generic fallback: princ each compat-layer frame.
                        (handler-case
                            (let* ((all (clpm.repl.compat:list-backtrace))
                                   (head (subseq all 0 (min (length all)
                                                            +max-backtrace-frames+))))
                              (loop for f in head
                                    for i from 0
                                    collect (list :object
                                                  (list (cons "i" i)
                                                        (cons "name"
                                                              (princ-to-string f))))))
                          (error () nil)))))
        (setf entries
              (append entries
                      (list (cons "backtrace" (%json-array frames)))))))
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
                       ((symbolp name)
                        (%public-package-name
                         (or (symbol-package name) package)))
                       (t (%public-package-name package))))
           (key (list kind name-str pkg-name))
           (record (list (cons "kind" kind)
                         (cons "name" name-str)
                         (cons "package" pkg-name)
                         (cons "form" (let ((*print-pretty* nil))
                                        (prin1-to-string form))))))
      (cond
        (*current-worker*
         (setf (gethash key (worker-redefinitions *current-worker*))
               record))
        (*server*
         (setf (gethash key (server-redefinitions *server*))
               record)))
      record)))

(defun %find-package-loose (name)
  "Look up a package by NAME, accepting any case. NAME is a string."
  (or (find-package name)
      (find-package (string-upcase name))
      (find-package (string-downcase name))))

(defun %safe-prin1 (value &key (print-length nil print-length-p)
                                (print-level nil print-level-p)
                                (print-circle nil print-circle-p)
                                (print-radix nil print-radix-p)
                                (print-base nil print-base-p)
                                (print-pretty nil print-pretty-p))
  "Defensive `prin1-to-string'. If the value's `print-object' method
errors, return a fallback `#<unprintable ...>' string instead of letting
the error blast through the eval response."
  (handler-case
      (%public-package-text
       (let ((*print-length* (if print-length-p print-length (or *print-length* 200)))
             (*print-level*  (if print-level-p print-level (or *print-level* 8)))
             (*print-circle* (if print-circle-p print-circle t))
             (*print-radix*  (if print-radix-p print-radix *print-radix*))
             (*print-base*   (if print-base-p print-base *print-base*))
             (*print-pretty* (if print-pretty-p print-pretty nil)))
         (prin1-to-string value)))
    (error (c)
      (%public-package-text
       (format nil "#<unprintable ~A: ~A>"
               (handler-case (type-of value) (error () "?"))
               (handler-case (princ-to-string c) (error () "?")))))))

(defparameter +history-symbols+ '("*" "**" "***" "+" "++" "+++" "/" "//" "///")
  "REPL history bindings updated after every eval. CL semantics: `*' is the
primary value of the last eval, `+' is the last form, `/' is the values
list of the last eval; `**' / `++' / `//' are the prior, `***' / `+++' /
`///' the one before that.")

(defun %history-symbols ()
  (let ((pkg (find-package "COMMON-LISP")))
    (loop for name in +history-symbols+
          collect (find-symbol name pkg))))

(defun %history-values (history)
  (list (repl-history-star history)
        (repl-history-star-star history)
        (repl-history-star-star-star history)
        (repl-history-plus history)
        (repl-history-plus-plus history)
        (repl-history-plus-plus-plus history)
        (repl-history-slash history)
        (repl-history-slash-slash history)
        (repl-history-slash-slash-slash history)))

(defmacro %with-repl-history ((history) &body body)
  `(if ,history
       (progv (%history-symbols) (%history-values ,history)
         ,@body)
       (progn ,@body)))

(defun %read-history-snapshot (history &optional package)
  "Capture WORKER-local history as a JSON-friendly alist."
  (let ((*package* (or package *package*)))
    (loop for name in +history-symbols+
          for value in (%history-values history)
          collect (cons name (%safe-prin1 value)))))

(defun %update-history! (history last-form last-values)
  "Shift the worker-local REPL history bindings."
  (let ((primary (if (consp last-values) (first last-values) nil)))
    (setf (repl-history-star-star-star history)
          (repl-history-star-star history)
          (repl-history-star-star history)
          (repl-history-star history)
          (repl-history-star history)
          primary
          (repl-history-plus-plus-plus history)
          (repl-history-plus-plus history)
          (repl-history-plus-plus history)
          (repl-history-plus history)
          (repl-history-plus history)
          last-form
          (repl-history-slash-slash-slash history)
          (repl-history-slash-slash history)
          (repl-history-slash-slash history)
          (repl-history-slash history)
          (repl-history-slash history)
          (copy-list last-values))))

(defmacro %with-print-options (options &body body)
  "Bind *print-*' variables for the duration of BODY according to OPTIONS
(a plist). Unbound options inherit the surrounding dynamic state."
  (let ((opt (gensym "OPT")))
    `(let* ((,opt ,options)
            (*print-length* (getf ,opt :print-length *print-length*))
            (*print-level*  (getf ,opt :print-level  *print-level*))
            (*print-circle* (getf ,opt :print-circle *print-circle*))
            (*print-radix*  (getf ,opt :print-radix  *print-radix*))
            (*print-base*   (getf ,opt :print-base   *print-base*))
            (*print-pretty* (getf ,opt :print-pretty *print-pretty*)))
       ,@body)))

(defstruct handler-spec
  "One entry from `eval --handlers'. TYPE is a CL type specifier;
RESTART is a symbol naming the restart to invoke; ARGS is a list of
already-evaluated argument values."
  type restart args)

(defun %parse-handler-specs (raw package)
  "Translate RAW (a list of JSON objects {type, restart, args}) into a
list of handler-spec structs. ARGS forms are read+evaluated at parse
time so the handler doesn't have to do work when a condition arrives."
  (when raw
    (let ((*package* (or package *package*)))
      (loop for entry in raw
            for type-text = (%json-getf entry "type")
            for restart-text = (%json-getf entry "restart")
            for args-array = (%json-getf entry "args")
            for arg-forms = (and (consp args-array) (eq (car args-array) :array)
                                 (cadr args-array))
            for parsed-type = (handler-case
                                  (and (stringp type-text)
                                       (read-from-string type-text))
                                (error () nil))
            for parsed-restart = (handler-case
                                     (and (stringp restart-text)
                                          (read-from-string restart-text))
                                   (error () nil))
            for parsed-args = (handler-case
                                  (loop for s in arg-forms
                                        collect (eval (read-from-string
                                                       (princ-to-string s))))
                                (error () nil))
            when (and parsed-type parsed-restart)
            collect (make-handler-spec :type parsed-type
                                       :restart parsed-restart
                                       :args parsed-args)))))

(defun %try-declarative-handler (condition specs attempts-cell)
  "Walk SPECS; for the first one whose type matches CONDITION, invoke
its restart with its args. Returns T if a handler was invoked (which
transfers control and doesn't actually return), else NIL.

ATTEMPTS-CELL is a one-element list whose CAR accumulates a record for
each spec that matched by type but couldn't fire because its restart
wasn't bound for this condition. Without this observation, a misspelled
restart name and a non-matching type are indistinguishable from the
caller's seat."
  (dolist (spec specs nil)
    (when (typep condition (handler-spec-type spec))
      (let ((r (find-restart (handler-spec-restart spec) condition)))
        (cond
          (r
           (apply #'invoke-restart r (handler-spec-args spec))
           ;; invoke-restart unwinds; we never reach here.
           (return t))
          (t
           (push (%handler-attempt-json spec condition)
                 (car attempts-cell))))))))

(defun %handler-attempt-json (spec condition)
  "Build the JSON record for a `--handler' spec that matched CONDITION
by type but whose restart wasn't available. Lists the alternatives the
user *could* have invoked so the next attempt is informed."
  (let* ((type-name (handler-case (string (handler-spec-type spec))
                      (error () "?")))
         (restart-name (handler-case (string (handler-spec-restart spec))
                         (error () "?")))
         (available (handler-case
                        (mapcar (lambda (r)
                                  (let ((rn (restart-name r)))
                                    (if rn (string rn) "")))
                                (compute-restarts condition))
                      (error () nil))))
    (list :object
          (list (cons "type" type-name)
                (cons "restart" restart-name)
                (cons "outcome" "matched-no-restart")
                (cons "available_restarts" (%json-array available))))))

(defun %capture-error-snapshot (condition)
  "Snapshot CONDITION as JSON while the stack is still in place. If
serialization itself fails, return a minimal fallback object so the
caller still gets *something* to attach to the eval result."
  (handler-case
      (%condition-json condition :include-backtrace t)
    (error ()
      (list :object
            (list (cons "type" (handler-case (string (type-of condition))
                                 (error () "?")))
                  (cons "message"
                        (%public-package-text
                         (handler-case (princ-to-string condition)
                           (error () "<unprintable>")))))))))

#+sbcl
(defun %capture-frames (&key (max +max-backtrace-frames+))
  "Walk SBCL's stack and return a list of live frame objects (length <=
MAX). The frames stay valid only inside the handler-bind that creates
them; once the stack unwinds they become invalid."
  (let ((frames '())
        (i 0))
    (handler-case
        (sb-debug:map-backtrace
         (lambda (f)
           (when (< i max)
             (push f frames)
             (incf i))))
      (error () nil))
    (nreverse frames)))

#-sbcl
(defun %capture-frames (&key max)
  (declare (ignore max))
  '())

(defun %restart-by-name (session name)
  "Look up a restart in SESSION by its symbol-name (case-insensitive)."
  (find name (debug-session-restarts session)
        :test (lambda (s r)
                (let ((rn (restart-name r)))
                  (and rn (string-equal s (symbol-name rn)))))))

(defun %read-debug-form (form-text package)
  "Read FORM-TEXT for a debugger sub-eval. PACKAGE is used so the parsed
symbols resolve as the user expects."
  (let ((*package* (or package *package*)))
    (%read-form form-text)))

(defun %eval-in-frame (session frame-index form-text)
  "Evaluate FORM-TEXT in the lexical context of FRAME-INDEX.

On SBCL, `sb-di:eval-in-frame' is the semantic operation we want: it sees
the frame's real lexical variables, local functions, symbol macros, and
implementation-supported debug environment instead of a guessed LET wrapper."
  (let* ((frames (debug-session-frames session))
         (frame (and frames
                     (integerp frame-index)
                     (<= 0 frame-index)
                     (< frame-index (length frames))
                     (nth frame-index frames))))
    (cond
      ((null frame)
       (list (cons "error_output"
                   (format nil "no frame ~A (have ~A)"
                           frame-index (length frames)))))
      (t
       (let* ((pkg (or (and *server* (server-current-package *server*))
                       (find-package "COMMON-LISP-USER"))))
         (handler-case
             (let* ((parsed-form (%read-debug-form form-text pkg))
                    (out (make-string-output-stream))
                    (err (make-string-output-stream))
                    (values (let ((*standard-output* out)
                                  (*error-output* err)
                                  (*package* pkg))
                              (multiple-value-list
                               #+sbcl (sb-di:eval-in-frame frame parsed-form)
                               #-sbcl (eval parsed-form)))))
               (list (cons "values" (%json-array
                                     (mapcar #'%safe-prin1 values)))
                     (cons "value" (%safe-prin1 (first values)))
                     (cons "output" (get-output-stream-string out))
                     (cons "error_output" (get-output-stream-string err))))
           (error (c)
             (list (cons "value" nil)
                   (cons "error_output"
                         (princ-to-string c))))))))))

#-sbcl
(defun %eval-in-frame (session frame-index form-text)
  (declare (ignore session frame-index form-text))
  (list (cons "error_output" "eval-in-frame is SBCL-only")))

(defun %send-debug-action-result (mailbox payload)
  (when mailbox
    (clpm.repl.compat:send-message mailbox (list :result payload))))

(defun %send-debug-action-error (mailbox code message)
  (when mailbox
    (clpm.repl.compat:send-message mailbox
                                          (list :error code message))))

(defun %debug-action-result (session outcome)
  (%json-object "session" (debug-session-id session)
                "worker" (debug-session-worker-name session)
                "outcome" outcome))

(defun %eval-debug-restart-args (args-forms package)
  "Read and evaluate restart argument forms without consuming the debug stop.
Returns (values ARGS NIL) on success, or (values NIL MESSAGE) on failure."
  (handler-case
      (let ((*package* (or package *package*)))
        (values
         (loop for form-text in args-forms
               collect (eval (%read-debug-form form-text *package*)))
         nil))
    (error (c)
      (values nil (format nil "arg eval failed: ~A" c)))))

(defun %enter-debugger (condition job)
  "Build a debug session for CONDITION + the current restart chain and
loop processing debug-* actions until one of them either invokes a
restart (unwinds) or returns NIL (we let the error propagate)."
  (let* ((ctx (eval-job-ctx job))
         (worker-name (or (and *current-worker*
                               (worker-name *current-worker*))
                          +default-worker-name+))
         (frames (%capture-frames))
         (restarts (compute-restarts condition))
         (json-condition (handler-case
                             (%condition-json condition
                                             :include-backtrace t
                                             :frames frames)
                           (error () (%capture-error-snapshot condition))))
         (session (make-debug-session
                   :worker-name worker-name
                   :condition condition
                   :restarts restarts
                   :frames frames
                   :json-condition json-condition
                   :job job)))
    (when *server*
      (%register-debug-session *server* session))
    (setf (eval-job-debug-session job) session)
    (unwind-protect
         (progn
           (when ctx
             (%emit-event ctx "debugger-entered"
                          "session" (debug-session-id session)
                          "worker" worker-name
                          "condition" json-condition))
           (loop
             (let ((action (clpm.repl.compat:receive-message
                            (eval-job-debug-mailbox job))))
               (case (first action)
                 (:invoke-restart
                  (destructuring-bind (name args-forms &optional reply-box)
                      (rest action)
                    (let ((restart (%restart-by-name session name)))
                      (cond
                        ((null restart)
                         (let ((message (format nil "no restart named ~A"
                                                name)))
                           (%send-debug-action-error reply-box
                                                     "eval-error"
                                                     message)
                           (when ctx
                             (%emit-event ctx "debug-error"
                                          "message" message))))
                        (t
                         (multiple-value-bind (args arg-error)
                             (%eval-debug-restart-args
                              args-forms
                              (and *server* (server-current-package *server*)))
                           (cond
                             (arg-error
                              (%send-debug-action-error reply-box
                                                        "eval-error"
                                                        arg-error)
                              (when ctx
                                (%emit-event ctx "debug-error"
                                             "message" arg-error)))
                             (t
                              (%send-debug-action-result
                               reply-box
                               (%debug-action-result
                                session "restart-invoked"))
                              (apply #'invoke-restart restart args)
                              ;; invoke-restart unwinds; we never get here.
                              (return-from %enter-debugger nil)))))))))
                 (:eval-in-frame
                  (destructuring-bind (index form &optional reply-box)
                      (rest action)
                    (let ((result (%eval-in-frame session index form)))
                      (cond
                        (reply-box
                         (%send-debug-action-result
                          reply-box
                          (list :object result)))
                        (ctx
                         (apply #'%emit-event ctx "frame-eval-result"
                                (loop for (k . v) in result
                                      append (list k v))))))))
                 (:abort
                  (%send-debug-action-result
                   (second action)
                   (%debug-action-result session "aborted"))
                  ;; Let the original condition unwind through the outer
                  ;; handler-case so finish(:err-condition) takes over.
                  (return-from %enter-debugger nil))
                 (:continue
                  (let ((reply-box (second action))
                        (r (find-restart 'continue condition)))
                    (cond
                      (r
                       (%send-debug-action-result
                        reply-box
                        (%debug-action-result session "continued"))
                       (invoke-restart r))
                      (t
                       (%send-debug-action-error
                        reply-box "eval-error"
                        "no CONTINUE restart available")
                       ;; If there's no CONTINUE restart, fall through and keep
                       ;; waiting for further actions.
                       (when ctx
                         (%emit-event
                          ctx "debug-error"
                          "message"
                          "no CONTINUE restart available"))))))))))
      (setf (eval-job-debug-session job) nil)
      (when *server*
        (%unregister-debug-session *server* session)))))

(defun %eval-one (form-text &key package-override options job)
  "Evaluate FORM-TEXT inside the worker. Returns an eval-result struct.

OPTIONS is a plist of v2 toggles parsed from the request:
  :stream t            -- bind *standard-output* / *error-output* to a
                          streaming Gray stream so chunks are emitted as
                          `event:stdout' / `event:stderr' to JOB's ctx.
  :query-interactive t -- bind *standard-input* / *query-io* to a stream
                          that issues a server `event:query' on read.
  :print-length / :print-level / :print-circle / :print-radix /
  :print-base / :print-pretty -- per-eval print-control bindings.

JOB carries the request context for streaming and (eventually) debugger
sessions."
  (let* ((sink (make-bounded-sink))
         (ctx (and job (eval-job-ctx job)))
         (stream? (and ctx (getf options :stream)))
         (query? (and ctx (getf options :query-interactive)))
         (out-stream
           (cond
             (stream?
              #+sbcl (make-instance 'streaming-output-stream
                                    :ctx ctx :channel "stdout"
                                    :sink sink
                                    :flush-bytes +stream-flush-min-bytes+)
              #-sbcl (%make-capture-stream sink))
             (t (%make-capture-stream sink))))
         (err-stream
           (cond
             (stream?
              #+sbcl (make-instance 'streaming-output-stream
                                    :ctx ctx :channel "stderr"
                                    :sink sink
                                    :flush-bytes +stream-flush-min-bytes+)
              #-sbcl (%make-capture-stream sink))
             (t (%make-capture-stream sink))))
         (in-stream
           (cond
             (query?
              #+sbcl (make-instance 'query-input-stream
                                    :ctx ctx
                                    :mailbox (eval-job-query-mailbox job))
              #-sbcl (make-string-input-stream ""))
             (t (make-string-input-stream ""))))
         (start (get-internal-real-time))
         (form nil)
         (override-pkg (and package-override
                            (%resolve-package-for-server *server*
                                                         package-override)))
         (package (or override-pkg
                      (and *current-worker* (worker-package *current-worker*))
                      (and *server* (server-current-package *server*))
                      (find-package "COMMON-LISP-USER")))
         (history (and *current-worker* (worker-history *current-worker*)))
         (returned-values '())
         (code nil)
         (conditions '())
         (signaled '())
         (redefined nil)
         (handler-attempts-cell (list nil))
         ;; Live snapshot of the failing condition's restarts + backtrace,
         ;; captured by the handler-bind below *before* the stack unwinds
         ;; into the handler-case error handler. Without this, %condition-json
         ;; runs after unwind and only sees the toplevel ABORT.
         (error-snapshot nil))
    (when (and package-override (null override-pkg))
      (let ((c (make-condition 'simple-error
                               :format-control "No such package: ~A"
                               :format-arguments (list package-override))))
        (setf code "reader-error")
        (push (%condition-json c) conditions)
        (return-from %eval-one
          (make-eval-result
           :code code
           :values nil
           :output ""
           :error-output ""
           :package (%public-package-name
                     (or *package* (find-package "COMMON-LISP-USER")))
           :elapsed-ms 0
           :conditions (nreverse conditions)
           :signaled-conditions nil
           :truncated? nil
           :redefined nil
           :history nil))))
    (labels ((finish (&key err-condition)
               (when err-condition
                 (setf code "eval-error")
                 ;; Prefer the pre-unwind snapshot the handler-bind captured;
                 ;; only fall back to a fresh compute if we somehow don't
                 ;; have one (e.g., an unwind-triggered error from the outer
                 ;; let-bindings).
                 (push (or error-snapshot
                           (%condition-json err-condition :include-backtrace t))
                       conditions))
               (let ((value-strings
                       (and (null code)
                            (%with-print-options options
                              (let ((*package* package))
                                (mapcar (lambda (v) (%safe-prin1 v))
                                        returned-values)))))
                     (history-snap
                       (and (null code)
                            history
                            (handler-case (%read-history-snapshot history package)
                              (error () nil)))))
                 (make-eval-result
                  :code code
                  :values value-strings
                  :output (%capture-text out-stream sink)
                  :error-output (%capture-text err-stream sink)
                  :package (%public-package-name package)
                  :elapsed-ms (round (* 1000.0
                                        (/ (- (get-internal-real-time) start)
                                           internal-time-units-per-second)))
                  :conditions (nreverse conditions)
                  :signaled-conditions (nreverse signaled)
                  :truncated? (bounded-sink-truncated? sink)
                  :redefined redefined
                  :history history-snap
                  :handler-attempts (nreverse (car handler-attempts-cell))))))
      (handler-case
          (let ((*package* package))
            (setf form (%read-form form-text)))
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
            (%with-repl-history (history)
              (let* ((record-signals? (getf options :record-signals))
                     (debug? (and job (getf options :debug)))
                     (handler-specs (%parse-handler-specs
                                     (getf options :handlers)
                                     package))
                     (break-on-spec (getf options :break-on))
                     (break-on-none? (eq break-on-spec :none))
                     (break-on-type
                       (cond
                         (break-on-none? nil)
                         ((stringp break-on-spec)
                          (handler-case (read-from-string break-on-spec)
                            (error () nil)))
                         (t nil))))
                (flet ((on-condition (c)
                         (cond
                           ((typep c 'user-interrupt) nil)
                           ;; Resource-exhausted is fielded by the outer
                           ;; handler-case which converts it into the
                           ;; "resource-exhausted" eval-result code. Keep
                           ;; it out of the declarative / record-signals
                           ;; paths.
                           ((typep c 'resource-exhausted) nil)
                           ;; --handlers: non-interactive recovery. The
                           ;; first matching spec invokes its restart and
                           ;; transfers control; non-matching specs fall
                           ;; through. A spec that matches by type but
                           ;; has no such restart available records an
                           ;; attempt in handler-attempts-cell so the
                           ;; caller can see *why* recovery didn't fire.
                           ((%try-declarative-handler
                             c handler-specs handler-attempts-cell))
                           ((typep c 'error)
                            (unless error-snapshot
                              (setf error-snapshot
                                    (%capture-error-snapshot c)))
                            ;; --debug: hand control to the interactive
                            ;; debugger loop. It either invokes a restart
                            ;; (unwinds out of this lambda) or returns NIL
                            ;; (the condition propagates).
                            (when debug?
                              (%enter-debugger c job)))
                           (record-signals?
                            (push (%condition-json c) signaled)))))
                  (handler-bind ((condition #'on-condition))
                    (let ((*break-on-signals*
                            (cond
                              (break-on-none? nil)
                              (break-on-type)
                              (t *break-on-signals*)))
                          ;; --debug also intercepts `(break ...)`, which
                          ;; calls invoke-debugger directly without going
                          ;; through `signal'. ANSI says break nulls
                          ;; *debugger-hook*, so we have to hook the SBCL-
                          ;; specific *invoke-debugger-hook* (which break
                          ;; leaves untouched). Setting it to our hook
                          ;; replaces SBCL's --non-interactive quit hook
                          ;; for the duration of the eval.
                          #+sbcl
                          (sb-ext:*invoke-debugger-hook*
                            (if debug?
                                (lambda (c hook)
                                  (declare (ignore hook))
                                  (%enter-debugger c job))
                                sb-ext:*invoke-debugger-hook*))
                          #+sbcl
                          (heartbeat-timer
                           (and stream? ctx
                                (%start-heartbeat-timer ctx start)))
                          #+sbcl
                          (cap-timer
                           (%start-cap-timer
                            sb-thread:*current-thread*
                            options
                            start
                            (sb-ext:get-bytes-consed))))
                      (declare (ignorable
                                #+sbcl heartbeat-timer
                                #+sbcl cap-timer))
                      (unwind-protect
                           (setf returned-values
                                 (multiple-value-list (eval form)))
                        #+sbcl
                        (when heartbeat-timer
                          (ignore-errors
                           (sb-ext:unschedule-timer heartbeat-timer)))
                        #+sbcl
                        (when cap-timer
                          (ignore-errors
                           (sb-ext:unschedule-timer cap-timer)))))))))
            (setf package *package*)
            ;; History is updated *only* when no override was specified --
            ;; the override is per-call scoped. Persistent package state
            ;; lives on the worker (the default worker for v1 callers,
            ;; named workers for `eval --worker NAME').
            (when (null override-pkg)
              (when *current-worker*
                (setf (worker-package *current-worker*) *package*))
              (handler-case (%update-history! history form returned-values)
                (error () nil))))
        (user-interrupt ()
          (setf code "interrupted")
          (return-from %eval-one (finish)))
        (resource-exhausted (c)
          (setf code "resource-exhausted")
          (push (%json-object
                 "kind" (string-downcase
                         (symbol-name (resource-exhausted-kind c)))
                 "limit" (resource-exhausted-limit c)
                 "observed" (resource-exhausted-observed c))
                conditions)
          (return-from %eval-one (finish)))
        (error (c)
          (return-from %eval-one (finish :err-condition c))))
      (finish))))

#+sbcl
(defun %start-cap-timer (worker-thread options start-real-time start-cons-bytes)
  "If OPTIONS sets `:max-real-ms' or `:max-cons-bytes', schedule a
periodic timer that interrupts WORKER-THREAD with a `resource-exhausted'
signal once the cap is crossed. Returns the timer (or NIL when no cap
was requested). SBCL-only.

Polling interval is 250 ms -- granular enough to catch runaway loops
while keeping the timer fired count small."
  (let ((max-ms (getf options :max-real-ms))
        (max-bytes (getf options :max-cons-bytes)))
    (when (or (and max-ms (plusp max-ms))
              (and max-bytes (plusp max-bytes)))
      (let ((timer
              (sb-ext:make-timer
               (lambda ()
                 (handler-case
                     (let* ((elapsed (round
                                      (* 1000.0
                                         (/ (- (get-internal-real-time)
                                               start-real-time)
                                            internal-time-units-per-second))))
                            (consumed (and max-bytes
                                           (- (sb-ext:get-bytes-consed)
                                              start-cons-bytes)))
                            (kind (cond
                                    ((and max-ms (>= elapsed max-ms))
                                     :real-ms)
                                    ((and consumed (>= consumed max-bytes))
                                     :cons-bytes))))
                       (when kind
                         (sb-thread:interrupt-thread
                          worker-thread
                          (lambda ()
                            (signal 'resource-exhausted
                                    :kind kind
                                    :limit (if (eq kind :real-ms)
                                               max-ms max-bytes)
                                    :observed (if (eq kind :real-ms)
                                                  elapsed consumed))))))
                   (error () nil)))
               :name "clpm.repl.cap"
               :thread t)))
        (sb-ext:schedule-timer timer 1/4 :repeat-interval 1/4)
        timer))))

#+sbcl
(defun %start-heartbeat-timer (ctx start-real-time)
  "Schedule a periodic timer that emits `event:heartbeat' to CTX every
`+heartbeat-interval-seconds+'. Returns the timer so the caller can
`unschedule-timer' it after the eval completes. SBCL-only."
  (let ((timer (sb-ext:make-timer
                (lambda ()
                  (let ((elapsed (round
                                  (* 1000.0
                                     (/ (- (get-internal-real-time)
                                           start-real-time)
                                        internal-time-units-per-second)))))
                    (handler-case
                        (%emit-event ctx "heartbeat"
                                     "elapsed_ms" elapsed
                                     "bytes_consed"
                                     (sb-ext:get-bytes-consed)
                                     "gc_run_time"
                                     sb-ext:*gc-real-time*)
                      (error () nil))))
                :name "clpm.repl.heartbeat"
                :thread t)))
    (sb-ext:schedule-timer timer
                           +heartbeat-interval-seconds+
                           :repeat-interval +heartbeat-interval-seconds+)
    timer))

(defun %worker-loop (worker)
  "Pull jobs from WORKER's mailbox, eval each, post the result to the job's
result-mailbox. Returns when a `:stop' sentinel arrives. Binds
`*current-worker*' so `%eval-one' and `%record-redefinition' land their
side-effects on the right worker."
  (let* ((server (worker-server worker))
         (*server* server)
         (*default-pathname-defaults*
           (%server-default-pathname-defaults server))
         (mailbox (worker-mailbox worker)))
    (loop
      (let ((job (clpm.repl.compat:receive-message mailbox)))
        (cond
          ((eq job :stop) (return))
          (t
           (setf (worker-state worker) :busy
                 (worker-current-job worker) job
                 (worker-last-active-at worker) (get-universal-time))
           (let ((result
                   (let ((*current-worker* worker))
                     (handler-case
                         (%eval-one (eval-job-form job)
                                    :package-override (eval-job-package-override job)
                                    :options (eval-job-options job)
                                    :job job)
                       ;; Any unexpected interrupt at this outermost level
                       ;; becomes an "interrupted" result for the requester.
                       (user-interrupt ()
                         (make-eval-result :code "interrupted"
                                           :values nil :output "" :error-output ""
                                           :package "" :elapsed-ms 0
                                           :conditions '()
                                           :signaled-conditions nil
                                           :truncated? nil
                                           :redefined nil
                                           :history nil))))))
             (setf (worker-state worker) :idle
                   (worker-current-job worker) nil
                   (worker-last-active-at worker) (get-universal-time))
             (clpm.repl.compat:send-message (eval-job-result-mailbox job) result))))))))

(defun %make-worker (server name &key concurrent?)
  "Spawn a fresh worker thread and register it under NAME. Caller must hold
SERVER-WORKERS-MUTEX."
  (let* ((mailbox (clpm.repl.compat:make-mailbox))
         (worker (make-worker :name name
                              :server server
                              :mailbox mailbox
                              :package (server-initial-package server)
                              :concurrent? concurrent?)))
    (setf (worker-thread worker)
          (clpm.repl.compat:make-thread
           (lambda () (%worker-loop worker))
           :name (format nil "clpm.repl.worker[~A]" name)))
    (setf (gethash name (server-workers server)) worker)
    worker))

(defun %ensure-worker (server &key (name +default-worker-name+) concurrent?)
  "Return the worker named NAME, spawning it if absent or dead. With
CONCURRENT?, NAME is a freshly-minted one-shot identifier; the caller
should pass an existing concurrent name back as NAME to address it
again.

When NAME existed but its thread is dead (unexpected crash, not a
clean `:stop'), the replacement worker is marked RESTARTED? so the
next eval response can surface `worker_restarted: true' to the
client. An event-log entry records the death."
  (clpm.repl.compat:with-mutex ((server-workers-mutex server))
    (let ((existing (gethash name (server-workers server))))
      (cond
        ((and existing
              (clpm.repl.compat:thread-alive-p (worker-thread existing)))
         existing)
        (existing
         (remhash name (server-workers server))
         (%log-event (server-event-log server) "worker-died"
                     "worker" name)
         (let ((fresh (%make-worker server name :concurrent? concurrent?)))
           (setf (worker-restarted? fresh) t)
           fresh))
        (t
         (%make-worker server name :concurrent? concurrent?))))))

(defun %fresh-concurrent-worker (server)
  "Spawn a one-shot worker for `eval --concurrent'. The name is
auto-generated and the worker is destroyed once the eval completes."
  (clpm.repl.compat:with-mutex ((server-workers-mutex server))
    (let* ((n (incf (server-concurrent-counter server)))
           (name (format nil "$concurrent-~D" n)))
      (%make-worker server name :concurrent? t))))

(defun %find-worker (server name)
  "Return the worker named NAME, or NIL if unknown / dead."
  (clpm.repl.compat:with-mutex ((server-workers-mutex server))
    (gethash name (server-workers server))))

(defun %remove-worker (server name)
  "Remove NAME from the workers table. Caller is responsible for stopping
the thread before calling."
  (clpm.repl.compat:with-mutex ((server-workers-mutex server))
    (remhash name (server-workers server))))

(defun %all-workers (server)
  "Snapshot of all workers as a list. Thread-safe."
  (clpm.repl.compat:with-mutex ((server-workers-mutex server))
    (loop for w being the hash-values of (server-workers server) collect w)))

(defun %kill-worker (server worker)
  "Terminate WORKER's thread (if alive) and remove it from the registry."
  (when (clpm.repl.compat:thread-alive-p (worker-thread worker))
    (setf (worker-state worker) :dead)
    (clpm.repl.compat:terminate-thread (worker-thread worker)))
  (%remove-worker server (worker-name worker)))

(defun %interrupt-worker (server &optional (name +default-worker-name+))
  "Signal user-interrupt inside the named worker. NIL means the default.

Returns a keyword that distinguishes the real outcomes a caller might
care about, instead of collapsing them all into a silent T/NIL:
  :no-such-worker -- the user named a worker that doesn't exist.
  :idle           -- worker exists but isn't running an eval, OR the
                     default worker hasn't been spawned yet (the
                     spawning is lazy and the user shouldn't have to
                     know that).
  :interrupted    -- a user-interrupt was queued into the worker."
  (let* ((wname (or name +default-worker-name+))
         (default? (string= wname +default-worker-name+))
         (w (%find-worker server wname)))
    (cond
      ((and (null w) default?) :idle)
      ((null w) :no-such-worker)
      ((not (clpm.repl.compat:thread-alive-p (worker-thread w)))
       :no-such-worker)
      ((null (worker-current-job w)) :idle)
      (t
       (clpm.repl.compat:interrupt-thread
        (worker-thread w)
        (lambda () (signal 'user-interrupt)))
       :interrupted))))

(defun server-current-package (server)
  "The persistent eval `*package*' for the default worker. Other named
workers track their own package in `worker-package'."
  (worker-package (%ensure-worker server)))

(defun (setf server-current-package) (pkg server)
  (setf (worker-package (%ensure-worker server)) pkg))

(defun server-redefinitions (server)
  "Default-worker redefinition table. Named workers each keep their own,
queryable via `list-redefinitions' with `worker'."
  (worker-redefinitions (%ensure-worker server)))

;;; --------------------------------------------------------------------------
;;; Method dispatch
;;;
;;; +METHOD-REGISTRY+ is the single source of truth for what RPCs exist,
;;; their parameter schemas, their documentation, and their handler
;;; functions. %dispatch-method looks methods up here, and `methods' /
;;; `help' RPCs serialize the same data structure so docs cannot drift
;;; from the dispatcher.
;;; --------------------------------------------------------------------------

(defstruct method-spec
  "Self-describing registry entry for one RPC method.

PARAMS is a list of param descriptors, each a plist with
  :name (string)
  :type (\"string\" / \"object\" / \"array\" / \"boolean\" / \"integer\" /
         \"string-or-boolean\" / \"any\")
  :required (boolean)
  :description (string).

HANDLER is `(server params id ctx) -> terminal-response | NIL'. Returning
NIL means the handler has already emitted its terminal frame."
  name
  summary
  doc
  params
  handler)

(defvar +method-registry+ nil
  "Alist of (method-name . method-spec). Populated below.")

(defun %register-method (spec)
  (let ((existing (assoc (method-spec-name spec) +method-registry+
                         :test #'string=)))
    (cond
      (existing (setf (cdr existing) spec))
      (t (setf +method-registry+
               (append +method-registry+
                       (list (cons (method-spec-name spec) spec))))))))

(defun %lookup-method (name)
  (cdr (assoc name +method-registry+ :test #'string=)))

(defparameter +undiscoverable-methods+ '("eval" "shutdown" "query-response")
  "Registered wire messages that are not part of public `repl call' discovery.")

(defun %discoverable-method-spec-p (spec)
  (not (member (method-spec-name spec)
               +undiscoverable-methods+
               :test #'string=)))

(defun %method-param-type-name (type)
  (ecase type
    (:any "any")
    (:string "string")
    (:integer "integer")
    (:boolean "boolean")
    (:array "array")
    (:object "object")
    (:string-or-boolean "string-or-boolean")))

(defun %method-spec-as-json (spec)
  (%json-object
   "name" (method-spec-name spec)
   "summary" (method-spec-summary spec)
   "doc" (method-spec-doc spec)
   "params" (%json-array
             (loop for p in (method-spec-params spec)
                   collect (%json-object
                            "name" (getf p :name)
                            "type" (%method-param-type-name (getf p :type))
                            "required" (and (getf p :required) t)
	                            "description" (getf p :description))))))

(defparameter +implicit-method-params+ '("token" "explain" "project_root")
  "Transport/dispatch params accepted for every method without appearing in
the method-local schema.")

(defun %json-object-alist (value)
  (when (and (consp value) (eq (car value) :object))
    (cadr value)))

(defun %json-array-p (value)
  (and (consp value) (eq (car value) :array)))

(defun %json-object-p (value)
  (and (consp value) (eq (car value) :object)))

(defun %json-boolean-p (value)
  (or (eq value t) (eq value :false)))

(defun %json-value-type-name (value)
  (cond
    ((stringp value) "string")
    ((integerp value) "integer")
    ((%json-array-p value) "array")
    ((%json-object-p value) "object")
    ((%json-boolean-p value) "boolean")
    (t (string-downcase (symbol-name (type-of value))))))

(defun %json-value-matches-type-p (value type)
  (ecase type
    (:any t)
    (:string (stringp value))
    (:integer (integerp value))
    (:boolean (%json-boolean-p value))
    (:array (%json-array-p value))
    (:object (%json-object-p value))
    (:string-or-boolean
     (or (stringp value) (%json-boolean-p value)))))

(defun %method-param-spec (spec name)
  (find name (method-spec-params spec)
        :key (lambda (p) (getf p :name))
        :test #'string=))

(defun %implicit-method-param-p (name)
  (member name +implicit-method-params+ :test #'string=))

(defun %method-local-params (params)
  "Return PARAMS without transport/dispatch fields."
  (let ((alist (%json-object-alist params)))
    (if alist
        (list :object
              (loop for entry in alist
                    unless (%implicit-method-param-p (car entry))
                      collect entry))
        (%json-object))))

(defun %decode-method-params (spec params id)
  "Validate PARAMS against SPEC. Returns (values PARAMS NIL) on success,
or (values NIL ERROR-RESPONSE) on failure."
  (flet ((error-result (fmt &rest args)
           (%error-response id "protocol-error"
                            (apply #'format nil fmt args))))
    (let ((alist nil))
      (cond
        ((null params))
        ((%json-object-p params)
         (setf alist (%json-object-alist params)))
        (t
         (return-from %decode-method-params
           (values nil
                   (error-result "params must be an object for ~A"
                                 (method-spec-name spec))))))
      (dolist (entry alist)
        (let ((name (car entry)))
          (unless (and (stringp name)
                       (or (%method-param-spec spec name)
                           (%implicit-method-param-p name)))
            (return-from %decode-method-params
              (values nil
                      (error-result "unknown param `~A' for ~A"
                                    name
                                    (method-spec-name spec)))))))
      (dolist (param-spec (method-spec-params spec))
        (let* ((name (getf param-spec :name))
               (type (getf param-spec :type))
               (cell (assoc name alist :test #'string=)))
          (cond
            ((and (getf param-spec :required) (null cell))
             (return-from %decode-method-params
               (values nil
                       (error-result "missing required param `~A' for ~A"
                                     name
                                     (method-spec-name spec)))))
            ((and cell
                  type
                  (not (%json-value-matches-type-p (cdr cell) type)))
             (return-from %decode-method-params
               (values nil
                       (error-result "param `~A' expected ~A, got ~A"
                                     name
                                     (%method-param-type-name type)
                                     (%json-value-type-name (cdr cell)))))))))
      (values (or params (%json-object)) nil))))

(defun %decode-params-for-method (method params id)
  "Decode PARAMS for METHOD when METHOD is known. Unknown methods are left to
the normal dispatch path so they still produce the canonical unknown-method
error."
  (let ((spec (%lookup-method method)))
    (cond
      (spec (%decode-method-params spec params id))
      (t (values params nil)))))

(defun %dispatch-method (server method params id &optional ctx)
  "Return a terminal response for METHOD; never raises."
  (let ((spec (%lookup-method method)))
    (cond
      ((null spec)
       (%error-response id "protocol-error"
                        (format nil "unknown method: ~A" method)))
      (t
       (multiple-value-bind (decoded-params decode-error)
           (%decode-method-params spec params id)
         (cond
           (decode-error decode-error)
           (t
            (handler-case
                (funcall (method-spec-handler spec)
                         server decoded-params id ctx)
              (error (c)
                (%error-response id "protocol-error"
                                 (format nil "dispatch failed: ~A" c)))))))))))

;;; ----------------------------------------------------------------------------
;;; Registered methods

(defun %method-counts-json (server)
  "Snapshot public callable method counters for ping's observability payload."
  (clpm.repl.compat:with-mutex ((server-method-counts-mutex server))
    (let* ((tbl (server-method-counts server))
           (pairs (loop for m being the hash-keys of tbl using (hash-value v)
                        for spec = (%lookup-method m)
                        when (and spec (%discoverable-method-spec-p spec))
                          collect (cons m (list :object
                                                (list (cons "total" (car v))
                                                      (cons "errors" (cdr v))))))))
      (list :object pairs))))

(%register-method
 (make-method-spec
  :name "ping"
  :summary "Liveness probe; returns the daemon pid, uptime, and lisp version."
  :doc "Returns a small JSON object describing the daemon. No parameters.
Useful for confirming the connection works and for detecting daemon restarts
via the `pid' and `eval_count' fields.

The response includes public callable method counters (`method_counts':
method name -> {total, errors}) and a `recent_error_count' running total
since startup, so clients can spot a misbehaving RPC without scraping the
event log."
  :params nil
  :handler
  (lambda (server params id ctx)
    (declare (ignore params ctx))
    (%success-response
     id
     (apply #'%json-object
            (append
             (list
              "pid" (clpm.repl.compat:getpid)
              "uptime_ms" (* 1000 (- (get-universal-time)
                                      (server-started-at server)))
              "lisp" (format nil "~A ~A"
                              (lisp-implementation-type)
                              (lisp-implementation-version)))
             (let ((project-id (%server-project-fingerprint server)))
               (when project-id
                 (list "project_id" project-id)))
             (list
              "eval_count" (server-eval-count server)
              "method_counts" (%method-counts-json server)
              "recent_error_count" (server-recent-error-count server))))))))

(%register-method
 (make-method-spec
  :name "current-package"
  :summary "Return the persistent eval *package* as a string."
  :doc "Returns `{package: \"FOO\"}'. Pass `worker' to inspect a named
worker's package; otherwise the default worker is reported."
  :params (list (list :name "worker" :type :string :required nil
                      :description "Name of the worker (default: \"default\")."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((wname (or (%json-getf params "worker") +default-worker-name+))
           (w (%ensure-worker server :name wname)))
      (%success-response
       id (%json-object "worker" (worker-name w)
                        "package" (%public-package-name
                                   (worker-package w) server)))))))

(%register-method
 (make-method-spec
  :name "set-package"
  :summary "Set the persistent eval *package* by name."
  :doc "Looks up the package case-insensitively. Returns the canonical name.
Pass `worker' to set a named worker's package; otherwise the default
worker is changed."
  :params (list (list :name "name" :type :string :required t
                      :description "Package name; matched case-insensitively.")
                (list :name "worker" :type :string :required nil
                      :description "Worker name (default: \"default\")."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((name (%json-getf params "name"))
           (wname (or (%json-getf params "worker") +default-worker-name+))
           (pkg (%resolve-package-for-server server name)))
      (cond
        ((not (stringp name))
         (%error-response id "protocol-error" "missing `name` param"))
        ((null pkg)
         (%error-response id "eval-error"
                          (format nil "No such package: ~A" name)))
        (t
         (let ((w (%ensure-worker server :name wname)))
           (setf (worker-package w) pkg)
           (%success-response id
            (%json-object "worker" (worker-name w)
                          "package" (%public-package-name pkg server))))))))))

(%register-method
 (make-method-spec
  :name "eval"
  :summary "Evaluate a Lisp form and return its values, output, and condition signals."
  :doc "Required: `form'. Optional toggles:

  `package'            -- per-call reader package override, non-persistent.
  `stream'             -- emit incremental `event:stdout' / `event:stderr'
                          frames; long evals also get `event:heartbeat'
                          every 30 s with bytes-consed + gc time.
  `query_interactive'  -- bind *standard-input* to a stream that emits
                          `event:query'; client replies via `query-response'.
  `debug'              -- pause on errors in the interactive debugger.
  `record_signals'     -- collect non-error signaled conditions.
  `worker' / `concurrent' -- route to a named or one-shot worker.
  `handlers'           -- declarative restart auto-invocation.
  `break_on'           -- bind *break-on-signals*; \"none\" / false / \"nil\"
                          disables the global default for this eval.
  `max_real_ms' / `max_cons_bytes'
                       -- per-eval resource caps; crossing one aborts the
                          eval with code `resource-exhausted'.
  `print_length' / `print_level' / `print_circle' / `print_radix' /
  `print_base' / `print_pretty' -- printer bindings for the values'
                                   prin1 output.

The response includes `values' (a JSON array of prin1'd values),
`value' (the primary value, retained for v1 clients), `output' /
`error_output', `package' (post-eval), `elapsed_ms', `conditions' (any
ERROR that unwound the form), `signaled_conditions' (non-errors, when
`record_signals' is set), `history' (a snapshot of the REPL bindings
*, **, ***, +, ++, +++, /, //, ///), and -- when a top-level form
redefines something tracked -- `redefined'.

When `--handler' specs were supplied and one matched the condition's
type but its named restart wasn't available, `handler_attempts' lists
each failed match: `{type, restart, outcome: \"matched-no-restart\",
available_restarts}'. This makes a misspelled restart name (and a form
lacking the expected `restart-case') distinguishable from a plain
no-handler-matched outcome.

After an unexpected worker death, the next eval's result carries
`worker_restarted: true' so the client knows in-image state was
lost."
  :params (list (list :name "form" :type :string :required t
                      :description "Lisp source for exactly one form.")
                (list :name "package" :type :string :required nil
                      :description "Per-call package override.")
                (list :name "stream" :type :boolean :required nil
                      :description "Emit incremental stdout/stderr events.")
	                (list :name "query_interactive" :type :boolean :required nil
	                      :description "Bind *standard-input* to a bidirectional query stream.")
	                (list :name "debug" :type :boolean :required nil
	                      :description "Enter a server-owned debug session on unhandled conditions.")
	                (list :name "record_signals" :type :boolean :required nil
	                      :description "Record non-error conditions signaled during eval.")
	                (list :name "worker" :type :string :required nil
	                      :description "Run on a named worker; spawned if absent.")
	                (list :name "concurrent" :type :boolean :required nil
	                      :description "Run on a fresh disposable worker that's destroyed after the eval.")
	                (list :name "handlers" :type :array :required nil
	                      :description "Declarative condition handlers as {type,restart,args} objects.")
	                (list :name "break_on" :type :string-or-boolean :required nil
	                      :description "Type name to bind *break-on-signals* to; \"none\" / false / \"nil\" disables.")
	                (list :name "max_real_ms" :type :integer :required nil
	                      :description "Abort with code resource-exhausted if real time exceeds this.")
	                (list :name "max_cons_bytes" :type :integer :required nil
	                      :description "Abort with code resource-exhausted if bytes-consed exceeds this.")
	                (list :name "print_length" :type :integer :required nil
	                      :description "Bind *print-length* during prin1 of values.")
	                (list :name "print_level" :type :integer :required nil
	                      :description "Bind *print-level* during prin1 of values.")
	                (list :name "print_circle" :type :boolean :required nil
	                      :description "Bind *print-circle* during prin1 of values.")
	                (list :name "print_radix" :type :boolean :required nil
	                      :description "Bind *print-radix* during prin1 of values.")
	                (list :name "print_base" :type :integer :required nil
	                      :description "Bind *print-base* during prin1 of values.")
	                (list :name "print_pretty" :type :boolean :required nil
	                      :description "Bind *print-pretty* during prin1 of values."))
  :handler
  (lambda (server params id ctx)
    (%dispatch-eval server params id ctx))))

(%register-method
 (make-method-spec
  :name "interrupt"
  :summary "Signal a user-interrupt inside the worker, unwinding its current eval."
  :doc "Async: returns immediately. The response `outcome' field distinguishes
the three states the caller might care about so an interrupt of a
non-existent worker (typo) and an interrupt of an idle one are not
confused with an interrupt that actually unwound an eval:

  \"interrupted\"     -- user-interrupt was sent to a busy worker.
  \"idle\"            -- worker exists but no eval was in progress.
  \"no-such-worker\"  -- no worker by that name (typo or already reset).

Pass `worker' to target a named worker; otherwise the default."
  :params (list (list :name "worker" :type :string :required nil
                      :description "Worker name (default: \"default\")."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((wname (or (%json-getf params "worker") +default-worker-name+))
           (outcome (progn
                      (%log-event (server-event-log server)
                                  "interrupt" "worker" wname)
                      (%interrupt-worker server wname))))
      (%success-response id
                         (%json-object "outcome" (string-downcase outcome)
                                       "worker" wname))))))

(%register-method
 (make-method-spec
  :name "reset"
  :summary "Kill a worker thread and clear its redefinition log."
  :doc "Use to recover from a runaway eval that ignored `interrupt'. The
next eval against the same worker name spawns a fresh thread.
Persistent package state is preserved across resets. Pass `worker' to
reset a named worker; otherwise the default worker is reset.

Response `outcome' field:
  \"reset\"           -- existing worker was terminated + respawned.
  \"spawned\"         -- the default worker did not yet exist; a fresh
                       one was created. (Only the default; for named
                       workers the spawning-on-reset would mask typos.)
  \"no-such-worker\"  -- the user named a worker that doesn't exist."
  :params (list (list :name "worker" :type :string :required nil
                      :description "Worker name (default: \"default\")."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((wname (or (%json-getf params "worker") +default-worker-name+))
           (default? (string= wname +default-worker-name+))
           (w (%find-worker server wname))
           (outcome
             (cond
               (w
                (let ((preserved-pkg (worker-package w)))
                  (when (clpm.repl.compat:thread-alive-p (worker-thread w))
                    (%log-event (server-event-log server)
                                "worker-terminated" "worker" wname)
                    (clpm.repl.compat:terminate-thread (worker-thread w)))
                  (%remove-worker server wname)
                  (let ((fresh (%ensure-worker server :name wname)))
                    (setf (worker-package fresh) preserved-pkg)))
                "reset")
               (default?
                ;; The default worker is lazy-spawned; resetting it
                ;; before any eval has run is well-defined.
                (%ensure-worker server :name wname)
                "spawned")
               (t "no-such-worker"))))
      (%success-response id
                         (%json-object "outcome" outcome "worker" wname))))))

(defun %worker-state-string (worker)
  "Human-readable string form of a worker's STATE keyword."
  (let ((debug (and (worker-current-job worker)
                    (eval-job-debug-session (worker-current-job worker)))))
    (cond
      (debug "in-debugger")
      ((eq (worker-state worker) :busy) "busy")
      ((eq (worker-state worker) :dead) "dead")
      (t "idle"))))

(defun %worker-json (worker)
  "JSON `(:object …)' summary of one worker. `now' is the current
universal time, used for `age_seconds'."
  (let ((now (get-universal-time)))
    (list :object
          (list (cons "name" (worker-name worker))
                (cons "state" (%worker-state-string worker))
                (cons "package" (%public-package-name
                                 (worker-package worker) *server*))
                (cons "last_eval_id" (worker-last-eval-id worker))
                (cons "started_at_unix" (worker-started-at worker))
                (cons "age_seconds" (- now (worker-started-at worker)))
                (cons "last_active_seconds_ago"
                      (- now (worker-last-active-at worker)))
                (cons "concurrent" (worker-concurrent? worker))
                (cons "alive"
                      (and (worker-thread worker)
                           (clpm.repl.compat:thread-alive-p
                            (worker-thread worker))
                           t))))))

(%register-method
 (make-method-spec
  :name "list-workers"
  :summary "Return every named eval worker with its state and package."
  :doc "Returns `{entries: [{name, state, package, last_eval_id,
started_at_unix, age_seconds, last_active_seconds_ago, concurrent,
alive}, ...]}'. `state' is one of `idle' / `busy' / `in-debugger' /
`dead'."
  :params nil
  :handler
  (lambda (server params id ctx)
    (declare (ignore params ctx))
    ;; Make sure the default worker shows up even before any eval has
    ;; been routed to it.
    (%ensure-worker server)
    (%success-response
     id
     (%json-object "entries"
                   (%json-array
                    (mapcar #'%worker-json (%all-workers server))))))))

(%register-method
 (make-method-spec
  :name "kill-worker"
  :summary "Terminate a worker thread and forget it."
  :doc "Like `reset' but the worker is *not* recreated -- subsequent
`eval' calls naming a killed worker will spawn a fresh one. The
default worker cannot be killed; use `reset' instead."
  :params (list (list :name "name" :type :string :required t
                      :description "Name of the worker to kill."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((name (%json-getf params "name")))
      (cond
        ((not (stringp name))
         (%error-response id "protocol-error" "missing `name' param"))
        ((string= name +default-worker-name+)
         (%error-response id "eval-error"
                          "use `reset' instead of killing the default worker"))
        (t
         (let ((w (%find-worker server name)))
           (cond
             ((null w)
              ;; Idempotent: killing an unknown worker is a no-op.
              (%success-response id
               (%json-object "name" name "killed" nil)))
             (t
              (%log-event (server-event-log server)
                          "worker-killed" "worker" name)
              (%kill-worker server w)
              (%success-response id
               (%json-object "name" name "killed" t)))))))))))

(%register-method
 (make-method-spec
  :name "describe"
  :summary "Return the CL:DESCRIBE output for a symbol."
  :doc "Resolves `symbol' in `package' (default: the persistent package).
Returns `{output: <text>}'."
  :params (list (list :name "symbol" :type :string :required t
                      :description "Symbol name; matched case-insensitively.")
                (list :name "package" :type :string :required nil
                      :description "Package to resolve the symbol in."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (%dispatch-describe server params id))))

(%register-method
 (make-method-spec
  :name "list-redefinitions"
  :summary "Return the running log of top-level redefinitions seen by eval."
  :doc "Defun, defmethod, defmacro, defgeneric, defclass, defstruct, defvar,
defparameter, defconstant, define-condition, defpackage. The log is
cleared by `reset'. Pass `worker' to scope to a named worker, otherwise
the default worker's log is returned."
  :params (list (list :name "worker" :type :string :required nil
                      :description "Worker name (default: \"default\")."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((wname (or (%json-getf params "worker") +default-worker-name+))
           (w (%ensure-worker server :name wname)))
      (%success-response
       id
       (%json-object "worker" (worker-name w)
                     "entries"
                     (%json-array
                      (loop for v being the hash-values of
                            (worker-redefinitions w)
                            collect (list :object v)))))))))

(%register-method
 (make-method-spec
  :name "shutdown"
  :summary "Ask the daemon to exit. Always returns success before unwinding."
  :doc "Closes the listening socket; the daemon's accept loop notices and
unwinds. Any in-flight evals are interrupted via the unwind-protect."
  :params nil
  :handler
  (lambda (server params id ctx)
    (declare (ignore params ctx))
    (%log-event (server-event-log server) "shutdown")
    (setf (server-shutdown-requested? server) t)
    (ignore-errors
     (when (server-socket server)
       (sb-bsd-sockets:socket-close (server-socket server))))
    (%success-response id (%json-object)))))

;;; ----------------------------------------------------------------------------
;;; File watching (BRIDGE_V2 #180-#182)
;;;
;;; A watch is a background thread that polls a directory's `*.glob`
;;; entries every 1 s. On mtime change the watcher LOADs the file (so
;;; redefinitions land in-image) and streams `event: file-changed' /
;;; `event: file-reloaded' / `event: reload-failed' frames back through
;;; the original `watch' request's context. The terminal `result' frame
;;; is emitted by the watcher thread itself when an `unwatch' arrives.
;;;
;;; Auto-revert (#182): if AUTO-REVERT? is set, any definition recorded
;;; in the current worker's redefinition log that points at the watched
;;; file is re-evaluated from disk. Today that's equivalent to "load
;;; the file"; the simple path keeps the on-disk version authoritative.
;;; ----------------------------------------------------------------------------

(defstruct watch
  (id 0 :type integer)
  (dir "" :type string)
  (glob "*.lisp" :type string)
  (auto-revert? nil :type boolean)
  ctx
  thread
  control-mailbox
  ;; truename namestring -> file-write-date integer
  (mtimes (make-hash-table :test 'equal)))

(defun %watch-pattern (dir glob)
  "Return the wild pathname used by `directory' to enumerate matches."
  (merge-pathnames glob (uiop:ensure-directory-pathname dir)))

(defun %watch-scan (watch)
  "Compute a fresh `(filename . write-date)' alist of matching files."
  (let ((files (handler-case
                   (directory (%watch-pattern (watch-dir watch)
                                              (watch-glob watch)))
                 (error () nil))))
    (loop for f in files
          for tn = (handler-case (namestring (truename f))
                     (error () nil))
          for mt = (and tn (handler-case (file-write-date f)
                             (error () nil)))
          when (and tn mt)
            collect (cons tn mt))))

(defun %watch-load-file (path)
  "Load PATH and return `(list :ok? :diagnostics)' for the watcher to
broadcast. Errors are captured rather than re-signaled."
  (let ((diags '())
        (ok? t))
    (handler-case
        (handler-bind ((warning
                         (lambda (c)
                           (push (%json-object
                                  "severity" "warning"
                                  "message" (princ-to-string c))
                                 diags)
                           (muffle-warning c))))
          (load path :verbose nil :print nil))
      (error (c)
        (setf ok? nil)
        (push (%json-object
               "severity" "error"
               "message" (princ-to-string c))
              diags)))
    (list (cons :ok? ok?)
          (cons :diagnostics (nreverse diags)))))

(defun %watch-emit (watch event-name &rest fields)
  "Broadcast a non-terminal event to the watch's request context, if
still attached. Failures (closed socket) are swallowed."
  (let ((ctx (watch-ctx watch)))
    (when ctx
      (apply #'%emit-event ctx event-name fields))))

(defun %watch-revert-from-file (watch path diagnostics)
  "When AUTO-REVERT? is set, emit a `revert-applied' event after the
load so clients can see that the on-disk version is now authoritative.
The actual revert is implicit in the load that just succeeded."
  (declare (ignore diagnostics))
  (when (watch-auto-revert? watch)
    (let ((target (handler-case (namestring (truename path))
                    (error () path))))
      (%watch-emit watch "revert-applied" "file" target))))

(defun %watch-poll (watch)
  "Compare current scan with WATCH's tracked mtimes; emit events for
added / modified / deleted files."
  (let* ((scan (%watch-scan watch))
         (now (make-hash-table :test 'equal))
         (mtimes (watch-mtimes watch)))
    (loop for (f . mt) in scan do (setf (gethash f now) mt))
    ;; Added / modified.
    (loop for f being the hash-keys of now using (hash-value mt)
          for prior = (gethash f mtimes)
          when (or (null prior) (> mt prior))
            do (let ((reload (%watch-load-file f)))
                 (cond
                   ((cdr (assoc :ok? reload))
                    (%watch-emit watch "file-reloaded"
                                 "file" f
                                 "diagnostics"
                                 (%json-array (cdr (assoc :diagnostics reload))))
                    (%watch-revert-from-file watch f
                                              (cdr (assoc :diagnostics reload))))
                   (t
                    (%watch-emit watch "reload-failed"
                                 "file" f
                                 "diagnostics"
                                 (%json-array (cdr (assoc :diagnostics reload)))))))
             (setf (gethash f mtimes) mt))
    ;; Removed.
    (loop for f being the hash-keys of mtimes
          unless (gethash f now)
            do (%watch-emit watch "file-removed" "file" f)
               (remhash f mtimes))))

(defun %watch-loop (server watch)
  "Daemon thread for one watch. Polls every second; exits when its
control mailbox receives `:stop'. Emits the terminal frame on the way
out so the watcher's `watch' request finally completes."
  (let ((mbox (watch-control-mailbox watch)))
    (%watch-emit watch "watch-started"
                 "id" (watch-id watch)
                 "dir" (watch-dir watch)
                 "glob" (watch-glob watch)
                 "auto_revert" (watch-auto-revert? watch))
    ;; Seed the mtime map so the first poll doesn't fire a flood of
    ;; "file-reloaded" events for files that haven't actually changed
    ;; since the watch started.
    (loop for (f . mt) in (%watch-scan watch)
          do (setf (gethash f (watch-mtimes watch)) mt))
    (loop
      (let ((msg (clpm.repl.compat:receive-message-no-hang mbox)))
        (when (cdr msg)
          (case (car msg)
            (:stop
             (let ((ctx (watch-ctx watch)))
               (when ctx
                 (%emit-terminal
                  ctx
                  (%success-response (request-context-id ctx)
                                     (%json-object "id" (watch-id watch)
                                                   "unwatched" t)))))
             (clpm.repl.compat:with-mutex
                 ((server-watches-mutex server))
               (remhash (watch-id watch) (server-watches server)))
             (return)))))
      (handler-case (%watch-poll watch)
        (error (c)
          (%watch-emit watch "watch-error" "message" (princ-to-string c))))
      (sleep 1))))

(defun %make-watch (server dir glob auto-revert? ctx)
  "Spawn a fresh watch, register it, and start its polling thread."
  (clpm.repl.compat:with-mutex ((server-watches-mutex server))
    (let* ((id (incf (server-watch-counter server)))
           (w (make-watch :id id
                          :dir dir
                          :glob glob
                          :auto-revert? auto-revert?
                          :ctx ctx
                          :control-mailbox
                          (clpm.repl.compat:make-mailbox))))
      (setf (watch-thread w)
            (clpm.repl.compat:make-thread
             (let ((owner server))
               (lambda ()
                 (let ((*server* owner)
                       (*default-pathname-defaults*
                         (%server-default-pathname-defaults owner)))
                   (%watch-loop owner w))))
             :name (format nil "clpm.repl.watch[~D]" id)))
      (setf (gethash id (server-watches server)) w)
      w)))

(defun %find-watch (server id)
  (clpm.repl.compat:with-mutex ((server-watches-mutex server))
    (gethash id (server-watches server))))

(defun %all-watches (server)
  (clpm.repl.compat:with-mutex ((server-watches-mutex server))
    (loop for v being the hash-values of (server-watches server) collect v)))

(defun %stop-watch (watch)
  (clpm.repl.compat:send-message (watch-control-mailbox watch) :stop))

(%register-method
 (make-method-spec
  :name "watch"
  :summary "Spawn a directory watcher that reloads matching files on save."
  :doc "Required: `dir' (absolute path). Optional: `glob' (default
\"*.lisp\") and `auto_revert' (default false). Returns immediately
with `{id, dir, glob}', then streams `event: file-reloaded' / `event:
reload-failed' / `event: file-removed' / `event: revert-applied'
frames as files on disk change. Polls at 1 s.

The terminal `result' frame is emitted when `unwatch ID' is called or
when the daemon shuts down.

With `auto_revert: true', the watcher emits a `revert-applied' event
listing in-image definitions originally recorded as coming from the
reloaded file -- those definitions are now equivalent to the on-disk
version (because the file was just LOADed)."
  :params (list (list :name "dir" :type :string :required t
                      :description "Directory to watch.")
                (list :name "glob" :type :string :required nil
                      :description "Filename glob (default \"*.lisp\").")
                (list :name "auto_revert" :type :boolean :required nil
                      :description "Emit revert-applied events for matching definitions."))
  :handler
  (lambda (server params id ctx)
    (let* ((dir (%json-getf params "dir"))
           (glob (or (%json-getf params "glob") "*.lisp"))
           (auto-revert? (and (%json-getf params "auto_revert") t)))
      (cond
        ((not (stringp dir))
         (%error-response id "protocol-error" "missing `dir' param"))
        ((not (probe-file (uiop:ensure-directory-pathname dir)))
         (%error-response id "eval-error"
                          (format nil "no such directory: ~A" dir)))
        (t
         (let ((w (%make-watch server dir glob auto-revert? ctx)))
           ;; Emit the head of the stream immediately so the client
           ;; knows the watch id. We return NIL from this handler so
           ;; the dispatcher does NOT close the request -- the watcher
           ;; thread keeps the ctx alive and finalizes via unwatch.
           (%emit-event ctx "watch-acknowledged"
                        "id" (watch-id w)
                        "dir" (watch-dir w)
                        "glob" (watch-glob w)
                        "auto_revert" (watch-auto-revert? w))
           nil)))))))

(%register-method
 (make-method-spec
  :name "list-watches"
  :summary "Return every active directory watcher."
  :doc "Returns `{entries: [{id, dir, glob, auto_revert, alive}, ...]}'."
  :params nil
  :handler
  (lambda (server params id ctx)
    (declare (ignore params ctx))
    (%success-response
     id
     (%json-object
      "entries"
      (%json-array
       (mapcar (lambda (w)
                 (%json-object
                  "id" (watch-id w)
                  "dir" (watch-dir w)
                  "glob" (watch-glob w)
                  "auto_revert" (watch-auto-revert? w)
                  "alive" (and (watch-thread w)
                               (clpm.repl.compat:thread-alive-p
                                (watch-thread w))
                               t)))
               (%all-watches server))))))))

(%register-method
 (make-method-spec
  :name "unwatch"
  :summary "Stop a directory watcher, emitting its terminal `result' frame."
  :doc "Required: `id'. Idempotent on unknown ids."
  :params (list (list :name "id" :type :integer :required t
                      :description "Watch id returned by `watch'."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((wid (%json-getf params "id"))
           (w (and (integerp wid) (%find-watch server wid))))
      (cond
        ((not (integerp wid))
         (%error-response id "protocol-error" "missing or invalid `id'"))
        ((null w)
         (%success-response id
                            (%json-object "id" wid "stopped" nil)))
        (t
         (%stop-watch w)
         (%success-response id
                            (%json-object "id" wid "stopped" t))))))))

(%register-method
 (make-method-spec
  :name "methods"
  :summary "List public RPC methods with their parameter schemas and summaries."
  :doc "Returns `{methods: [<method-spec>, ...]}'. The list is generated
from the same registry the dispatcher consults, minus `eval' and
protocol-internal continuation/lifecycle frames."
  :params nil
  :handler
  (lambda (server params id ctx)
    (declare (ignore server params ctx))
    (%success-response
     id
     (%json-object
      "methods"
      (%json-array
       (loop for entry in +method-registry+
             for spec = (cdr entry)
             when (%discoverable-method-spec-p spec)
               collect (%method-spec-as-json spec))))))))

(%register-method
 (make-method-spec
  :name "help"
  :summary "Return the long-form documentation for one RPC method."
  :doc "Required: `method'. Returns `{method: <method-spec>}' with the full
`doc' field. Returns a protocol-error for unknown methods."
  :params (list (list :name "method" :type :string :required t
                      :description "Name of the RPC to document."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore server ctx))
    (let* ((name (%json-getf params "method"))
           (spec (and (stringp name) (%lookup-method name))))
      (cond
        ((not (stringp name))
         (%error-response id "protocol-error" "missing `method' param"))
        ((or (null spec)
             (not (%discoverable-method-spec-p spec)))
         (%error-response id "protocol-error"
                          (format nil "unknown method: ~A" name)))
        (t
         (%success-response id
          (%json-object "method" (%method-spec-as-json spec)))))))))

;;; query-response is a continuation message routed inline by
;;; %route-query-response, not dispatched as a normal RPC. It remains
;;; registered for schema validation but is intentionally absent from
;;; public `methods' / `help' discovery. The same discovery rule hides
;;; eval: raw protocol clients may send it, but the public CLI constructor is
;;; `clpm repl eval FORM', not `clpm repl call eval'.
(%register-method
 (make-method-spec
  :name "query-response"
  :summary "Continuation message: reply to a daemon `event:query'."
  :doc "Sent on the *same* id as the in-flight eval that issued the query.
Required: `value'. Optional: `eof' (boolean) signals end-of-input. Has no
terminal frame -- the originating eval's terminal frame is the response."
  :params (list (list :name "value" :type :string :required nil
                      :description "The string the worker's read-line returns.")
                (list :name "eof" :type :boolean :required nil
                      :description "If true, the worker sees EOF on the input stream."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore server params ctx))
    ;; If we ever reach here it's because the connection thread routed
    ;; this as a fresh request rather than a continuation -- which means
    ;; there's no matching in-flight eval. Return an error.
    (%error-response id "protocol-error"
                     "no in-flight query waiting on this id"))))

;;; debug-* methods are also continuations. They are routed inline by
;;; %route-debug-action; we register them here so `methods' and `help'
;;; document the protocol.

(%register-method
 (make-method-spec
  :name "list-debug-sessions"
  :summary "Return every active server-owned debug session."
  :doc "No parameters. Returns `{sessions:[{session, worker,
entered_at_unix, condition}, ...]}'. A session exists from
`debugger-entered' until a restart, continue, or abort resolves it."
  :params nil
  :handler
  (lambda (server params id ctx)
    (declare (ignore params ctx))
    (%success-response
     id
     (%json-object
      "sessions"
      (%json-array
       (mapcar #'%debug-session-json
               (%all-debug-sessions server))))))))

(defun %debug-orphan-error (id)
  (%error-response id "protocol-error"
                   "no in-flight debug session on this id"))

(%register-method
 (make-method-spec
  :name "debug-invoke-restart"
  :summary "Pick a restart in the active debug session."
  :doc "Continuation: either send on the same id as an `eval' that is
currently paused in the debugger, or pass `session' to address a
server-owned debug session from a fresh connection. Required: `name'
(case-insensitive). Optional: `args' (an array of Lisp source forms,
evaluated in the worker's package before being passed to the restart).
Same-id continuations have no terminal frame; fresh session-addressed
requests return `{session, worker, outcome}'."
  :params (list (list :name "name" :type :string :required t
                      :description "Restart name, e.g. ABORT, CONTINUE, USE-VALUE.")
                (list :name "args" :type :array :required nil
                      :description "Forms to evaluate as restart arguments.")
                (list :name "session" :type :integer :required nil
                      :description "Server-owned debug session id."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore server params ctx))
    (%debug-orphan-error id))))

(%register-method
 (make-method-spec
  :name "debug-eval-in-frame"
  :summary "Evaluate a form in the lexenv of a stack frame."
  :doc "Continuation. Required: `frame' (integer index into the captured
backtrace, 0 = innermost), `form' (Lisp source). Same-id continuations
emit `event:frame-eval-result' with `values'/`output'/`error_output'.
Fresh requests may pass `session' and receive those fields as their
terminal result. The debugger session remains active afterwards."
  :params (list (list :name "frame" :type :integer :required t
                      :description "Frame index; 0 is the innermost.")
                (list :name "form" :type :string :required t
                      :description "Form to evaluate.")
                (list :name "session" :type :integer :required nil
                      :description "Server-owned debug session id."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore server params ctx))
    (%debug-orphan-error id))))

(%register-method
 (make-method-spec
  :name "debug-continue"
  :summary "Invoke the CONTINUE restart in the active debug session."
  :doc "Sugar over debug-invoke-restart for the CONTINUE restart
established by `cerror' / `break'. Pass `session' to drive a
server-owned debug session from a fresh connection."
  :params (list (list :name "session" :type :integer :required nil
                      :description "Server-owned debug session id."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore server params ctx))
    (%debug-orphan-error id))))

(%register-method
 (make-method-spec
  :name "debug-abort"
  :summary "Abort the active debug session and unwind the eval."
  :doc "Lets the original condition propagate; the eval's terminal frame
becomes the v1-shape `eval-error' response. Pass `session' to drive a
server-owned debug session from a fresh connection."
  :params (list (list :name "session" :type :integer :required nil
                      :description "Server-owned debug session id."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore server params ctx))
    (%debug-orphan-error id))))

;;; ----------------------------------------------------------------------------
;;; Source navigation and introspection (BRIDGE_V2 #130-#136, #140-#148).
;;; ----------------------------------------------------------------------------

(defun %symbol-from-string (sym-name pkg-name &key (server *server*))
  "Resolve SYM-NAME in PKG-NAME, defaulting to the server's persistent
package. Returns the symbol or NIL."
  (let* ((pkg (cond
                ((null pkg-name)
                 (and server (server-current-package server)))
                (t (%find-package-loose pkg-name)))))
    (and pkg (find-symbol (string-upcase sym-name) pkg))))

#+sbcl
(defun %definition-source-json (def-source)
  "Convert an sb-introspect:definition-source into the v2 JSON shape."
  (let* ((pathname (sb-introspect:definition-source-pathname def-source))
         (form-path (sb-introspect:definition-source-form-path def-source))
         (char-offset (sb-introspect:definition-source-character-offset def-source))
         (plist (sb-introspect:definition-source-plist def-source)))
    (%json-object
     "file" (and pathname (namestring pathname))
     "line" (and (consp form-path) (integerp (first form-path)) (first form-path))
     "form_path" (and form-path (%json-array
                                  (mapcar (lambda (e)
                                            (if (integerp e) e (princ-to-string e)))
                                          form-path)))
     "char_offset" char-offset
     "plist" (and plist (%safe-prin1 plist)))))

#+sbcl
(defun %find-definitions (symbol kind-kw)
  "Return a list of JSON objects describing where SYMBOL is defined.
KIND-KW filters by kind (:function, :method, :macro, :class, :variable,
:condition, ...) or NIL for all kinds known to sb-introspect."
  (handler-case
      (let* ((kinds (or (and kind-kw (list kind-kw))
                        '(:function :method :generic-function :macro
                          :class :condition :type :variable :constant
                          :package :method-combination :symbol-macro
                          :compiler-macro :setf-expander :alien-type)))
             (entries '()))
        (dolist (k kinds (nreverse entries))
          (dolist (src (sb-introspect:find-definition-sources-by-name
                        symbol k))
            (push (list :object
                        (list (cons "kind" (string-downcase (symbol-name k)))
                              (cons "location"
                                    (%definition-source-json src))))
                  entries))))
    (error () nil)))

#-sbcl
(defun %find-definitions (symbol kind-kw)
  (declare (ignore symbol kind-kw))
  nil)

(%register-method
 (make-method-spec
  :name "find-definition"
  :summary "Return source locations where SYMBOL is defined."
  :doc "Required: `symbol' (string). Optional: `package' (defaults to the
persistent package), `kind' (one of \"function\", \"method\", \"macro\",
\"class\", \"variable\", \"condition\", ...). Returns
`{entries: [{kind, location: {file, line, ...}}, ...]}'."
  :params (list (list :name "symbol" :type :string :required t
                      :description "Symbol name; matched case-insensitively.")
                (list :name "package" :type :string :required nil
                      :description "Resolve `symbol' in this package.")
                (list :name "kind" :type :string :required nil
                      :description "Filter to one definition kind."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((sym-name (%json-getf params "symbol"))
           (pkg-name (%json-getf params "package"))
           (kind-str (%json-getf params "kind"))
           (kind-kw (and (stringp kind-str)
                         (intern (string-upcase kind-str) :keyword)))
           (sym (and (stringp sym-name)
                     (%symbol-from-string sym-name pkg-name
                                          :server server))))
      (cond
        ((not (stringp sym-name))
         (%error-response id "protocol-error" "missing `symbol' param"))
        ((null sym)
         (%error-response id "eval-error"
                          (format nil "no symbol ~A in ~A"
                                  sym-name (or pkg-name "current package"))))
        (t
         (%success-response
          id
          (%json-object
           "entries"
           (%json-array (%find-definitions sym kind-kw))))))))))

#+sbcl
(defun %xref-entries (symbol direction)
  "Return JSON entries for an xref query.
DIRECTION is one of :callers, :callees, :references, :sets, :binds,
:macroexpands, :specializes."
  (handler-case
      (let ((raw
              (case direction
                (:callers (sb-introspect:who-calls symbol))
                (:references (sb-introspect:who-references symbol))
                (:sets (sb-introspect:who-sets symbol))
                (:binds (sb-introspect:who-binds symbol))
                (:macroexpands (sb-introspect:who-macroexpands symbol))
                (:specializes (sb-introspect:who-specializes-directly symbol))
                (:callees
                 ;; sb-introspect doesn't expose who-callees directly; fall
                 ;; back to function-precedence + lambda-list inspection.
                 nil)
                (t nil))))
        (loop for entry in raw
              collect
              ;; sb-introspect returns (NAME . DEFINITION-SOURCE) dotted
              ;; pairs, not proper (NAME DEFINITION-SOURCE) lists. (cdr
              ;; entry) is the definition-source; (second entry) would
              ;; try (car <struct>) and signal an error that gets
              ;; swallowed by the surrounding handler-case below.
              (let* ((name (if (consp entry) (car entry) entry))
                     (src (when (consp entry) (cdr entry))))
                (%json-object
                 "name" (%safe-prin1 name)
                 "location" (and src (%definition-source-json src))))))
    (error () nil)))

#-sbcl
(defun %xref-entries (symbol direction)
  (declare (ignore symbol direction))
  nil)

(%register-method
 (make-method-spec
  :name "xref"
  :summary "Cross-reference: who calls / references / sets / binds SYMBOL."
  :doc "Required: `symbol', `direction' (callers|callees|references|sets|binds|
macroexpands|specializes). Returns
`{entries: [{name, location}, ...]}'."
  :params (list (list :name "symbol" :type :string :required t
                      :description "Symbol name.")
                (list :name "direction" :type :string :required t
                      :description "callers, callees, references, sets, binds, macroexpands, specializes.")
                (list :name "package" :type :string :required nil
                      :description "Resolve `symbol' in this package."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((sym-name (%json-getf params "symbol"))
           (dir-str (%json-getf params "direction"))
           (pkg-name (%json-getf params "package"))
           (sym (and (stringp sym-name)
                     (%symbol-from-string sym-name pkg-name :server server)))
           (dir-kw (and (stringp dir-str)
                        (intern (string-upcase dir-str) :keyword))))
      (cond
        ((not (stringp sym-name))
         (%error-response id "protocol-error" "missing `symbol' param"))
        ((null sym)
         (%error-response id "eval-error"
                          (format nil "no symbol ~A" sym-name)))
        ((null dir-kw)
         (%error-response id "protocol-error" "missing `direction' param"))
        (t
         (%success-response
          id
          (%json-object
           "entries"
           (%json-array (%xref-entries sym dir-kw))))))))))

(%register-method
 (make-method-spec
  :name "macroexpand"
  :summary "Expand a macro form once or fully."
  :doc "Required: `form' (Lisp source string). Optional: `recursive'
(boolean, defaults to false). Returns `{form: <prin1>, expanded_p: bool}'."
  :params (list (list :name "form" :type :string :required t
                      :description "Form to expand.")
                (list :name "recursive" :type :boolean :required nil
                      :description "Fully expand (macroexpand) vs one step.")
                (list :name "package" :type :string :required nil
                      :description "Reader/expander package."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((form-text (%json-getf params "form"))
           (recursive (%json-getf params "recursive"))
           (pkg-name (%json-getf params "package"))
           (pkg (or (and pkg-name (%find-package-loose pkg-name))
                    (and server (server-current-package server))
                    (find-package "COMMON-LISP-USER"))))
      (cond
        ((not (stringp form-text))
         (%error-response id "protocol-error" "missing `form' param"))
        (t
         (handler-case
             (let* ((parsed (let ((*package* pkg))
                              (%read-form form-text))))
               (multiple-value-bind (expansion expanded-p)
                   (if recursive
                       (macroexpand parsed)
                       (macroexpand-1 parsed))
                 (%success-response
                  id
                  (%json-object
                   "form" (let ((*package* pkg)
                                (*print-pretty* nil))
                            (%safe-prin1 expansion))
                   "expanded_p" (and expanded-p t)))))
           (error (c)
             (%error-response id "eval-error" (princ-to-string c))))))))))

;;; ----------------------------------------------------------------------------
;;; compile-file / load-file with structured diagnostics (#130, #131).
;;; ----------------------------------------------------------------------------

(defun %compile-condition-severity (condition)
  (cond
    ((typep condition 'error) "error")
    ((typep condition 'sb-ext:compiler-note) "note")
    ((typep condition 'style-warning) "style-warning")
    ((typep condition 'warning) "warning")
    (t "note")))

(defun %compile-source-json (condition)
  "Best-effort source location for a compiler condition. SBCL stores it
in sb-c::source-form / sb-c::source-path on the condition, but those
accessors are internal; we princ the condition's source and let the
caller diff source-form strings."
  (handler-case
      (let* ((source-form (when (slot-exists-p condition 'sb-c::source-form)
                            (slot-value condition 'sb-c::source-form))))
        (when source-form
          (%json-object "form" (%safe-prin1 source-form))))
    (error () nil)))

(defun %emit-compile-diagnostic (ctx condition)
  "Emit one `event:diagnostic' frame for CONDITION."
  (when ctx
    (%emit-event
     ctx "diagnostic"
     "severity" (%compile-condition-severity condition)
     "type" (string (type-of condition))
     "message" (handler-case (princ-to-string condition)
                 (error () "<unprintable>"))
     "source" (%compile-source-json condition))))

(%register-method
 (make-method-spec
  :name "compile-file"
  :summary "Compile a Lisp file, streaming structured diagnostics."
  :doc "Required: `path' (absolute path to a .lisp file). Streams a
`diagnostic' event per condition signaled during compilation. Terminal
result carries `success', `output_truename', `warnings_p', `failure_p'."
  :params (list (list :name "path" :type :string :required t
                      :description "Absolute path to the source file."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore server))
    (let ((path (%json-getf params "path")))
      (cond
        ((not (stringp path))
         (%error-response id "protocol-error" "missing `path' param"))
        (t
         (handler-case
             (let ((handler (lambda (c) (%emit-compile-diagnostic ctx c))))
               (multiple-value-bind (truename warnings-p failure-p)
                   (handler-bind ((condition handler))
                     (compile-file path :verbose nil :print nil))
                 (%success-response
                  id
                  (%json-object
                   "success" (not failure-p)
                   "output_truename" (and truename (namestring truename))
                   "warnings_p" (and warnings-p t)
                   "failure_p" (and failure-p t)))))
           (error (c)
             (%error-response id "eval-error" (princ-to-string c))))))))))

;;; ----------------------------------------------------------------------------
;;; Introspection (#140-#148)
;;; ----------------------------------------------------------------------------

(defun %symbol-kinds (sym)
  "Return a list of keyword tags describing which namespaces SYM
inhabits: :function, :macro, :generic-function, :special-operator,
:variable, :constant, :class, :type, :package."
  (let ((tags '()))
    (when (fboundp sym)
      (cond
        ((special-operator-p sym) (push :special-operator tags))
        ((macro-function sym) (push :macro tags))
        ((and (fboundp sym) (typep (fdefinition sym) 'generic-function))
         (push :generic-function tags))
        (t (push :function tags))))
    (when (boundp sym)
      (push (if (constantp sym) :constant :variable) tags))
    (when (find-class sym nil)
      (push :class tags))
    (when (find-package sym) (push :package tags))
    (nreverse tags)))

(defun %apropos-entries (pattern pkg &optional server)
  "Build [{name, package, kinds, external}, ...] for symbols matching PATTERN.
PKG is a package object or NIL (search all)."
  (let ((upat (string-upcase pattern))
        (entries '()))
    (flet ((add (sym)
             (let* ((name (symbol-name sym))
                    (sym-pkg (symbol-package sym))
                    (public-pkg (%public-package-list-name sym-pkg server))
                    (external? (and sym-pkg
                                    (eq :external
                                        (nth-value 1
                                                   (find-symbol name sym-pkg))))))
               (when public-pkg
                 (push (list :object
                             (list (cons "name" name)
                                   (cons "package" public-pkg)
                                   (cons "kinds"
                                         (%json-array
                                          (mapcar (lambda (k)
                                                    (string-downcase
                                                     (symbol-name k)))
                                                  (%symbol-kinds sym))))
                                   (cons "external" (and external? t))))
                       entries)))))
      (cond
        (pkg
         (do-symbols (s pkg)
           (when (search upat (symbol-name s)) (add s))))
        (t
         ;; Walk every package and visit each symbol *once* by gating on
         ;; its home package. This matches the standard `apropos' --
         ;; internal symbols in CL-USER (where the REPL defines things)
         ;; show up too, not just exported ones.
         (dolist (p (list-all-packages))
           (do-symbols (s p)
             (when (and (eq (symbol-package s) p)
                        (search upat (symbol-name s)))
               (add s))))))
      ;; Dedupe by (name . package).
      (remove-duplicates (nreverse entries)
                         :test (lambda (a b)
                                 (and (string= (lookup* a "name")
                                               (lookup* b "name"))
                                      (string= (or (lookup* a "package") "")
                                               (or (lookup* b "package") ""))))))))

(defun lookup* (object key)
  (when (and (consp object) (eq (car object) :object))
    (cdr (assoc key (cadr object) :test #'string=))))

(%register-method
 (make-method-spec
  :name "apropos"
  :summary "Search symbols by name substring."
  :doc "Required: `pattern' (case-insensitive substring). Optional:
`package' restricts the search to one package (otherwise every
symbol -- internal and external -- across all packages, visited via
its home package). Returns `{entries: [{name, package, kinds,
external}, ...]}'."
  :params (list (list :name "pattern" :type :string :required t
                      :description "Substring to match, case-insensitive.")
                (list :name "package" :type :string :required nil
                      :description "Limit the search to one package."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((pat (%json-getf params "pattern"))
           (pkg-name (%json-getf params "package"))
           (pkg (and pkg-name
                     (%resolve-package-for-server server pkg-name))))
      (cond
        ((not (stringp pat))
         (%error-response id "protocol-error" "missing `pattern' param"))
        ((and pkg-name (null pkg))
         (%error-response id "eval-error"
                          (format nil "no such package: ~A" pkg-name)))
        (t
         (%success-response
          id
          (%json-object
           "entries"
           (%json-array (%apropos-entries pat pkg server))))))))))

(%register-method
 (make-method-spec
  :name "documentation"
  :summary "Return cl:documentation for SYMBOL of TYPE."
  :doc "Required: `symbol', `type' (function|variable|type|structure|
setf|method-combination|compiler-macro). Returns `{doc: <string or
null>}'."
  :params (list (list :name "symbol" :type :string :required t
                      :description "Symbol name.")
                (list :name "type" :type :string :required t
                      :description "Documentation type.")
                (list :name "package" :type :string :required nil
                      :description "Resolve `symbol' in this package."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((sym-name (%json-getf params "symbol"))
           (type-name (%json-getf params "type"))
           (pkg-name (%json-getf params "package"))
           (sym (and (stringp sym-name)
                     (%symbol-from-string sym-name pkg-name :server server)))
           ;; cl:documentation takes a CL symbol (function, variable, type,
           ;; ...) not a keyword. Intern into the CL package so the symbol
           ;; identity matches the method dispatch.
           (type-sym (and (stringp type-name)
                          (find-symbol (string-upcase type-name)
                                       (find-package "COMMON-LISP")))))
      (cond
        ((or (null sym) (null type-sym))
         (%error-response id "protocol-error"
                          "missing `symbol' or unknown `type' param"))
        (t
         (let ((doc (handler-case (documentation sym type-sym)
                      (error () nil))))
           (%success-response id (%json-object "doc" doc)))))))))

#+sbcl
(defun %arglist-of (sym)
  (handler-case (sb-introspect:function-lambda-list sym)
    (error () nil)))
#-sbcl
(defun %arglist-of (sym) (declare (ignore sym)) nil)

(%register-method
 (make-method-spec
  :name "arglist"
  :summary "Return the lambda-list of a function-bound symbol."
  :doc "Required: `symbol'. Optional: `package'. Returns `{arglist:
<prin1>, parsed: <list>}'. PARSED is a JSON array of strings, one per
element of the lambda list (including lambda-list keywords like
`&optional')."
  :params (list (list :name "symbol" :type :string :required t
                      :description "Function-bound symbol name.")
                (list :name "package" :type :string :required nil
                      :description "Resolve in this package."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((sym-name (%json-getf params "symbol"))
           (pkg-name (%json-getf params "package"))
           (sym (and (stringp sym-name)
                     (%symbol-from-string sym-name pkg-name :server server))))
      (cond
        ((or (null sym) (not (fboundp sym)))
         (%error-response id "eval-error"
                          (format nil "not fbound: ~A" sym-name)))
        (t
         (let ((arglist (%arglist-of sym)))
           (%success-response
            id
            (%json-object
             "arglist" (%safe-prin1 arglist)
             "parsed" (%json-array
                       (mapcar (lambda (e) (%safe-prin1 e)) arglist)))))))))))

(%register-method
 (make-method-spec
  :name "complete-symbol"
  :summary "List symbols whose name starts with PREFIX."
  :doc "Required: `prefix' (case-insensitive). Optional: `package'
(otherwise external symbols across all packages), `limit' (default 50)."
  :params (list (list :name "prefix" :type :string :required t
                      :description "Case-insensitive starts-with match.")
                (list :name "package" :type :string :required nil
                      :description "Limit to one package.")
                (list :name "limit" :type :integer :required nil
                      :description "Maximum candidates (default 50)."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore server ctx))
    (let* ((prefix (%json-getf params "prefix"))
           (pkg-name (%json-getf params "package"))
           (pkg (and pkg-name (%find-package-loose pkg-name)))
           (limit (or (%json-getf params "limit") 50))
           (upat (and (stringp prefix) (string-upcase prefix)))
           (names '()))
      (cond
        ((null upat)
         (%error-response id "protocol-error" "missing `prefix' param"))
        (t
         (flet ((add (s)
                  (let ((n (symbol-name s)))
                    (when (and (>= (length n) (length upat))
                               (string= upat n :end2 (length upat)))
                      (push n names)))))
           (cond
             (pkg (do-symbols (s pkg) (add s)))
             (t
              (dolist (p (list-all-packages))
                (do-external-symbols (s p) (add s))))))
         (let* ((unique (remove-duplicates names :test #'string=))
                (sorted (sort unique #'string<))
                (head (subseq sorted 0 (min limit (length sorted)))))
           (%success-response
            id
            (%json-object
             "candidates" (%json-array head)
             "total" (length unique)
             "truncated" (> (length sorted) limit))))))))))

(%register-method
 (make-method-spec
  :name "package-info"
  :summary "Describe a package: nicknames, use, used-by, exports."
  :doc "Required: `name'. Returns nicknames, use list, used-by list,
external symbol count, and a small head of exported symbols."
  :params (list (list :name "name" :type :string :required t
                      :description "Package name."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((name (%json-getf params "name"))
           (pkg (%resolve-package-for-server server name)))
      (cond
        ((null pkg)
         (%error-response id "eval-error"
                          (format nil "no such package: ~A" name)))
        (t
         (let ((exports '())
               (export-count 0))
           (do-external-symbols (s pkg)
             (incf export-count)
             (when (< (length exports) 100)
               (push (symbol-name s) exports)))
           (%success-response
            id
            (%json-object
             "name" (%public-package-name pkg server)
             "nicknames" (%json-array (package-nicknames pkg))
             "use" (%json-array
                    (remove nil
                            (mapcar (lambda (p)
                                      (%public-package-list-name p server))
                                    (package-use-list pkg))))
             "used_by" (%json-array
                        (remove nil
                                (mapcar (lambda (p)
                                          (%public-package-list-name p server))
                                        (package-used-by-list pkg))))
             "export_count" export-count
             "exports_head" (%json-array (sort exports #'string<)))))))))))

(%register-method
 (make-method-spec
  :name "class-info"
  :summary "Describe a CL class: supers, subs, precedence, slots."
  :doc "Required: `name' (symbol). Returns direct supers, direct subs,
precedence list, slot specs (name, type, initform, accessors)."
  :params (list (list :name "name" :type :string :required t
                      :description "Class symbol.")
                (list :name "package" :type :string :required nil
                      :description "Resolve in this package."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((name (%json-getf params "name"))
           (pkg-name (%json-getf params "package"))
           (sym (and (stringp name)
                     (%symbol-from-string name pkg-name :server server)))
           (class (and sym (find-class sym nil))))
      (cond
        ((null class)
         (%error-response id "eval-error"
                          (format nil "no such class: ~A" name)))
        (t
         (handler-case
             #+sbcl
           (progn
             (sb-mop:finalize-inheritance class)
             (let* ((direct-supers
                      (mapcar (lambda (c) (symbol-name (class-name c)))
                              (sb-mop:class-direct-superclasses class)))
                    (direct-subs
                      (mapcar (lambda (c) (symbol-name (class-name c)))
                              (sb-mop:class-direct-subclasses class)))
                    (precedence
                      (mapcar (lambda (c) (symbol-name (class-name c)))
                              (sb-mop:class-precedence-list class)))
                    (slots
                      (loop for slot in (sb-mop:class-direct-slots class)
                            collect
                            (%json-object
                             "name" (symbol-name
                                     (sb-mop:slot-definition-name slot))
                             "type" (%safe-prin1
                                     (sb-mop:slot-definition-type slot))
                             "initform" (let ((iff (sb-mop:slot-definition-initform slot)))
                                          (and iff (%safe-prin1 iff)))
                             "readers" (%json-array
                                        (mapcar #'symbol-name
                                                (sb-mop:slot-definition-readers slot)))
                             "writers" (%json-array
                                        (mapcar #'%safe-prin1
                                                (sb-mop:slot-definition-writers slot)))))))
               (%success-response
                id
                (%json-object
                 "name" (symbol-name (class-name class))
                 "direct_supers" (%json-array direct-supers)
                 "direct_subs" (%json-array direct-subs)
                 "precedence" (%json-array precedence)
                 "slots" (%json-array slots)))))
           #-sbcl
           (%error-response id "eval-error" "class-info is SBCL-only")
           (error (c)
             (%error-response id "eval-error" (princ-to-string c))))))))))

(%register-method
 (make-method-spec
  :name "function-info"
  :summary "Combined arglist, documentation, and known types for a function."
  :doc "Required: `symbol'. Returns arglist, function-type (if known),
documentation, and inline-p."
  :params (list (list :name "symbol" :type :string :required t
                      :description "Function-bound symbol.")
                (list :name "package" :type :string :required nil
                      :description "Resolve in this package."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((sym-name (%json-getf params "symbol"))
           (pkg-name (%json-getf params "package"))
           (sym (and (stringp sym-name)
                     (%symbol-from-string sym-name pkg-name :server server))))
      (cond
        ((or (null sym) (not (fboundp sym)))
         (%error-response id "eval-error"
                          (format nil "not fbound: ~A" sym-name)))
        (t
         (%success-response
          id
          (%json-object
           "name" (symbol-name sym)
           "package" (and (symbol-package sym)
                          (%public-package-name
                           (symbol-package sym) server))
           "arglist" (%safe-prin1 (%arglist-of sym))
           "documentation" (handler-case (documentation sym 'function)
                             (error () nil))
           "function_type"
           #+sbcl (handler-case
                      (%safe-prin1 (sb-introspect:function-type sym))
                    (error () nil))
           #-sbcl nil
           "macro_p" (and (macro-function sym) t)
           "generic_p" (and (fboundp sym)
                            (typep (fdefinition sym) 'generic-function)
                            t)))))))))

(%register-method
 (make-method-spec
  :name "disassemble"
  :summary "Capture the disassembly of a function."
  :doc "Required: `symbol'. Captures cl:disassemble's stdout into the
response. Bounded by the daemon's 1 MB output cap."
  :params (list (list :name "symbol" :type :string :required t
                      :description "Function symbol.")
                (list :name "package" :type :string :required nil
                      :description "Resolve in this package."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((sym-name (%json-getf params "symbol"))
           (pkg-name (%json-getf params "package"))
           (sym (and (stringp sym-name)
                     (%symbol-from-string sym-name pkg-name :server server))))
      (cond
        ((or (null sym) (not (fboundp sym)))
         (%error-response id "eval-error"
                          (format nil "not fbound: ~A" sym-name)))
        (t
         (let ((out (with-output-to-string (s)
                      (handler-case (disassemble sym :stream s)
                        (error (c) (format s "~A" c))))))
           (%success-response id (%json-object "output" out)))))))))

(%register-method
 (make-method-spec
  :name "describe-system"
  :summary "ASDF reflection: a system's components, deps, source root."
  :doc "Required: `name'. Returns the system's primary metadata as
recorded by ASDF, plus its declared and resolved dependencies."
  :params (list (list :name "name" :type :string :required t
                      :description "ASDF system name."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore server ctx))
    (let* ((name (%json-getf params "name"))
           (sys (and (stringp name)
                     (handler-case (asdf:find-system name nil)
                       (error () nil)))))
      (cond
        ((null sys)
         (%error-response id "eval-error"
                          (format nil "no such system: ~A" name)))
        (t
         (%success-response
          id
          (%json-object
           "name" (asdf:component-name sys)
           "version" (handler-case (asdf:component-version sys)
                       (error () nil))
           "source_directory"
           (handler-case (namestring (asdf:system-source-directory sys))
             (error () nil))
           "depends_on" (%json-array
                         (mapcar (lambda (d)
                                   (if (consp d) (%safe-prin1 d)
                                       (princ-to-string d)))
                                 (asdf:component-sideway-dependencies sys)))
           "license" (handler-case (asdf:system-licence sys)
                       (error () nil))
           "description" (handler-case (asdf:system-description sys)
                           (error () nil))
           "author" (handler-case (asdf:system-author sys)
                      (error () nil))))))))))

;;; ----------------------------------------------------------------------------
;;; Inspector (#120-#125)
;;;
;;; An inspector session is a stack of focused values. Each call to
;;; inspect-into pushes a new frame; inspect-pop pops one. The session
;;; renders its current top-of-stack as a list of "parts" -- one entry
;;; per slot / element / kv pair -- shaped to the underlying type.
;;; ----------------------------------------------------------------------------

(defstruct inspector-session
  id              ; string "ins-N"
  ;; Stack: list of values, newest first. (car STACK) is the current focus.
  stack
  ;; Whether mutate is permitted (opt-in per session).
  mutable?
  ;; Pagination state: a plist :offset N (default 0).
  view-state)

(defun %allocate-inspector (server value mutable?)
  (clpm.repl.compat:with-mutex
      ((server-inspectors-mutex server))
    (incf (server-inspector-counter server))
    (let* ((id (format nil "ins-~A" (server-inspector-counter server)))
           (sess (make-inspector-session
                  :id id :stack (list value)
                  :mutable? (and mutable? t)
                  :view-state (list :offset 0))))
      (setf (gethash id (server-inspectors server)) sess)
      sess)))

(defun %lookup-inspector (server id)
  (clpm.repl.compat:with-mutex
      ((server-inspectors-mutex server))
    (gethash id (server-inspectors server))))

(defun %drop-inspector (server id)
  (clpm.repl.compat:with-mutex
      ((server-inspectors-mutex server))
    (remhash id (server-inspectors server))))

(defun %inspector-current (sess)
  (first (inspector-session-stack sess)))

(defparameter +inspector-page-size+ 100)
(defparameter +inspector-print-length+ 64)
(defparameter +inspector-print-level+ 4)

(defun %inspect-part-repr (value)
  (let ((*print-length* +inspector-print-length+)
        (*print-level* +inspector-print-level+)
        (*print-pretty* nil))
    (%safe-prin1 value)))

(defun %proper-list-p (x)
  "T if X is a proper list (NIL or a cons whose final cdr is NIL)."
  (or (null x)
      (and (consp x)
           (handler-case (and (listp (cdr (last x))) t)
             (error () nil)))))

(defun %inspector-parts (value offset)
  "Render parts for VALUE starting from OFFSET. Returns
(values parts kind total-count). Pagination is the caller's job: this
slices [offset, offset+page-size)."
  (let ((page +inspector-page-size+))
    (cond
      ;; Proper lists: render as indexed elements.
      ((%proper-list-p value)
       (let* ((len (length value))
              (start (max 0 (min offset len)))
              (end (min (+ start page) len))
              (parts (loop for i from start below end
                           collect (%json-object
                                    "i" i
                                    "label" (princ-to-string i)
                                    "repr" (%inspect-part-repr (nth i value))
                                    "kind" "elem"))))
         (values parts "list" len)))
      ;; Dotted pair: car/cdr.
      ((consp value)
       (let ((parts (list
                     (%json-object "i" 0 "label" "car"
                                   "repr" (%inspect-part-repr (car value))
                                   "kind" "car")
                     (%json-object "i" 1 "label" "cdr"
                                   "repr" (%inspect-part-repr (cdr value))
                                   "kind" "cdr"))))
         (values parts "cons" 2)))
      (t
       (typecase value
      (string
       (let* ((len (length value))
              (start (max 0 (min offset len)))
              (end (min (+ start page) len))
              (parts (loop for i from start below end
                           collect (%json-object
                                    "i" i
                                    "label" (princ-to-string i)
                                    "repr" (%inspect-part-repr (char value i))
                                    "kind" "char"))))
         (values parts "string" len)))
      (vector
       (let* ((len (length value))
              (start (max 0 (min offset len)))
              (end (min (+ start page) len))
              (parts (loop for i from start below end
                           collect (%json-object
                                    "i" i
                                    "label" (princ-to-string i)
                                    "repr" (%inspect-part-repr (aref value i))
                                    "kind" "elem"))))
         (values parts "vector" len)))
      (list
       (let* ((len (length value))
              (start (max 0 (min offset len)))
              (end (min (+ start page) len))
              (parts (loop for i from start below end
                           collect (%json-object
                                    "i" i
                                    "label" (princ-to-string i)
                                    "repr" (%inspect-part-repr (nth i value))
                                    "kind" "elem"))))
         (values parts "list" len)))
      (hash-table
       (let ((entries '())
             (i 0))
         (maphash
          (lambda (k v)
            (when (and (>= i offset) (< i (+ offset page)))
              (push (%json-object
                     "i" i
                     "label" (%inspect-part-repr k)
                     "repr" (%inspect-part-repr v)
                     "kind" "kv")
                    entries))
            (incf i))
          value)
         (values (nreverse entries) "hash-table" (hash-table-count value))))
      (symbol
       (let* ((parts
                (append
                 (list (%json-object "i" 0 "label" "name"
                                     "repr" (%safe-prin1 (symbol-name value))
                                     "kind" "name"))
                 (list (%json-object "i" 1 "label" "package"
                                     "repr" (and (symbol-package value)
                                                 (%public-package-name
                                                  (symbol-package value)))
                                     "kind" "package"))
                 (when (boundp value)
                   (list (%json-object "i" 2 "label" "value"
                                       "repr" (%inspect-part-repr
                                               (symbol-value value))
                                       "kind" "value")))
                 (when (fboundp value)
                   (list (%json-object "i" 3 "label" "function"
                                       "repr" (%inspect-part-repr
                                               (symbol-function value))
                                       "kind" "function")))
                 (let ((p (symbol-plist value)))
                   (when p
                     (list (%json-object "i" 4 "label" "plist"
                                         "repr" (%inspect-part-repr p)
                                         "kind" "plist")))))))
         (values parts "symbol" (length parts))))
      (standard-object
       #+sbcl
       (let* ((class (class-of value))
              (slots (handler-case
                         (progn
                           (sb-mop:finalize-inheritance class)
                           (sb-mop:class-slots class))
                       (error () nil)))
              (parts (loop for slot in slots
                           for idx from 0
                           for name = (sb-mop:slot-definition-name slot)
                           collect
                           (%json-object
                            "i" idx
                            "label" (symbol-name name)
                            "repr" (if (slot-boundp value name)
                                       (%inspect-part-repr
                                        (slot-value value name))
                                       "<unbound>")
                            "kind" "slot"))))
         (values parts "instance" (length parts)))
       #-sbcl
       (values nil "instance" 0))
      (structure-object
       #+sbcl
       (let* ((class (class-of value))
              (slots (handler-case
                         (progn
                           (sb-mop:finalize-inheritance class)
                           (sb-mop:class-slots class))
                       (error () nil)))
              (parts (loop for slot in slots
                           for idx from 0
                           for name = (sb-mop:slot-definition-name slot)
                           collect
                           (%json-object
                            "i" idx
                            "label" (symbol-name name)
                            "repr" (if (slot-boundp value name)
                                       (%inspect-part-repr
                                        (slot-value value name))
                                       "<unbound>")
                            "kind" "slot"))))
         (values parts "struct" (length parts)))
       #-sbcl
       (values nil "struct" 0))
      (t
       ;; For atoms (numbers, characters, ...), `type-of' may return a
       ;; compound type specifier like `(INTEGER 0 4611686018427387903)'.
       ;; `string' chokes on that, so use the head symbol when it's a
       ;; cons and only stringify when we actually have a symbol.
       (let* ((ty (type-of value))
              (head (if (consp ty) (car ty) ty))
              (label (if (symbolp head)
                         (string-downcase (symbol-name head))
                         (princ-to-string head))))
         (values nil label 0))))))))

(defun %inspector-into (sess i)
  "Push the i-th part of the current focus onto the stack. Returns the
new focus, or :no-part on out-of-range."
  (let ((focus (%inspector-current sess)))
    (typecase focus
      (cons (case i
              (0 (push (car focus) (inspector-session-stack sess))
                 (car focus))
              (1 (push (cdr focus) (inspector-session-stack sess))
                 (cdr focus))
              (t :no-part)))
      (string
       (cond ((and (integerp i) (<= 0 i (1- (length focus))))
              (push (char focus i) (inspector-session-stack sess))
              (char focus i))
             (t :no-part)))
      (vector
       (cond ((and (integerp i) (<= 0 i (1- (length focus))))
              (push (aref focus i) (inspector-session-stack sess))
              (aref focus i))
             (t :no-part)))
      (list
       (cond ((and (integerp i) (<= 0 i (1- (length focus))))
              (push (nth i focus) (inspector-session-stack sess))
              (nth i focus))
             (t :no-part)))
      (hash-table
       (let ((found :no-part))
         (let ((j 0))
           (maphash (lambda (k v)
                      (when (= j i)
                        (push (cons k v) (inspector-session-stack sess))
                        (setf found (cons k v)))
                      (incf j))
                    focus))
         found))
      (standard-object
       #+sbcl
       (handler-case
           (let* ((class (class-of focus))
                  (slots (progn (sb-mop:finalize-inheritance class)
                                (sb-mop:class-slots class))))
             (cond
               ((and (integerp i) (< i (length slots)))
                (let* ((slot (nth i slots))
                       (name (sb-mop:slot-definition-name slot))
                       (v (if (slot-boundp focus name)
                              (slot-value focus name)
                              :unbound)))
                  (push v (inspector-session-stack sess))
                  v))
               (t :no-part)))
         (error () :no-part)))
      (t :no-part))))

(defun %inspector-render (sess)
  "Build the v2 JSON payload describing SESS's current view."
  (let* ((focus (%inspector-current sess))
         (offset (or (getf (inspector-session-view-state sess) :offset) 0)))
    (multiple-value-bind (parts kind total)
        (%inspector-parts focus offset)
      (%json-object
       "session" (inspector-session-id sess)
       "value_repr" (%inspect-part-repr focus)
       "type" (string-downcase
               (handler-case (princ-to-string (type-of focus))
                 (error () "?")))
       "kind" kind
       "parts" (%json-array parts)
       "offset" offset
       "total" total
       "actions" (%json-array
                  (append '("into" "pop" "eval" "page")
                          (when (inspector-session-mutable? sess)
                            '("mutate"))
                          '("close")))
       "depth" (length (inspector-session-stack sess))))))

;;; ----------------------------------------------------------------------------
;;; Image and ASDF management (#190-#194)
;;; ----------------------------------------------------------------------------

(%register-method
 (make-method-spec
  :name "image-info"
  :summary "Daemon vitals: pid, lisp, uptime, features, GC, working dir."
  :doc "No parameters. Useful for `the daemon is in state X' diagnostics."
  :params nil
  :handler
  (lambda (server params id ctx)
    (declare (ignore params ctx))
    (%success-response
     id
     (%json-object
      "pid" (clpm.repl.compat:getpid)
      "lisp" (format nil "~A ~A"
                     (lisp-implementation-type)
                     (lisp-implementation-version))
      "uptime_ms" (* 1000 (- (get-universal-time)
                             (server-started-at server)))
      "default_pathname" (namestring *default-pathname-defaults*)
      "working_directory"
      (namestring (or *default-pathname-defaults* (truename ".")))
      "features" (%json-array
                  (mapcar (lambda (f)
                            (if (keywordp f) (symbol-name f)
                                (princ-to-string f)))
                          *features*))
      "bytes_consed"
      #+sbcl (sb-ext:get-bytes-consed)
      #-sbcl 0
      "gc_run_time"
      #+sbcl sb-ext:*gc-real-time*
      #-sbcl 0
      "eval_count" (server-eval-count server))))))

(%register-method
 (make-method-spec
  :name "loaded-systems"
  :summary "ASDF systems currently loaded into the image."
  :doc "Returns each system's name, version (if any), and source
directory."
  :params nil
  :handler
  (lambda (server params id ctx)
    (declare (ignore server params ctx))
    (let ((entries '()))
      (asdf/session:with-asdf-session ()
        (asdf:map-systems
         (lambda (sys)
           (push (%json-object
                  "name" (asdf:component-name sys)
                  "version" (handler-case (asdf:component-version sys)
                              (error () nil))
                  "source_directory"
                  (handler-case (namestring (asdf:system-source-directory sys))
                    (error () nil)))
                 entries))))
      (%success-response
       id (%json-object "entries" (%json-array (nreverse entries))))))))

(%register-method
 (make-method-spec
  :name "list-packages"
  :summary "List every package with its symbol counts."
  :doc "Returns name, nicknames, external count, internal count."
  :params nil
  :handler
  (lambda (server params id ctx)
    (declare (ignore params ctx))
    (let ((entries
            (loop for pkg in (list-all-packages)
                  for public-name = (%public-package-list-name pkg server)
                  when public-name
                  collect
                  (let ((internal 0) (external 0))
                    (do-symbols (s pkg)
                      (multiple-value-bind (sym status)
                          (find-symbol (symbol-name s) pkg)
                        (declare (ignore sym))
                        (case status
                          (:internal (incf internal))
                          (:external (incf external)))))
                    (%json-object
                     "name" public-name
                     "nicknames" (%json-array (package-nicknames pkg))
                     "external" external
                     "internal" internal)))))
      (%success-response
       id (%json-object "entries" (%json-array entries)))))))

(%register-method
 (make-method-spec
  :name "gc"
  :summary "Trigger a GC, return bytes_consed before and after."
  :doc "Optional: `full' (boolean) for a full GC."
  :params (list (list :name "full" :type :boolean :required nil
                      :description "Full GC (default: minor)."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore server ctx))
    (let* ((full (%json-getf params "full"))
           (before #+sbcl (sb-ext:get-bytes-consed) #-sbcl 0))
      #+sbcl (sb-ext:gc :full full)
      #-sbcl (declare (ignore full))
      (%success-response
       id
       (%json-object
        "before_bytes" before
        "after_bytes" #+sbcl (sb-ext:get-bytes-consed) #-sbcl 0))))))

;;; ----------------------------------------------------------------------------
;;; Trace (#160-#162)
;;; ----------------------------------------------------------------------------

(defun %local-trace-enabled-p (entry server)
  (and entry server (gethash server (local-trace-entry-servers entry))))

(defun %local-trace-call (sym original args)
  (let ((enabled? nil)
        (server *server*))
    (clpm.repl.compat:with-mutex (*local-trace-registry-mutex*)
      (setf enabled?
            (%local-trace-enabled-p
             (gethash sym *local-trace-registry*)
             server)))
    (when enabled?
      (format *trace-output* "~&~S <=~{ ~S~}~%" sym args)
      (force-output *trace-output*))
    (apply original args)))

(defun %all-server-traces (server)
  (clpm.repl.compat:with-mutex (*local-trace-registry-mutex*)
    (let ((symbols '()))
      (maphash (lambda (sym _enabled)
                 (declare (ignore _enabled))
                 (push sym symbols))
               (server-traces server))
      (sort symbols #'string< :key #'%safe-prin1))))

(defun %trace-symbol (sym-name pkg-name &key (server *server*))
  "Resolve SYM-NAME in PKG-NAME and trace it for SERVER only."
  (let ((sym (%symbol-from-string sym-name pkg-name :server server)))
    (cond
      ((or (null sym) (not (fboundp sym))) nil)
      ((null server) nil)
      (t
       (clpm.repl.compat:with-mutex (*local-trace-registry-mutex*)
         (let ((entry (gethash sym *local-trace-registry*)))
           (unless entry
             (let ((original (fdefinition sym)))
               (setf entry
                     (make-local-trace-entry
                      :original original
                      :servers (make-hash-table :test 'eq)))
               (setf (gethash sym *local-trace-registry*) entry)
               (setf (fdefinition sym)
                     (let ((captured-original original))
                       (lambda (&rest args)
                         (%local-trace-call sym captured-original args))))))
           (setf (gethash server (local-trace-entry-servers entry)) t)
           (setf (gethash sym (server-traces server)) t)))
       t))))

(defun %untrace-symbol (sym-name pkg-name &key (server *server*))
  (let ((sym (%symbol-from-string sym-name pkg-name :server server)))
    (cond
      ((or (null sym) (null server)) nil)
      (t
       (clpm.repl.compat:with-mutex (*local-trace-registry-mutex*)
         (let ((entry (gethash sym *local-trace-registry*)))
           (cond
             ((null entry) nil)
             (t
              (remhash server (local-trace-entry-servers entry))
              (remhash sym (server-traces server))
              (when (zerop (hash-table-count (local-trace-entry-servers entry)))
                (setf (fdefinition sym) (local-trace-entry-original entry))
                (remhash sym *local-trace-registry*))
              t))))))))

(defun %untrace-all-for-server (server)
  (dolist (sym (%all-server-traces server))
    (%untrace-symbol (symbol-name sym) (package-name (symbol-package sym))
                     :server server)))

(%register-method
 (make-method-spec
  :name "trace"
  :summary "Trace one or more functions in this daemon."
  :doc "Required: `symbols' (array of symbol names). Optional:
`package' (resolves all names in this package). Trace state is scoped to
this daemon; another project daemon in the same host Lisp does not list or
emit these traces."
  :params (list (list :name "symbols" :type :array :required t
                      :description "Symbol names to trace.")
                (list :name "package" :type :string :required nil
                      :description "Resolve names in this package."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((syms-array (%json-getf params "symbols"))
           (names (array-items-of-param syms-array))
           (pkg-name (%json-getf params "package"))
           (traced '())
           (missing '()))
      (cond
        ((not (listp names))
         (%error-response id "protocol-error" "missing `symbols' array"))
        (t
         (dolist (n names)
           (cond
             ((not (stringp n)) (push n missing))
             ((%trace-symbol n pkg-name :server server) (push n traced))
             (t (push n missing))))
         (%success-response
          id
          (%json-object
           "traced" (%json-array (nreverse traced))
           "missing" (%json-array (nreverse missing))))))))))

(%register-method
 (make-method-spec
  :name "untrace"
  :summary "Remove daemon-local tracing from symbols (or all if none given)."
  :doc "Optional: `symbols' (array). With no symbols, untraces
everything traced by this daemon."
  :params (list (list :name "symbols" :type :array :required nil
                      :description "Symbol names; default: untrace all.")
                (list :name "package" :type :string :required nil
                      :description "Resolve names in this package."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((syms-array (%json-getf params "symbols"))
           (names (array-items-of-param syms-array))
           (pkg-name (%json-getf params "package")))
      (cond
        ((null names)
         (%untrace-all-for-server server)
         (%success-response id (%json-object "untraced_all" t)))
        (t
         (let ((untraced '()))
           (dolist (n names)
             (when (stringp n)
               (when (%untrace-symbol n pkg-name :server server)
                 (push n untraced))))
           (%success-response
            id
            (%json-object "untraced" (%json-array (nreverse untraced)))))))))))

(%register-method
 (make-method-spec
  :name "list-traced"
  :summary "Return this daemon's traced functions."
  :doc "No parameters."
  :params nil
  :handler
  (lambda (server params id ctx)
    (declare (ignore params ctx))
    (let ((entries
            (mapcar #'%safe-prin1 (%all-server-traces server))))
      (%success-response id
                         (%json-object "entries" (%json-array entries)))))))

(%register-method
 (make-method-spec
  :name "inspect"
  :summary "Open an inspector session on the value of FORM."
  :doc "Required: `form'. Optional: `mutable' (boolean), `package'.
Returns the initial inspection view including a `session' id used by
subsequent inspect-* RPCs."
  :params (list (list :name "form" :type :string :required t
                      :description "Form whose value is inspected.")
                (list :name "mutable" :type :boolean :required nil
                      :description "Allow inspect-mutate.")
                (list :name "package" :type :string :required nil
                      :description "Reader package for FORM."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((form-text (%json-getf params "form"))
           (mutable (%json-getf params "mutable"))
           (pkg-name (%json-getf params "package"))
           (pkg (or (and pkg-name (%find-package-loose pkg-name))
                    (and server (server-current-package server))
                    (find-package "COMMON-LISP-USER"))))
      (cond
        ((not (stringp form-text))
         (%error-response id "protocol-error" "missing `form' param"))
        (t
         (handler-case
             (let* ((parsed (let ((*package* pkg))
                              (%read-form form-text)))
                    (value (let ((*package* pkg)) (eval parsed)))
                    (sess (%allocate-inspector server value mutable)))
               (%success-response id (%inspector-render sess)))
           (error (c)
             (%error-response id "eval-error" (princ-to-string c))))))))))

(%register-method
 (make-method-spec
  :name "inspect-into"
  :summary "Push the i-th part of the current focus onto the inspector stack."
  :doc "Required: `session', `i'. Returns the new view."
  :params (list (list :name "session" :type :string :required t
                      :description "Inspector session id.")
                (list :name "i" :type :integer :required t
                      :description "Part index to descend into."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((sid (%json-getf params "session"))
           (i (%json-getf params "i"))
           (sess (and (stringp sid) (%lookup-inspector server sid))))
      (cond
        ((null sess)
         (%error-response id "eval-error" "no such inspector session"))
        ((not (integerp i))
         (%error-response id "protocol-error" "missing `i' param"))
        (t
         (let ((res (%inspector-into sess i)))
           (cond
             ((eq res :no-part)
              (%error-response id "eval-error"
                               (format nil "no part ~A" i)))
             (t (%success-response id (%inspector-render sess)))))))))))

(%register-method
 (make-method-spec
  :name "inspect-pop"
  :summary "Pop one frame off the inspector stack."
  :doc "Required: `session'. Pops back to the previous focus. No-op
when the stack already only has one frame; returns that frame."
  :params (list (list :name "session" :type :string :required t
                      :description "Inspector session id."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((sid (%json-getf params "session"))
           (sess (and (stringp sid) (%lookup-inspector server sid))))
      (cond
        ((null sess)
         (%error-response id "eval-error" "no such inspector session"))
        (t
         (when (> (length (inspector-session-stack sess)) 1)
           (pop (inspector-session-stack sess))
           (setf (getf (inspector-session-view-state sess) :offset) 0))
         (%success-response id (%inspector-render sess))))))))

(%register-method
 (make-method-spec
  :name "inspect-eval"
  :summary "Evaluate FORM with `*' bound to the inspector's current focus."
  :doc "Required: `session', `form'. Returns `{value_repr: <prin1>}'
-- a single rendered value, not a new inspector view. The session is
*not* automatically descended into the result; use `inspect' on the
form again if you want to walk into it."
  :params (list (list :name "session" :type :string :required t
                      :description "Inspector session id.")
                (list :name "form" :type :string :required t
                      :description "Form to evaluate.")
                (list :name "package" :type :string :required nil
                      :description "Reader package for FORM."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((sid (%json-getf params "session"))
           (sess (and (stringp sid) (%lookup-inspector server sid)))
           (form-text (%json-getf params "form"))
           (pkg-name (%json-getf params "package"))
           (pkg (or (and pkg-name (%find-package-loose pkg-name))
                    (and server (server-current-package server))
                    (find-package "COMMON-LISP-USER"))))
      (cond
        ((null sess)
         (%error-response id "eval-error" "no such inspector session"))
        ((not (stringp form-text))
         (%error-response id "protocol-error" "missing `form' param"))
        (t
         (handler-case
             (let* ((focus (%inspector-current sess))
                    (parsed (let ((*package* pkg))
                              (%read-form form-text)))
                    (value (progv (list (find-symbol "*" "CL"))
                                  (list focus)
                             (let ((*package* pkg))
                               (eval parsed)))))
               (%success-response
                id
                (%json-object "value_repr" (%inspect-part-repr value))))
           (error (c)
             (%error-response id "eval-error" (princ-to-string c))))))))))

(%register-method
 (make-method-spec
  :name "inspect-mutate"
  :summary "Set part i of the focus to the value of FORM."
  :doc "Required: `session', `i', `form'. Requires the session to have
been opened with `mutable: true'. Returns the refreshed view."
  :params (list (list :name "session" :type :string :required t
                      :description "Inspector session id.")
                (list :name "i" :type :integer :required t
                      :description "Part index to overwrite.")
                (list :name "form" :type :string :required t
                      :description "Form whose value replaces the part."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((sid (%json-getf params "session"))
           (sess (and (stringp sid) (%lookup-inspector server sid)))
           (i (%json-getf params "i"))
           (form-text (%json-getf params "form")))
      (cond
        ((null sess)
         (%error-response id "eval-error" "no such inspector session"))
        ((not (inspector-session-mutable? sess))
         (%error-response id "protocol-error"
                          "session is not mutable; reopen with mutable: true"))
        ((or (not (integerp i)) (not (stringp form-text)))
         (%error-response id "protocol-error" "missing `i' or `form' param"))
        (t
         (handler-case
             (let* ((focus (%inspector-current sess))
                    (parsed (%read-form form-text))
                    (new-value (eval parsed)))
               (typecase focus
                 (cons (case i
                         (0 (setf (car focus) new-value))
                         (1 (setf (cdr focus) new-value))))
                 (vector (setf (aref focus i) new-value))
                 (hash-table
                  (let ((j 0) (target-key nil))
                    (maphash (lambda (k v)
                               (declare (ignore v))
                               (when (= j i) (setf target-key k))
                               (incf j))
                             focus)
                    (when target-key
                      (setf (gethash target-key focus) new-value))))
                 (standard-object
                  #+sbcl
                  (let* ((class (class-of focus))
                         (slots (progn (sb-mop:finalize-inheritance class)
                                       (sb-mop:class-slots class))))
                    (when (< i (length slots))
                      (setf (slot-value focus
                                        (sb-mop:slot-definition-name
                                         (nth i slots)))
                            new-value)))))
               (%success-response id (%inspector-render sess)))
           (error (c)
             (%error-response id "eval-error" (princ-to-string c))))))))))

(%register-method
 (make-method-spec
  :name "inspect-page"
  :summary "Set the page offset for the inspector view."
  :doc "Required: `session', `offset' (integer >= 0). Returns the
refreshed view."
  :params (list (list :name "session" :type :string :required t
                      :description "Inspector session id.")
                (list :name "offset" :type :integer :required t
                      :description "Starting index for the next page."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let* ((sid (%json-getf params "session"))
           (sess (and (stringp sid) (%lookup-inspector server sid)))
           (offset (%json-getf params "offset")))
      (cond
        ((null sess)
         (%error-response id "eval-error" "no such inspector session"))
        ((not (integerp offset))
         (%error-response id "protocol-error" "missing `offset' param"))
        (t
         (setf (getf (inspector-session-view-state sess) :offset)
               (max 0 offset))
         (%success-response id (%inspector-render sess))))))))

(%register-method
 (make-method-spec
  :name "inspect-close"
  :summary "Discard an inspector session."
  :doc "Required: `session'. Frees the server-side stack."
  :params (list (list :name "session" :type :string :required t
                      :description "Inspector session id."))
  :handler
  (lambda (server params id ctx)
    (declare (ignore ctx))
    (let ((sid (%json-getf params "session")))
      (cond
        ((not (stringp sid))
         (%error-response id "protocol-error" "missing `session' param"))
        (t
         (%drop-inspector server sid)
         (%success-response id (%json-object))))))))

(%register-method
 (make-method-spec
  :name "load-file"
  :summary "Load a Lisp file, streaming structured diagnostics."
  :doc "Required: `path'. Streams a `diagnostic' event per condition
signaled during load. Terminal result carries `success' and the package
active when load returned."
  :params (list (list :name "path" :type :string :required t
                      :description "Absolute path to the file to load."))
  :handler
  (lambda (server params id ctx)
    (let ((path (%json-getf params "path")))
      (cond
        ((not (stringp path))
         (%error-response id "protocol-error" "missing `path' param"))
        (t
         (handler-case
             (let ((handler (lambda (c) (%emit-compile-diagnostic ctx c))))
               (handler-bind ((condition handler))
                 (load path :verbose nil :print nil))
               (when server
                 (setf (server-current-package server) *package*))
               (%success-response
                id
                (%json-object
                 "success" t
                 "package" (%public-package-name *package* server))))
           (error (c)
             (%error-response id "eval-error" (princ-to-string c))))))))))

(defun %parse-eval-options (params)
  "Translate v2 eval params (a `(:object ((k . v)…))') into the worker's
plist of `:stream', `:debug', `:query-interactive', `:record-signals',
`:break-on', `:handlers', and `:print-*' options. Unknown fields are
ignored. Returns NIL when PARAMS is plain v1 (no v2 toggles), which
keeps the worker fast-path identical to v1."
  (let ((options '()))
    (flet ((maybe-bool (key plist-key)
             (let ((v (%json-getf params key 'unset)))
               (unless (eq v 'unset)
                 (setf options (list* plist-key (and v t) options)))))
           (maybe-int (key plist-key)
             (let ((v (%json-getf params key)))
               (when (integerp v)
                 (setf options (list* plist-key v options)))))
           (maybe-string (key plist-key)
             (let ((v (%json-getf params key)))
               (when (and (stringp v) (plusp (length v)))
                 (setf options (list* plist-key v options))))))
      (maybe-bool "stream" :stream)
      (maybe-bool "debug" :debug)
      (maybe-bool "query_interactive" :query-interactive)
      (maybe-bool "record_signals" :record-signals)
      (maybe-bool "concurrent" :concurrent)
      (maybe-string "worker" :worker)
      ;; #211: `break_on' accepts a type name ("error"), the special
      ;; strings "none" / "nil", or boolean false. The latter values
      ;; explicitly disable *break-on-signals* for the eval, overriding
      ;; any global default the daemon was started with.
      (let ((b (%json-getf params "break_on" 'unset)))
        (cond
          ((eq b 'unset))
          ((eq b :false)
           (setf options (list* :break-on :none options)))
          ((and (stringp b)
                (member (string-downcase b) '("none" "nil" "false")
                        :test #'string=))
           (setf options (list* :break-on :none options)))
          ((and (stringp b) (plusp (length b)))
           (setf options (list* :break-on b options)))))
      ;; "handlers" is an array of {type, restart, args} objects. We pass
      ;; the raw array form through; the eval path translates it.
      (let ((h (%json-getf params "handlers")))
        (when (and (consp h) (eq (car h) :array))
          (setf options (list* :handlers (cadr h) options))))
      (maybe-int "max_real_ms" :max-real-ms)
      (maybe-int "max_cons_bytes" :max-cons-bytes)
      (maybe-int "print_length" :print-length)
      (maybe-int "print_level"  :print-level)
      (maybe-bool "print_circle" :print-circle)
      (maybe-bool "print_radix"  :print-radix)
      (maybe-int "print_base"   :print-base)
      (maybe-bool "print_pretty" :print-pretty))
    options))

(defun %history-payload (history)
  "Convert the history alist returned by `%read-history-snapshot' into the
JSON object that ships in eval responses."
  (when history
    (list :object history)))

(defun %eval-success-payload (result &key worker-restarted?)
  "Build the JSON `result' object for a successful eval. Includes the v2
`values' array plus the v1-compatible scalar `value' (the primary value
as a prin1 string, or NIL for `(values)')."
  (let ((values (eval-result-values result)))
    (list :object
          (append
           (list (cons "value" (first values))
                 (cons "values" (%json-array values))
                 (cons "output" (eval-result-output result))
                 (cons "error_output" (eval-result-error-output result))
                 (cons "package" (eval-result-package result))
                 (cons "elapsed_ms" (eval-result-elapsed-ms result))
                 (cons "conditions" (%json-array (eval-result-conditions result))))
           (when (eval-result-signaled-conditions result)
             (list (cons "signaled_conditions"
                         (%json-array (eval-result-signaled-conditions result)))))
           (when (eval-result-handler-attempts result)
             (list (cons "handler_attempts"
                         (%json-array (eval-result-handler-attempts result)))))
           (when (eval-result-history result)
             (list (cons "history" (%history-payload (eval-result-history result)))))
           (when (eval-result-redefined result)
             (list (cons "redefined" (list :object (eval-result-redefined result)))))
           (when (eval-result-truncated? result)
             (list (cons "truncated" t)))
           (when worker-restarted?
             (list (cons "worker_restarted" t)))))))

(defun %dispatch-eval (server params id &optional ctx)
  (let* ((form (%json-getf params "form"))
         (package-override (%json-getf params "package"))
         (options (%parse-eval-options params))
         (concurrent? (getf options :concurrent))
         (worker-name (or (getf options :worker) +default-worker-name+))
         (worker (cond
                   (concurrent? (%fresh-concurrent-worker server))
                   (t (%ensure-worker server :name worker-name)))))
    (cond
      ((not (stringp form))
       (%error-response id "protocol-error" "missing `form` param"))
      (t
       (let* ((mailbox (worker-mailbox worker))
              (reply-box (clpm.repl.compat:make-mailbox))
              (query-box (and (getf options :query-interactive)
                              (clpm.repl.compat:make-mailbox)))
              (debug-box (and (getf options :debug)
                              (clpm.repl.compat:make-mailbox)))
              (job (make-eval-job
                    :form form
                    :package-override package-override
                    :options options
                    :ctx ctx
                    :query-mailbox query-box
                    :debug-mailbox debug-box
                    :result-mailbox reply-box)))
         (when ctx
           (setf (request-context-options ctx) options))
         (setf (worker-last-eval-id worker) id)
         (let ((restarted? (worker-restarted? worker)))
           ;; Consume the restart flag: the result marker ships only with
           ;; the first eval after a respawn, not every subsequent one.
           (when restarted? (setf (worker-restarted? worker) nil))
           ;; Register *before* posting to the worker so a continuation
           ;; message racing with `event:query' still finds the job.
           (when (and ctx (request-context-cstate ctx))
             (%register-in-flight (request-context-cstate ctx) id job))
           (incf (server-eval-count server))
           (clpm.repl.compat:send-message mailbox job)
           (let ((result
                   ;; #212: poll the reply mailbox so we can notice a
                   ;; worker thread that died mid-eval (and never gets
                   ;; to send us a result).
                   (loop
                     (let ((msg (clpm.repl.compat:receive-message-no-hang
                                 reply-box)))
                       (when (cdr msg) (return (car msg))))
                     (unless (clpm.repl.compat:thread-alive-p
                              (worker-thread worker))
                       (%log-event (server-event-log server) "worker-died"
                                   "worker" (worker-name worker)
                                   "id" id)
                       (setf (worker-state worker) :dead
                             (worker-current-job worker) nil)
                       (return (make-eval-result
                                :code "worker-died"
                                :values nil
                                :output ""
                                :error-output ""
                                :package (%public-package-name
                                          (worker-package worker) server)
                                :elapsed-ms 0
                                :conditions
                                (list (%json-object
                                       "type" "WORKER-DIED"
                                       "message"
                                       (format nil
                                               "worker '~A' died mid-eval"
                                               (worker-name worker))))
                                :signaled-conditions nil
                                :truncated? nil
                                :redefined nil
                                :history nil)))
                     (sleep 0.1))))
             (when (worker-concurrent? worker)
               ;; One-shot: teardown after the eval completes (success or
               ;; failure). Best-effort; if the worker is wedged the
               ;; supervisor can still see it via `list-workers'.
               (clpm.repl.compat:send-message mailbox :stop)
               (%remove-worker server (worker-name worker)))
             (cond
               ((null (eval-result-code result))
                (%success-response
                 id
                 (%eval-success-payload
                  result
                  :worker-restarted? restarted?)))
               (t
                (let ((details
                        (list :object
                              (append
                               (list (cons "output" (eval-result-output result))
                                     (cons "error_output" (eval-result-error-output result))
                                     (cons "package" (eval-result-package result))
                                     (cons "elapsed_ms" (eval-result-elapsed-ms result))
                                     (cons "conditions"
                                           (%json-array
                                            (eval-result-conditions result))))
                               (when (eval-result-signaled-conditions result)
                                 (list (cons "signaled_conditions"
                                             (%json-array
                                              (eval-result-signaled-conditions result)))))
                               (when (eval-result-handler-attempts result)
                                 (list (cons "handler_attempts"
                                             (%json-array
                                              (eval-result-handler-attempts result)))))))))
                  (%error-response id (eval-result-code result)
                                   (or (let ((c0 (first (eval-result-conditions result))))
                                         (when c0
                                           (cdr (assoc "message"
                                                       (cadr c0)
                                                       :test #'string=))))
                                       (eval-result-code result))
                                   :details details)))))))))))

(defun %dispatch-describe (server params id)
  (let* ((sym-name (%json-getf params "symbol"))
         (pkg-name (%json-getf params "package"))
         (pkg-default (server-current-package server)))
    (cond
      ((not (stringp sym-name))
       (%error-response id "protocol-error" "missing `symbol` param"))
      (t
       (let* ((pkg (or (and pkg-name
                            (%resolve-package-for-server server pkg-name))
                       (and (null pkg-name) pkg-default)
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
               (%json-object "output"
                             (%public-package-text text server)))))))))))

;;; --------------------------------------------------------------------------
;;; Server: accept loop
;;; --------------------------------------------------------------------------

(defun start-server (&key socket-path log-path transport-kind port-path
                       project-root)
  "Start a daemon listening for JSON-RPC connections. Blocks until a
`shutdown' request arrives. Cleans up the listener and ensures the worker
thread is stopped before returning.

TRANSPORT-KIND selects the listener:
  :unix (default on non-Windows) - bind a Unix-domain socket at SOCKET-PATH
                                   with mode 0600.
  :tcp  (default on Windows)     - bind a loopback TCP socket on a random
                                   ephemeral port and write `<port>~%<token>~%'
                                   to PORT-PATH. Every request must carry
                                   `token: <token>' in its params.

When LOG-PATH is supplied, append one JSON line per protocol event
(accept, request, response, interrupt, worker-died, shutdown) and rotate
once the file exceeds 10 MB.

PROJECT-ROOT is the canonical project identity accepted from CLPM CLI
clients in each request's `project_root' parameter. A NIL PROJECT-ROOT means
raw test/tooling servers do not enforce a project-root guard.

Each daemon thread binds `*server*' and project-root pathname defaults
dynamically to its own SERVER. Connection and worker threads inherit that
identity explicitly. A host Lisp process may own at most one project daemon;
different projects need separate Lisp processes so ASDF, packages, workers,
debugger sessions, and other process-global state cannot bleed across roots."
  (let* ((kind (or transport-kind (%default-transport-kind)))
         (advertise (ecase kind
                      (:unix (or socket-path
                                 (error ":unix transport requires :socket-path")))
                      (:tcp  (or port-path
                                 (error ":tcp transport requires :port-path")))))
         (transport (make-transport :kind kind :path advertise))
         (server (make-instance 'server :socket-path advertise
                                        :project-root project-root
                                        :initial-package
                                        (%project-initial-package project-root)
                                        :transport transport))
         (registered-project? nil))
    (when (and log-path (stringp log-path))
      (setf (server-event-log server) (%open-event-log log-path)))
    (let ((*server* server)
          (*default-pathname-defaults*
            (%server-default-pathname-defaults server)))
      (unwind-protect
           (progn
             (unless (%reserved-project-server-root-p project-root)
               (setf registered-project?
                     (%register-active-project-server project-root)))
             (%open-listener transport)
             (setf (server-socket server) (transport-listener transport))
             (%log-event (server-event-log server) "start"
                         "pid" (clpm.repl.compat:getpid)
                         "transport" (string-downcase (string kind))
                         "path" advertise
                         "port" (transport-port transport))
             ;; Spawn a thread per connection so eval (which blocks on the worker)
             ;; doesn't wedge the accept loop. The worker mailbox serializes
             ;; eval requests; other methods (interrupt, ping, status, ...) run
             ;; concurrently with whatever the worker is doing.
             ;;
             ;; Shutdown path: the `shutdown' handler closes this listening
             ;; socket so the blocking `socket-accept' below errors out; the
             ;; handler-case turns that into a clean loop exit.
             (loop until (server-shutdown-requested? server) do
               (handler-case
                   (let ((conn (sb-bsd-sockets:socket-accept
                                (transport-listener transport))))
                     (%log-event (server-event-log server) "accept")
                     (clpm.repl.compat:make-thread
                      (let ((c conn)
                            (owner server))
                        (lambda ()
                          (let ((*server* owner)
                                (*default-pathname-defaults*
                                  (%server-default-pathname-defaults owner)))
                            (unwind-protect
                                 (handler-case
                                     (%handle-connection owner c)
                                   (error (e)
                                     (%log-event (server-event-log owner)
                                                 "handler-error"
                                                 "error" (princ-to-string e))
                                     (format *error-output*
                                             "repl handler error: ~A~%" e)))
                              (ignore-errors
                               (sb-bsd-sockets:socket-close c))))))
                      :name "clpm.repl.conn"))
                 (error ()
                   (when (server-shutdown-requested? server)
                     (loop-finish))))))
        ;; Stop every watcher first so its polling threads tear down
        ;; before the workers it might be loading code into.
        (handler-case (%untrace-all-for-server server)
          (error () nil))
        (dolist (w (%all-watches server))
          (handler-case (%stop-watch w) (error () nil))
          (when (clpm.repl.compat:thread-alive-p (watch-thread w))
            (handler-case
                (clpm.repl.compat:join-thread (watch-thread w))
              (error () nil))))
        ;; A worker in the debugger is blocked on its debug mailbox, not on
        ;; the worker mailbox. Resolve those stops first so ordinary worker
        ;; shutdown can drain below.
        (dolist (session (%all-debug-sessions server))
          (handler-case
              (%abort-debug-session server session "shutdown")
            (error () nil)))
        ;; Stop every worker we spawned. Best-effort: the daemon is going
        ;; away, so failure to join is acceptable.
        (dolist (w (%all-workers server))
          (when (clpm.repl.compat:thread-alive-p (worker-thread w))
            (clpm.repl.compat:send-message (worker-mailbox w) :stop)
            (handler-case
                (clpm.repl.compat:join-thread (worker-thread w))
              (error () nil))))
        (%log-event (server-event-log server) "stop")
        (%close-event-log (server-event-log server))
        (%close-listener transport)
        (when registered-project?
          (%unregister-active-project-server project-root))
        (%delete-project-initial-package server)))))

;;; --------------------------------------------------------------------------
;;; Request context: per-in-flight-request handle used by dispatch to emit
;;; non-terminal `event' frames and the terminal `result' / `error' frame.
;;;
;;; The connection thread owns the socket stream; the worker thread may emit
;;; events through this context as the eval runs. STREAM-MUTEX serializes
;;; writes from any thread.
;;; --------------------------------------------------------------------------

(defstruct connection-state
  "Per-connection routing state. The connection thread is the *only* reader
of STREAM; any thread may write under STREAM-MUTEX. IN-FLIGHT maps the
ids of in-progress requests (currently: evals that opt in to continuation
messages like `query-response') to their eval-job, so a continuation
frame the connection thread reads can be routed to the worker that's
blocked on the corresponding mailbox."
  stream
  stream-mutex
  ;; hash-table: id (any equalp-able JSON scalar) -> eval-job
  in-flight
  in-flight-mutex)

(defun %make-connection-state (stream)
  (make-connection-state
   :stream stream
   :stream-mutex (clpm.repl.compat:make-mutex
                  :name "clpm.repl.conn-stream")
   :in-flight (make-hash-table :test #'equal)
   :in-flight-mutex (clpm.repl.compat:make-mutex
                     :name "clpm.repl.in-flight")))

(defun %register-in-flight (cstate id job)
  (clpm.repl.compat:with-mutex ((connection-state-in-flight-mutex cstate))
    (setf (gethash id (connection-state-in-flight cstate)) job)))

(defun %unregister-in-flight (cstate id)
  (clpm.repl.compat:with-mutex ((connection-state-in-flight-mutex cstate))
    (remhash id (connection-state-in-flight cstate))))

(defun %lookup-in-flight (cstate id)
  (clpm.repl.compat:with-mutex ((connection-state-in-flight-mutex cstate))
    (gethash id (connection-state-in-flight cstate))))

(defstruct request-context
  server
  stream
  stream-mutex
  ;; Connection-state, for the in-flight table. May be NIL when the request
  ;; isn't a candidate for continuation routing.
  cstate
  id
  ;; Plist of v2 opt-ins parsed out of the request params (:stream, :debug,
  ;; :query-interactive, :handlers, ...). NIL for v1 requests.
  options
  ;; Internal latch flipped after the terminal frame is written; further
  ;; %emit-event calls become no-ops.
  terminated?)

(defun %emit-frame (ctx frame)
  "Serialize FRAME to the connection's stream under the stream mutex.
Frames are arbitrary JSON objects (`(:object ...)' forms). Errors are
swallowed: a broken socket should never propagate into eval."
  (when (request-context-terminated? ctx)
    (return-from %emit-frame))
  (clpm.repl.compat:with-mutex ((request-context-stream-mutex ctx))
    (handler-case
        (%write-line-json (request-context-stream ctx) frame)
      (error () nil))))

(defun %emit-event (ctx event-name &rest fields)
  "Send a non-terminal `event' frame with EVENT-NAME and arbitrary FIELDS."
  (let ((obj (apply #'%json-object
                    "id" (request-context-id ctx)
                    "event" event-name
                    fields)))
    (%emit-frame ctx obj)))

(defun %emit-terminal (ctx frame)
  "Send the terminal frame for this request, then mark the context closed
so any straggling events from a background thread are dropped."
  (%emit-frame ctx frame)
  (setf (request-context-terminated? ctx) t))

;;; --------------------------------------------------------------------------
;;; %handle-connection: keep the connection open across requests so a client
;;; can pipeline ping/eval/inspect/... or drive a long-lived debug session
;;; using one TCP/Unix handshake.
;;; --------------------------------------------------------------------------

(defun %handle-connection (server conn)
  (let* ((stream (sb-bsd-sockets:socket-make-stream
                  conn :input t :output t
                        :buffering :line
                        :external-format :utf-8
                        :element-type 'character))
         (cstate (%make-connection-state stream)))
    (loop
      (let ((line (handler-case
                      (%read-request-line stream)
                    (clpm.errors:clpm-parse-error (c)
                      (%log-event (server-event-log server)
                                  "request-parse-error"
                                  "error" (princ-to-string c))
                      (clpm.repl.compat:with-mutex
                          ((connection-state-stream-mutex cstate))
                        (handler-case
                            (%write-line-json
                             stream
                             (%error-response nil "protocol-error"
                                              (princ-to-string c)))
                          (error () nil)))
                      ;; Resync and keep the connection alive.
                      :continue))))
        (cond
          ((eq line :continue))             ; soft-recover from a bad line
          ((null line) (return))            ; EOF: client closed cleanly
          (t (%handle-incoming-line server cstate line)))))))

(defun %eval-uses-continuation? (params)
  "Does this eval request opt into continuation messages (query-response,
debug-invoke-restart, ...)? Such requests must run in their own thread so
the connection thread stays free to read those continuations."
  (and params
       (or (%json-getf params "query_interactive")
           (%json-getf params "debug"))))

(defun %write-error-inline (cstate id code message)
  (clpm.repl.compat:with-mutex ((connection-state-stream-mutex cstate))
    (handler-case
        (%write-line-json (connection-state-stream cstate)
                          (%error-response id code message))
      (error () nil))))

(defun %write-success-inline (cstate id payload)
  (clpm.repl.compat:with-mutex ((connection-state-stream-mutex cstate))
    (handler-case
        (%write-line-json (connection-state-stream cstate)
                          (%success-response id payload))
      (error () nil))))

(defun %write-response-inline (cstate response)
  (clpm.repl.compat:with-mutex ((connection-state-stream-mutex cstate))
    (handler-case
        (%write-line-json (connection-state-stream cstate) response)
      (error () nil))))

(defun %write-debug-action-reply (cstate id reply)
  (case (first reply)
    (:result
     (%write-success-inline cstate id (second reply)))
    (:error
     (%write-error-inline cstate id (second reply) (third reply)))
    (t
     (%write-error-inline cstate id "protocol-error"
                          "invalid debug action reply"))))

(defun %debug-session-param (params)
  (let ((raw (%json-getf params "session")))
    (and (integerp raw) raw)))

(defun %debug-action-for-method (method params &optional reply-box)
  (cond
    ((string= method "debug-invoke-restart")
     (let* ((name (%json-getf params "name"))
            (args (array-items-of-param (%json-getf params "args"))))
       (list :invoke-restart
             (and (stringp name) (string-upcase name))
             (or args '())
             reply-box)))
    ((string= method "debug-eval-in-frame")
     (let ((index (%json-getf params "frame" 0))
           (form (%json-getf params "form")))
       (list :eval-in-frame (or index 0) form reply-box)))
    ((string= method "debug-continue")
     (list :continue reply-box))
    ((string= method "debug-abort")
     (list :abort reply-box))))

(defun %debug-target-for-action (server cstate id params)
  "Return (values JOB FRESH-REQUEST-P ERROR-MESSAGE) for a debug action.

Old continuation clients address the eval by matching request ID on the same
connection and expect no terminal response. Fresh clients address a server
session via `session' and do expect a terminal response."
  (let* ((session-id (%debug-session-param params))
         (explicit-session (and session-id
                                (%find-debug-session server session-id)))
         (job-by-id (%lookup-in-flight cstate id))
         (implicit-sessions (and (null job-by-id)
                                 (null session-id)
                                 (%all-debug-sessions server)))
         (session (or explicit-session
                      (and (= 1 (length implicit-sessions))
                           (first implicit-sessions))))
         (job (or (and session (debug-session-job session))
                  job-by-id))
         (fresh? (or session-id (null job-by-id))))
    (cond
      ((and session-id (null explicit-session))
       (values nil fresh?
               (format nil "no active debug session ~A" session-id)))
      ((and (null job-by-id)
            (null session-id)
            (null implicit-sessions))
       (values nil fresh? "no active debug session"))
      ((and (null job-by-id)
            (null session-id)
            (> (length implicit-sessions) 1))
       (values nil fresh?
               "multiple active debug sessions; pass `session'"))
      ((or (null job) (null (eval-job-debug-mailbox job)))
       (values nil fresh? "no in-flight debug session on this id"))
      (t
       (values job fresh? nil)))))

(defun %route-debug-action (server cstate id method params)
  "Route a debug action to either an in-flight eval or a server-owned session."
  (multiple-value-bind (job fresh? error-message)
      (%debug-target-for-action server cstate id params)
    (cond
      (error-message
       (%log-event (server-event-log server) "debug-action-unmatched"
                   "id" id "method" method)
       (%write-error-inline cstate id "protocol-error" error-message))
      (t
       (let* ((reply-box (and fresh?
                              (clpm.repl.compat:make-mailbox)))
              (action (%debug-action-for-method method params reply-box)))
         (clpm.repl.compat:send-message
          (eval-job-debug-mailbox job) action)
         (%log-event (server-event-log server) "debug-action"
                     "id" id "action" method
                     "fresh" (and fresh? t))
         (when reply-box
           (%write-debug-action-reply
            cstate id
            (clpm.repl.compat:receive-message reply-box))))))))

(defun array-items-of-param (a)
  "Extract elements from a JSON array param, or NIL if not an array."
  (when (and (consp a) (eq (car a) :array))
    (cadr a)))

(defun %route-query-response (server cstate id params)
  "Continuation message: deliver the user's reply to the eval-job that is
blocked on its query-mailbox."
  (let ((job (%lookup-in-flight cstate id)))
    (cond
      ((or (null job) (null (eval-job-query-mailbox job)))
       (%log-event (server-event-log server) "query-response-unmatched"
                   "id" id)
       (%write-error-inline cstate id "protocol-error"
                            "no in-flight query waiting on this id"))
      (t
       (let* ((eof? (%json-getf params "eof"))
              (raw-value (%json-getf params "value"))
              (value (cond
                       (eof? :eof)
                       ((stringp raw-value) raw-value)
                       ((null raw-value) "")
                       (t (princ-to-string raw-value)))))
         (clpm.repl.compat:send-message
          (eval-job-query-mailbox job)
          value)
         ;; query-response has no terminal frame of its own.
         (%log-event (server-event-log server) "query-response"
                     "id" id))))))

(defun %bump-method-count (server method err?)
  "Increment SERVER's per-method counter (total, errored) for METHOD."
  (clpm.repl.compat:with-mutex ((server-method-counts-mutex server))
    (let* ((tbl (server-method-counts server))
           (cell (or (gethash method tbl)
                     (setf (gethash method tbl) (cons 0 0)))))
      (incf (car cell))
      (when err?
        (incf (cdr cell))
        (incf (server-recent-error-count server))))))

(defun %dispatch-and-finalize (server cstate id method params)
  "Run %dispatch-method, log the response, and emit the terminal frame.
Used by both the inline path and the threaded path."
  (let* ((ctx (make-request-context :server server
                                    :stream (connection-state-stream cstate)
                                    :stream-mutex (connection-state-stream-mutex cstate)
                                    :cstate cstate
                                    :id id
                                    :options params))
         (start (get-internal-real-time))
         (explain? (and params (%json-getf params "explain")))
         (response (handler-case
                       (progn
                         ;; `explain: true' -- emit a `plan' event before
                         ;; the handler runs so the client can verify the
                         ;; daemon parsed their request the way they
                         ;; intended. The handler still runs as usual.
                         (when explain?
                           (%emit-event ctx "plan"
                                        "method" method
                                        "params" (%method-local-params params)))
                         (%dispatch-method server method params id ctx))
                     (error (c)
                       (%error-response id "protocol-error"
                                        (format nil "dispatch failed: ~A" c)))))
         (elapsed (round (* 1000.0
                            (/ (- (get-internal-real-time) start)
                               internal-time-units-per-second))))
         (error-code (%terminal-response-error-code response)))
    (%bump-method-count server method (not (null error-code)))
    (%log-event (server-event-log server) "response"
                "id" id "method" method
                "elapsed_ms" elapsed
                "error" error-code)
    ;; Slowlog (#214): any eval over the threshold gets a dedicated
    ;; log entry so a future operator can find pathological forms.
    (when (and (string= method "eval")
               (>= elapsed +slow-eval-threshold-ms+))
      (let* ((form (and params (%json-getf params "form")))
             (preview (and (stringp form)
                           (if (> (length form) 200)
                               (concatenate 'string (subseq form 0 200) "...")
                               form))))
        (%log-event (server-event-log server) "slow-eval"
                    "id" id "elapsed_ms" elapsed "form" preview)))
    ;; Dispatchers that pump their own events return NIL after emitting
    ;; their terminal frame directly. Otherwise we emit it.
    (when response
      (%emit-terminal ctx response))
    (%unregister-in-flight cstate id)))

(defun %project-root-mismatch-p (server params)
  "Return true when PARAMS do not authorize access to SERVER's project."
  (let ((expected (server-project-root server)))
    (and expected
         (let ((actual (%json-getf params "project_root")))
           (or (not (stringp actual))
               (not (string= actual expected)))))))

(defun %handle-incoming-line (server cstate line)
  "Parse one line off the connection. Continuation messages (query-response,
...) are routed inline and produce no terminal frame. New requests are
either dispatched inline (fast RPCs) or in a dedicated thread (`eval'
with v2 toggles requiring continuations)."
  (handler-case
      (let* ((request (handler-case
                          (clpm.io.json:read-json-from-string line)
                        (error (c)
                          (%write-error-inline cstate nil "protocol-error"
                                               (princ-to-string c))
                          (return-from %handle-incoming-line))))
             (id (%json-getf request "id"))
             (method (%json-getf request "method"))
             (params (%json-getf request "params"))
             (transport (server-transport server))
             (expected-token (and transport (transport-token transport))))
        (cond
          ((not (stringp method))
           (%log-event (server-event-log server) "request-invalid" "id" id)
           (%write-error-inline cstate id "protocol-error" "missing `method'"))
          ((and params (not (%json-object-p params)))
           (%log-event (server-event-log server) "request-invalid"
                       "id" id "method" method)
           (%write-error-inline cstate id "protocol-error"
                                (format nil "params must be an object for ~A"
                                        method)))
          ((and expected-token
                (let ((tok (%json-getf params "token")))
                  (or (not (stringp tok))
                      (not (%constant-string= tok expected-token)))))
           (%log-event (server-event-log server) "auth-rejected"
                       "id" id "method" method)
           (%write-error-inline cstate id "protocol-error"
                                "missing or invalid `token`"))
          ((%project-root-mismatch-p server params)
           (%log-event (server-event-log server) "project-rejected"
                       "id" id "method" method)
           (%write-error-inline cstate id "protocol-error"
                                "missing or invalid `project_root`"))
	          ;; Continuation: the user's reply to an `event:query'.
	          ((string= method "query-response")
	           (multiple-value-bind (decoded-params decode-error)
	               (%decode-params-for-method method params id)
	             (cond
	               (decode-error
	                (%write-response-inline cstate decode-error))
	               (t
	                (%route-query-response server cstate id decoded-params)))))
	          ;; Continuations: debug-* actions driving an in-flight debugger.
	          ((or (string= method "debug-invoke-restart")
	               (string= method "debug-eval-in-frame")
	               (string= method "debug-continue")
	               (string= method "debug-abort"))
	           (multiple-value-bind (decoded-params decode-error)
	               (%decode-params-for-method method params id)
	             (cond
	               (decode-error
	                (%write-response-inline cstate decode-error))
	               (t
	                (%route-debug-action server cstate id method decoded-params)))))
          ;; eval with v2 continuation toggles -> spawn a dispatcher thread.
          ((and (string= method "eval")
                (%eval-uses-continuation? params))
           (%log-event (server-event-log server) "request"
                       "id" id "method" method)
           (clpm.repl.compat:make-thread
            (let ((owner server))
              (lambda ()
                (let ((*server* owner)
                      (*default-pathname-defaults*
                        (%server-default-pathname-defaults owner)))
                  (handler-case
                      (%dispatch-and-finalize owner cstate id method params)
                    (error (c)
                      (%log-event (server-event-log owner)
                                  "dispatch-thread-error"
                                  "id" id "error" (princ-to-string c))
                      (%write-error-inline cstate id "protocol-error"
                                           (princ-to-string c))
                      (%unregister-in-flight cstate id))))))
            :name "clpm.repl.dispatch"))
          (t
           (%log-event (server-event-log server) "request"
                       "id" id "method" method)
           (%dispatch-and-finalize server cstate id method params))))
    (error (c)
      (%write-error-inline cstate nil "protocol-error"
                           (princ-to-string c)))))

;;; --------------------------------------------------------------------------
;;; Client
;;; --------------------------------------------------------------------------

(defun %inject-token (params token)
  "Add a `token' field to PARAMS (a JSON object form), creating one if NIL.
Malformed non-object params are preserved so the server reports the real
protocol error instead of hiding it behind token injection."
  (cond
    ((null token) (or params (%json-object)))
    (t
     (let ((base (or params (%json-object))))
       (cond
         ((and (consp base) (eq (car base) :object))
          (list :object (cons (cons "token" token) (cadr base))))
         (t base))))))

(defun send-request (endpoint method &key params (id 1) (connect-timeout 5)
                                          on-event)
  "Send one request and return its terminal frame (the `result' / `error'
JSON object). Returns
   :no-daemon if the daemon is absent / unreachable,
   :io-error  if the connection dropped mid-exchange.

ENDPOINT is a filesystem path. If it ends in `.port', the TCP transport
is used: the file's first line is the bound port, the second line is a
32-hex shared token, and the token is injected into the request's
params. Otherwise the path is treated as a Unix-domain socket and the
token is read from ENDPOINT.token.

ON-EVENT, when supplied, is invoked once per non-terminal frame
(streamed stdout, debugger-entered, trace lines, ...). It receives the
parsed JSON object and may return NIL to keep waiting or :stop to abort
the connection immediately.

Responses are read without a size cap; daemon output (`+max-output-bytes+`,
1 MB) can legitimately fill a line."
  (check-type endpoint string)
  (let ((kind (%infer-transport-kind endpoint)))
    (multiple-value-bind (sock token)
        (%connect-transport kind endpoint :timeout-seconds connect-timeout)
      (unless sock
        (return-from send-request :no-daemon))
      (unwind-protect
           (let* ((stream (sb-bsd-sockets:socket-make-stream
                           sock :input t :output t :buffering :full
                                :external-format :utf-8
                                :element-type 'character))
                  (request (%json-object "id" id
                                         "method" method
                                         "params" (%inject-token params token))))
             (handler-case
                 (progn
                   (%write-line-json stream request)
                   (loop
                     (let ((line (read-line stream nil nil)))
                       (cond
                         ((null line) (return :io-error))
                         (t
                          (let* ((frame (clpm.io.json:read-json-from-string line))
                                 (event (%json-getf frame "event")))
                            (cond
                              ((null event)
                               ;; Terminal frame (has either `result' or
                               ;; `error', no `event').
                               (return frame))
                              (t
                               (when on-event
                                 (let ((r (funcall on-event frame)))
                                   (when (eq r :stop)
                                     (return :io-error))))))))))))
               (error () :io-error)))
        (ignore-errors (sb-bsd-sockets:socket-close sock))))))

(defclass connection ()
  ((endpoint :initarg :endpoint :reader connection-endpoint)
   (socket :initarg :socket :reader connection-socket)
   (stream :initarg :stream :reader connection-stream)
   (token :initarg :token :reader connection-token)
   (mutex :initform (clpm.repl.compat:make-mutex
                     :name "clpm.repl.client-conn")
          :reader connection-mutex)
   (closed? :initform nil :accessor connection-closed?)))

(defun open-connection (endpoint &key (connect-timeout 5))
  "Open and return a CONNECTION to ENDPOINT. The connection persists until
CLOSE-CONNECTION; multiple SEND-ON-CONNECTION calls may share it.

Returns :no-daemon if the daemon is unreachable."
  (check-type endpoint string)
  (let ((kind (%infer-transport-kind endpoint)))
    (multiple-value-bind (sock token)
        (%connect-transport kind endpoint :timeout-seconds connect-timeout)
      (unless sock (return-from open-connection :no-daemon))
      (let ((stream (sb-bsd-sockets:socket-make-stream
                     sock :input t :output t :buffering :full
                          :external-format :utf-8
                          :element-type 'character)))
        (make-instance 'connection
                       :endpoint endpoint
                       :socket sock
                       :stream stream
                       :token token)))))

(defun close-connection (conn)
  "Idempotently close CONN."
  (unless (connection-closed? conn)
    (setf (connection-closed? conn) t)
    (ignore-errors (close (connection-stream conn)))
    (ignore-errors (sb-bsd-sockets:socket-close (connection-socket conn)))))

(defun send-on-connection (conn method &key params (id 1) on-event)
  "Like SEND-REQUEST but reuses an open CONNECTION. Returns the terminal
frame, or :io-error on EOF or socket error.

The connection is single-threaded by convention: do not call this
function from two threads concurrently with the same CONN."
  (when (connection-closed? conn)
    (return-from send-on-connection :io-error))
  (let* ((stream (connection-stream conn))
         (request (%json-object "id" id
                                "method" method
                                "params" (%inject-token
                                          params (connection-token conn)))))
    (handler-case
        (progn
          (%write-line-json stream request)
          (loop
            (let ((line (read-line stream nil nil)))
              (cond
                ((null line) (return :io-error))
                (t
                 (let* ((frame (clpm.io.json:read-json-from-string line))
                        (event (%json-getf frame "event")))
                   (cond
                     ((null event) (return frame))
                     (t
                      (when on-event
                        (let ((r (funcall on-event frame)))
                          (when (eq r :stop)
                            (return :io-error))))))))))))
      (error () :io-error))))

(defun send-continuation-on-connection (conn id method &key params)
  "Push a continuation message onto an open CONNECTION without waiting.

The current thread is busy reading event frames inside
`send-on-connection'; this writes the JSON line directly to the
connection's stream so the daemon can route the continuation while we
keep reading. Returns T on success, NIL on stream error.

Use this for continuation methods that share an `id' with an
in-flight request (debug-invoke-restart, debug-eval-in-frame,
debug-abort, debug-continue, query-response). Do not use it for
fresh requests -- those should open their own connection or use
`send-request'."
  (when (connection-closed? conn)
    (return-from send-continuation-on-connection nil))
  (let* ((stream (connection-stream conn))
         (msg (%json-object
               "id" id
               "method" method
               "params" (%inject-token params (connection-token conn)))))
    (handler-case
        (progn (%write-line-json stream msg) t)
      (error () nil))))
