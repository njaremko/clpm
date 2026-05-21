;;;; test/repl-tcp-test.lisp - TCP loopback transport + token auth.
;;;;
;;;; The TCP transport is the Windows fallback (#024). It works on any OS, so
;;;; we exercise it directly here by forcing :transport-kind :tcp. The CLI
;;;; layer selects the transport from uiop:os-windows-p, so a Windows-only
;;;; integration test would be needed to assert end-to-end OS detection.

(require :asdf)
(require :sb-bsd-sockets)
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

(defun lookup (object key)
  (when (and (consp object) (eq (car object) :object))
    (cdr (assoc key (cadr object) :test #'string=))))

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

;;; ----------------------------------------------------------------------------
;;; #024 acceptance: cmd-repl detects the OS and uses the right
;;; transport. We can't *change* OSes inside a single test run, so we assert
;;; the dispatch function picks `:tcp` on Windows hosts and `:unix` elsewhere.

(format t "Test: %default-transport-kind dispatches on host OS~%")
(let* ((sym (find-symbol "%DEFAULT-TRANSPORT-KIND"
                         (find-package "CLPM.REPL")))
       (kind (funcall sym))
       (windows-p (and (find-package "UIOP/OS")
                       (fboundp (find-symbol "OS-WINDOWS-P" "UIOP/OS"))
                       (funcall (find-symbol "OS-WINDOWS-P" "UIOP/OS")))))
  (assert-true sym "expected internal %DEFAULT-TRANSPORT-KIND")
  (if windows-p
      (assert-eql :tcp kind)
      (assert-eql :unix kind)))
(format t "  OS dispatch OK~%")

;;; ----------------------------------------------------------------------------
;;; The TCP transport itself is portable. Exercise it end-to-end on any OS.

(format t "Test: TCP daemon services ping + eval requests~%")
(clpm.store:with-temp-dir (tmp)
  (let* ((port-path (namestring (merge-pathnames "rb.port" tmp)))
         (server-thread
           (sb-thread:make-thread
            (lambda ()
              (handler-case
                  (clpm.repl:start-server :transport-kind :tcp
                                                 :port-path port-path)
                (error (c)
                  (format *error-output* "daemon: ~A~%" c))))
            :name "test-tcp-bridge")))
    (unwind-protect
         (progn
           ;; Wait for the port file to land.
           (loop for i from 0 below 50
                 while (not (probe-file port-path))
                 do (sleep 0.1))
           (assert-true (probe-file port-path) "port file never appeared")

           (let* ((raw (uiop:read-file-string port-path))
                  (nl (position #\Newline raw))
                  (port (and nl (parse-integer (subseq raw 0 nl) :junk-allowed nil)))
                  (token-tail (and nl (subseq raw (1+ nl))))
                  (token (and token-tail
                              (string-trim '(#\Space #\Newline #\Return)
                                           token-tail))))
             (assert-true (integerp port) "port file's port line is invalid: ~A" raw)
             (assert-true (and (stringp token) (= 32 (length token)))
                          "expected 32-hex token, got ~S" token))

           ;; send-request reads the port file and injects the token.
           (let ((resp (clpm.repl:send-request port-path "ping")))
             (assert-true (not (eq resp :no-daemon)) "TCP daemon unreachable")
             (assert-true (not (eq resp :io-error)) "I/O error talking to TCP daemon")
             (let ((result (lookup resp "result")))
               (assert-true (and (consp result) (eq (car result) :object))
                            "no result in response: ~S" resp)
               (assert-true (integerp (lookup result "pid"))
                            "ping result missing pid: ~S" result)))

           (let ((events '()))
             (let ((resp (clpm.repl:send-request
                          port-path
                          "ping"
                          :params (list :object (list (cons "explain" t)))
                          :on-event (lambda (frame)
                                      (push frame events)
                                      nil))))
               (assert-true (lookup resp "result")
                            "TCP explain ping failed: ~S" resp)
               (let ((plan (find-if (lambda (frame)
                                      (string= "plan" (lookup frame "event")))
                                    events)))
                 (assert-true plan "no TCP explain plan event: ~S" events)
                 (let ((params (lookup plan "params")))
                   (assert-true (not (lookup params "token"))
                                "plan leaked TCP auth token: ~S" plan)
                   (assert-true (not (lookup params "explain"))
                                "plan leaked explain dispatch param: ~S" plan)))))

           ;; eval round-trip.
           (let* ((resp (clpm.repl:send-request
                         port-path "eval"
                         :params (list :object (list (cons "form" "(+ 40 2)"))))))
             (let ((result (lookup resp "result")))
               (assert-true (and (stringp (lookup result "value"))
                                 (string= "42" (lookup result "value")))
                            "eval did not return 42: ~S" result))))
      (handler-case (clpm.repl:send-request port-path "shutdown")
        (error () nil))
      (loop for i from 0 below 30
            while (sb-thread:thread-alive-p server-thread)
            do (sleep 0.1))
      (when (sb-thread:thread-alive-p server-thread)
        (ignore-errors (sb-thread:terminate-thread server-thread))))))
(format t "  TCP transport OK~%")

;;; ----------------------------------------------------------------------------
;;; Token enforcement: a connection that omits or mis-spells the token is
;;; rejected at the protocol layer.

(format t "Test: TCP daemon rejects requests without the token~%")
(clpm.store:with-temp-dir (tmp)
  (let* ((port-path (namestring (merge-pathnames "rb.port" tmp)))
         (server-thread
           (sb-thread:make-thread
            (lambda ()
              (handler-case
                  (clpm.repl:start-server :transport-kind :tcp
                                                 :port-path port-path)
                (error (c)
                  (format *error-output* "daemon: ~A~%" c))))
            :name "test-tcp-auth")))
    (unwind-protect
         (progn
           (loop for i from 0 below 50
                 while (not (probe-file port-path))
                 do (sleep 0.1))
           (let* ((raw (uiop:read-file-string port-path))
                  (nl (position #\Newline raw))
                  (port (parse-integer (subseq raw 0 nl) :junk-allowed nil)))
             ;; Drive the raw socket ourselves so we can omit the token.
             (let* ((sock (make-instance 'sb-bsd-sockets:inet-socket
                                         :type :stream :protocol :tcp)))
               (sb-bsd-sockets:socket-connect sock #(127 0 0 1) port)
               (let ((stream (sb-bsd-sockets:socket-make-stream
                              sock :input t :output t :buffering :full
                                   :external-format :utf-8
                                   :element-type 'character)))
                 (unwind-protect
                      (progn
                        (write-string "{\"id\":1,\"method\":\"ping\",\"params\":{}}" stream)
                        (write-char #\Newline stream)
                        (force-output stream)
                        (let* ((line (read-line stream nil nil))
                               (resp (clpm.io.json:read-json-from-string line))
                               (err (lookup resp "error")))
                          (assert-true (and (consp err) (eq (car err) :object))
                                       "expected an error response, got ~S" resp)
                          (assert-true (string= "protocol-error" (lookup err "code"))
                                       "wrong error code: ~S" err)))
                   (ignore-errors (close stream))
                   (ignore-errors (sb-bsd-sockets:socket-close sock)))))

             ;; Sanity check: a connection that DOES supply the token (via
             ;; the normal send-request path) still succeeds, proving the
             ;; daemon survived the rejection.
             (let ((resp (clpm.repl:send-request port-path "ping")))
               (assert-true (lookup resp "result")
                            "daemon broke after auth rejection: ~S" resp))))
      (handler-case (clpm.repl:send-request port-path "shutdown")
        (error () nil))
      (loop for i from 0 below 30
            while (sb-thread:thread-alive-p server-thread)
            do (sleep 0.1))
      (when (sb-thread:thread-alive-p server-thread)
        (ignore-errors (sb-thread:terminate-thread server-thread))))))
(format t "  auth rejection OK~%")

;;; ----------------------------------------------------------------------------
;;; Wrong-token requests are also rejected (catches a typo + an attacker who
;;; has the port but not the secret).

(format t "Test: TCP daemon rejects requests with the wrong token~%")
(clpm.store:with-temp-dir (tmp)
  (let* ((port-path (namestring (merge-pathnames "rb.port" tmp)))
         (server-thread
           (sb-thread:make-thread
            (lambda ()
              (handler-case
                  (clpm.repl:start-server :transport-kind :tcp
                                                 :port-path port-path)
                (error (c)
                  (format *error-output* "daemon: ~A~%" c))))
            :name "test-tcp-badtoken")))
    (unwind-protect
         (progn
           (loop for i from 0 below 50
                 while (not (probe-file port-path))
                 do (sleep 0.1))
           (let* ((raw (uiop:read-file-string port-path))
                  (nl (position #\Newline raw))
                  (port (parse-integer (subseq raw 0 nl) :junk-allowed nil))
                  (sock (make-instance 'sb-bsd-sockets:inet-socket
                                       :type :stream :protocol :tcp)))
             (sb-bsd-sockets:socket-connect sock #(127 0 0 1) port)
             (let ((stream (sb-bsd-sockets:socket-make-stream
                            sock :input t :output t :buffering :full
                                 :external-format :utf-8
                                 :element-type 'character)))
               (unwind-protect
                    (progn
                      (write-string
                       "{\"id\":1,\"method\":\"ping\",\"params\":{\"token\":\"deadbeefdeadbeefdeadbeefdeadbeef\"}}"
                       stream)
                      (write-char #\Newline stream)
                      (force-output stream)
                      (let* ((line (read-line stream nil nil))
                             (resp (clpm.io.json:read-json-from-string line))
                             (err (lookup resp "error")))
                        (assert-true (and (consp err) (eq (car err) :object))
                                     "wrong-token request succeeded: ~S" resp)
                        (assert-true (string= "protocol-error" (lookup err "code"))
                                     "wrong-token code: ~S" err)))
                 (ignore-errors (close stream))
                 (ignore-errors (sb-bsd-sockets:socket-close sock))))))
      (handler-case (clpm.repl:send-request port-path "shutdown")
        (error () nil))
      (loop for i from 0 below 30
            while (sb-thread:thread-alive-p server-thread)
            do (sleep 0.1))
      (when (sb-thread:thread-alive-p server-thread)
        (ignore-errors (sb-thread:terminate-thread server-thread))))))
(format t "  wrong-token rejection OK~%")

;;; ----------------------------------------------------------------------------
;;; Lifecycle code treats a wrong-token endpoint as stale project metadata,
;;; rather than reporting another project's live daemon as this one.

(format t "Test: TCP stale foreign token is stale lifecycle state~%")
(clpm.store:with-temp-dir (tmp)
  (let* ((root-a (merge-pathnames "project-a/" tmp))
         (root-b (merge-pathnames "project-b/" tmp))
         (port-a (namestring (merge-pathnames ".clpm/repl.port" root-a)))
         (port-b (namestring (merge-pathnames ".clpm/repl.port" root-b)))
         (server-thread nil))
    (ensure-directories-exist (merge-pathnames ".clpm/" root-a))
    (ensure-directories-exist (merge-pathnames ".clpm/" root-b))
    (setf server-thread
          (sb-thread:make-thread
           (lambda ()
             (handler-case
                 (clpm.repl:start-server
                  :transport-kind :tcp
                  :port-path port-a
                  :project-root (namestring
                                 (uiop:ensure-directory-pathname
                                  (truename root-a))))
               (error (c)
                 (format *error-output* "daemon: ~A~%" c))))
           :name "test-tcp-foreign-token"))
    (unwind-protect
         (progn
           (loop for i from 0 below 50
                 while (not (probe-file port-a))
                 do (sleep 0.1))
           (assert-true (probe-file port-a) "project A port file never appeared")
           (let* ((raw (uiop:read-file-string port-a))
                  (nl (position #\Newline raw))
                  (port (parse-integer (subseq raw 0 nl) :junk-allowed nil))
                  (ping (find-symbol "%BRIDGE-PING-DAEMON"
                                     (find-package "CLPM.COMMANDS"))))
             (with-open-file (s port-b :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist :create
                                       :external-format :utf-8)
               (format s "~D~%deadbeefdeadbeefdeadbeefdeadbeef~%" port))
             (multiple-value-bind (state response result)
                 (funcall ping port-b root-b)
               (declare (ignore result))
               (assert-eql :project-mismatch state)
               (let ((err (lookup response "error")))
                 (assert-true (and err
                                   (search "token" (lookup err "message")))
                              "expected token rejection response, got ~S"
                              response)))))
      (handler-case (clpm.repl:send-request port-a "shutdown")
        (error () nil))
      (loop for i from 0 below 30
            while (sb-thread:thread-alive-p server-thread)
            do (sleep 0.1))
      (when (sb-thread:thread-alive-p server-thread)
        (ignore-errors (sb-thread:terminate-thread server-thread))))))
(format t "  TCP stale token lifecycle OK~%")

;;; ----------------------------------------------------------------------------
;;; Unix transport writes an auth token next to the socket. The public client
;;; helper reads and injects it.

(format t "Test: Unix transport injects its auth token~%")
(when (and (find-package "UIOP/OS")
           (fboundp (find-symbol "OS-WINDOWS-P" "UIOP/OS"))
           (funcall (find-symbol "OS-WINDOWS-P" "UIOP/OS")))
  (format t "  skipped: Unix sockets unavailable on Windows host~%"))
(unless (and (find-package "UIOP/OS")
             (fboundp (find-symbol "OS-WINDOWS-P" "UIOP/OS"))
             (funcall (find-symbol "OS-WINDOWS-P" "UIOP/OS")))
  (with-short-temp-dir (tmp)
    (let* ((sock-path (namestring (merge-pathnames "rb.sock" tmp)))
           (server-thread
             (sb-thread:make-thread
              (lambda ()
                (handler-case
                    (clpm.repl:start-server :transport-kind :unix
                                                   :socket-path sock-path)
                  (error (c)
                    (format *error-output* "daemon: ~A~%" c))))
              :name "test-unix-bridge")))
      (unwind-protect
           (progn
             (loop for i from 0 below 50
                   while (not (probe-file sock-path))
                   do (sleep 0.1))
             (let ((resp (clpm.repl:send-request sock-path "ping")))
               (assert-true (lookup resp "result")
                            "Unix daemon ping failed: ~S" resp)))
        (handler-case (clpm.repl:send-request sock-path "shutdown")
          (error () nil))
        (loop for i from 0 below 30
              while (sb-thread:thread-alive-p server-thread)
              do (sleep 0.1))
        (when (sb-thread:thread-alive-p server-thread)
          (ignore-errors (sb-thread:terminate-thread server-thread))))))
  (format t "  Unix transport OK~%"))

(format t "~%REPL TCP transport tests PASSED!~%")
(sb-ext:exit :code 0)
