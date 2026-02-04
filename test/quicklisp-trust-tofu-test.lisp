;;;; test/quicklisp-trust-tofu-test.lisp - Quicklisp distinfo trust (TOFU/pinning) tests

(require :asdf)
(require :sb-posix)
(require :sb-bsd-sockets)

;; Add repo root directory to ASDF load path.
(let* ((this-file (or *load-truename* *load-pathname*))
       (test-dir (uiop:pathname-directory-pathname this-file))
       (repo-root (uiop:pathname-parent-directory-pathname test-dir)))
  (push repo-root asdf:*central-registry*))

;; Load CLPM
(format t "Loading CLPM...~%")
(handler-case
    (asdf:load-system :clpm)
  (error (c)
    (format *error-output* "Failed to load CLPM: ~A~%" c)
    (sb-ext:exit :code 1)))

(format t "CLPM loaded successfully.~%~%")

(defun fail (fmt &rest args)
  (apply #'format *error-output* (concatenate 'string fmt "~%") args)
  (sb-ext:exit :code 1))

(defun assert-eql (expected actual)
  (unless (eql expected actual)
    (fail "Assertion failed: expected ~S, got ~S" expected actual)))

(defun assert-true (x fmt &rest args)
  (unless x
    (apply #'fail fmt args)))

(defun write-text (path text)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (write-string text s)))

(defun write-sexp (path form)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (let ((*print-case* :downcase))
      (prin1 form s)
      (terpri s))))

(defun run-cli-captured (args)
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((*standard-output* out)
          (*error-output* err))
      (let ((code (clpm:run-cli args)))
        (values code
                (get-output-stream-string out)
                (get-output-stream-string err))))))

;;; Minimal HTTP file server for serving a fake Quicklisp dist.

(defun read-file-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((data (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence data s)
      data)))

(defun %octets-prefix->string (octets end)
  (coerce (loop for i from 0 below end collect (code-char (aref octets i)))
          'string))

(defun %read-http-headers (stream)
  (let ((buf (make-array 0 :element-type '(unsigned-byte 8)
                         :adjustable t :fill-pointer 0)))
    (loop for b = (read-byte stream nil nil)
          while b do
            (vector-push-extend b buf)
            (let ((n (length buf)))
              (when (and (>= n 4)
                         (= (aref buf (- n 4)) 13)
                         (= (aref buf (- n 3)) 10)
                         (= (aref buf (- n 2)) 13)
                         (= (aref buf (- n 1)) 10))
                (return buf))))))

(defun %request-path (header-octets)
  (let* ((len (length header-octets))
         (line-end (or (position 13 header-octets) len))
         (line (%octets-prefix->string header-octets line-end))
         (parts (remove-if (lambda (s) (zerop (length s)))
                           (uiop:split-string line :separator '(#\Space)))))
    (when (and (<= 2 (length parts))
               (string= (first parts) "GET"))
      (let* ((raw (second parts))
             (q (position #\? raw))
             (path (if q (subseq raw 0 q) raw)))
        path))))

(defun %safe-relpath (path)
  (unless (and (stringp path) (plusp (length path)) (char= (char path 0) #\/))
    (return-from %safe-relpath nil))
  (let* ((rel (subseq path 1))
         (parts (uiop:split-string rel :separator '(#\/))))
    (when (or (member ".." parts :test #'string=)
              (member "." parts :test #'string=))
      (return-from %safe-relpath nil))
    rel))

(defparameter *empty-octets* (make-array 0 :element-type '(unsigned-byte 8)))

(defun %write-http-response (stream status body)
  (let* ((reason (ecase status
                   (200 "OK")
                   (400 "Bad Request")
                   (404 "Not Found")
                   (500 "Internal Server Error")))
         (body (or body *empty-octets*))
         (header
           (format nil "HTTP/1.1 ~D ~A~C~CContent-Length: ~D~C~CConnection: close~C~C~C~C"
                   status reason
                   #\Return #\Linefeed
                   (length body)
                   #\Return #\Linefeed
                   #\Return #\Linefeed
                   #\Return #\Linefeed))
         (header-octets (map '(vector (unsigned-byte 8)) #'char-code header)))
    (write-sequence header-octets stream)
    (write-sequence body stream)
    (finish-output stream)))

(defun start-file-http-server (root)
  "Start a tiny HTTP server that serves files rooted at ROOT.

Returns (values base-url stop-fn)."
  (let* ((root (uiop:ensure-directory-pathname root))
         (sock (make-instance 'sb-bsd-sockets:inet-socket
                              :type :stream
                              :protocol :tcp)))
    (sb-bsd-sockets:socket-bind sock #(127 0 0 1) 0)
    (sb-bsd-sockets:socket-listen sock 16)
    (multiple-value-bind (_addr port) (sb-bsd-sockets:socket-name sock)
      (declare (ignore _addr))
      (let* ((stop nil)
             (serve-client
               (lambda (client)
                 (unwind-protect
                      (let ((stream (sb-bsd-sockets:socket-make-stream
                                     client
                                     :input t
                                     :output t
                                     :element-type '(unsigned-byte 8)
                                     :buffering :none)))
                        (handler-case
                            (let* ((hdr (%read-http-headers stream))
                                   (path (%request-path hdr))
                                   (rel (%safe-relpath path)))
                              (cond
                                ((null rel)
                                 (%write-http-response stream 400 *empty-octets*))
                                (t
                                 (let ((file (merge-pathnames rel root)))
                                   (if (uiop:file-exists-p file)
                                       (%write-http-response stream 200 (read-file-bytes file))
                                       (%write-http-response stream 404 *empty-octets*))))))
                          (error ()
                            (ignore-errors
                              (%write-http-response stream 500 *empty-octets*)))))
                   (ignore-errors (sb-bsd-sockets:socket-close client))))))
        (labels ((server-loop ()
                   (loop until stop do
                     (handler-case
                         (multiple-value-bind (client _a _p)
                             (sb-bsd-sockets:socket-accept sock)
                           (declare (ignore _a _p))
                           (funcall serve-client client))
                       (error ()
                         (when (not stop)
                           (sleep 0.01)))))))
          (let ((th (sb-thread:make-thread #'server-loop :name "clpm.test.http")))
            (values (format nil "http://127.0.0.1:~D/" port)
                    (lambda ()
                      (setf stop t)
                      (ignore-errors (sb-bsd-sockets:socket-close sock))
                      (ignore-errors (sb-thread:join-thread th))))))))))

(defun file-base-url (root)
  "Return a file:// base URL for ROOT (directory)."
  (format nil "file://~A" (namestring (uiop:ensure-directory-pathname root))))

(format t "Testing Quicklisp distinfo trust pinning...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (dist-root (merge-pathnames "dist-root/" tmp))
         (distinfo (merge-pathnames "dist/quicklisp.txt" dist-root))
         (systems-txt (merge-pathnames "dist/systems.txt" dist-root))
         (releases-txt (merge-pathnames "dist/releases.txt" dist-root))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist dist-root)
    (multiple-value-bind (base-url stop-server)
        (handler-case
            (start-file-http-server dist-root)
          (sb-bsd-sockets:operation-not-permitted-error ()
            (values (file-base-url dist-root) (lambda () nil)))
          (error ()
            (values (file-base-url dist-root) (lambda () nil))))
      (unwind-protect
           (progn
             (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)

             ;; Fake Quicklisp dist served by localhost.
             (write-text
              distinfo
              (with-output-to-string (s)
                (format s "system-index-url: ~Adist/systems.txt~%" base-url)
                (format s "release-index-url: ~Adist/releases.txt~%" base-url)))
             (write-text systems-txt "foo foo.asd foo\n")
             (write-text
              releases-txt
              (with-output-to-string (s)
                (format s "foo ~Aarchive/foo/foo.tgz 0 0 0123456789012345678901234567890123456789 foo-20260201-git foo.asd~%"
                        base-url)))

             (let* ((sha1 (clpm.crypto.sha256:sha256-file distinfo))
                    (sha (clpm.crypto.sha256:bytes-to-hex sha1))
                    (pinned (format nil "sha256:~A" sha)))
               ;; Config: Quicklisp registry in TOFU mode.
               (write-sexp
                (merge-pathnames "config/config.sxp" clpm-home)
                `(:config :format 1
                  :registries ((:quicklisp :url ,(format nil "~Adist/quicklisp.txt" base-url)
                                :name "quicklisp"
                                :trust "tofu"))
                  :defaults nil))

               ;; First update pins distinfo digest into config.
               (multiple-value-bind (code _stdout stderr)
                   (run-cli-captured '("registry" "update" "quicklisp"))
                 (declare (ignore _stdout))
                 (unless (eql code 0)
                   (fail "registry update failed: ~D~%stderr:~A" code stderr)))

               (let* ((cfg (clpm.config:read-config))
                      (refs (clpm.config:config-registries cfg))
                      (ref (find "quicklisp" refs :key #'clpm.project:registry-ref-name :test #'string=)))
                 (assert-true ref "quicklisp registry ref missing from config after update")
                 (assert-true (string= pinned (clpm.project:registry-ref-trust ref))
                              "Expected pinned trust ~S, got ~S"
                              pinned (clpm.project:registry-ref-trust ref)))

               ;; Mutate served distinfo; update should now fail without refresh.
               (write-text
                distinfo
                (with-output-to-string (s)
                  (format s "system-index-url: ~Adist/systems.txt~%" base-url)
                  (format s "release-index-url: ~Adist/releases.txt~%" base-url)
                  (format s "# changed~%")))
               (let* ((sha2 (clpm.crypto.sha256:bytes-to-hex
                             (clpm.crypto.sha256:sha256-file distinfo)))
                      (pinned2 (format nil "sha256:~A" sha2)))
                 (multiple-value-bind (code _stdout stderr)
                     (run-cli-captured '("registry" "update" "quicklisp"))
                   (declare (ignore _stdout))
                   (assert-eql 1 code)
                   (assert-true (search "SHA-256 mismatch" stderr :test #'char-equal)
                                "Expected mismatch error, got:~%~A" stderr))

                 ;; With refresh, it succeeds and updates config pin.
                 (multiple-value-bind (code _stdout stderr)
                     (run-cli-captured '("registry" "update" "--refresh-trust" "quicklisp"))
                   (declare (ignore _stdout))
                   (unless (eql code 0)
                     (fail "registry update --refresh-trust failed: ~D~%stderr:~A" code stderr)))

                 (let* ((cfg (clpm.config:read-config))
                        (refs (clpm.config:config-registries cfg))
                        (ref (find "quicklisp" refs :key #'clpm.project:registry-ref-name :test #'string=)))
                   (assert-true ref "quicklisp registry ref missing from config after refresh")
                   (assert-true (string= pinned2 (clpm.project:registry-ref-trust ref))
                                "Expected refreshed pin ~S, got ~S"
                                pinned2 (clpm.project:registry-ref-trust ref)))))))
        (ignore-errors (funcall stop-server))
        (if old-home
            (sb-posix:setenv "CLPM_HOME" old-home 1)
            (sb-posix:unsetenv "CLPM_HOME")))))

(format t "  Quicklisp trust pinning PASSED~%")
(format t "~%Quicklisp trust tests PASSED!~%")
(sb-ext:exit :code 0)
