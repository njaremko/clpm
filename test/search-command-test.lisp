;;;; test/search-command-test.lisp - Registry search command tests

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
  (unless (equal expected actual)
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

;;; Minimal HTTP file server for serving a fake Quicklisp dist (from quicklisp-workflow-test).

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
    (multiple-value-bind (addr port) (sb-bsd-sockets:socket-name sock)
      (declare (ignore addr))
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
                          (error (c)
                            (format *error-output* "~&http server error: ~A~%" c)
                            (ignore-errors
                              (%write-http-response stream 500 *empty-octets*)))))
                   (ignore-errors (sb-bsd-sockets:socket-close client))))))
        (labels ((server-loop ()
                   (loop until stop do
                     (handler-case
                         (multiple-value-bind (client _addr _port)
                             (sb-bsd-sockets:socket-accept sock)
                           (declare (ignore _addr _port))
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

(format t "Testing `clpm search` across git + Quicklisp...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (dist-root (merge-pathnames "dist-root/" tmp))
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
             ;; Fake Quicklisp dist served by localhost.
             (let* ((distinfo (merge-pathnames "dist/quicklisp.txt" dist-root))
                    (systems-txt (merge-pathnames "dist/systems.txt" dist-root))
                    (releases-txt (merge-pathnames "dist/releases.txt" dist-root))
                    (date "20260201")
                    (proj "ql-quick")
                    (prefix (format nil "~A-~A-git" proj date)))
               (write-text
                distinfo
                (with-output-to-string (s)
                  (format s "system-index-url: ~Adist/systems.txt~%" base-url)
                  (format s "release-index-url: ~Adist/releases.txt~%" base-url)))
               (write-text
                systems-txt
                (with-output-to-string (s)
                  (format s "~A ~A.asd ~A~%" proj proj proj)))
               (write-text
                releases-txt
                (with-output-to-string (s)
                  ;; fields: project url size file-md5 content-sha1 prefix asd
                  (format s "~A ~Aarchive/~A/~A.tgz 0 0 ~A ~A ~A.asd~%"
                          proj base-url proj prefix
                          (make-string 40 :initial-element #\0)
                          prefix proj))))

             ;; Local git registry (pre-populate clone location; no remote clone needed).
             (let* ((reg-root (merge-pathnames "registries/git-test/" clpm-home))
                    (snapshot (merge-pathnames "registry/snapshot.sxp" reg-root)))
               (write-sexp
                snapshot
                `(:snapshot :format 1 :generated-at "2026-02-01T00:00:00Z"
                  :releases ("pkg@1.0.0" "pkg@2.0.0")
                  :provides (("git-only" . "other@1.0.0")
                             ("ql-git" . "pkg@1.0.0")
                             ("ql-git" . "pkg@2.0.0")))))

             ;; Configure both registries.
             (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)
             (write-sexp
              (merge-pathnames "config/config.sxp" clpm-home)
              `(:config
                :format 1
                :registries ((:git :url "file:///unused" :name "git-test" :trust nil)
                             (:quicklisp :url ,(format nil "~Adist/quicklisp.txt" base-url)
                              :name "quicklisp-test" :trust nil))
                :defaults nil))

             ;; Search output should be deterministic and include both registries.
             (multiple-value-bind (code stdout stderr)
                 (run-cli-captured '("search" "ql-"))
               (declare (ignore stderr))
               (assert-eql 0 code)
               (assert-eql
                (format nil "ql-git~Cgit-test~Cpkg@2.0.0~%ql-quick~Cquicklisp-test~Cql-quick@0.0.20260201~%"
                        #\Tab #\Tab #\Tab #\Tab)
                stdout))

             ;; JSON output should be valid-ish and stable.
             (multiple-value-bind (code stdout stderr)
                 (run-cli-captured '("search" "ql-" "--json"))
               (declare (ignore stderr))
               (assert-eql 0 code)
               (assert-true (search "\"system\"" stdout) "Expected JSON output to include system key")
               (assert-true (search "\"registry\"" stdout) "Expected JSON output to include registry key")
               (assert-true (search "\"release\"" stdout) "Expected JSON output to include release key")))
        (progn
          (funcall stop-server)
          (if old-home
              (sb-posix:setenv "CLPM_HOME" old-home 1)
              (sb-posix:unsetenv "CLPM_HOME")))))))

(format t "  Search command tests PASSED~%")
(format t "~%Search command tests PASSED!~%")
(sb-ext:exit :code 0)
