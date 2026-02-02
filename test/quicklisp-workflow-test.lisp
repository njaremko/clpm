;;;; test/quicklisp-workflow-test.lisp - End-to-end Quicklisp dist workflow

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

(defun read-file-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((data (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence data s)
      data)))

(defun %octets-prefix->string (octets end)
  (coerce (loop for i from 0 below end collect (code-char (aref octets i)))
          'string))

(defun %read-http-headers (stream)
  "Read request headers from STREAM (octet stream) and return a vector of octets."
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
  "Parse PATH from an HTTP request header octet vector."
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
  "Convert URL PATH like \"/dist/foo.txt\" into a safe relative pathname string."
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

(defun tar-gz-dir (src-dir dest-file)
  "Create a .tgz at DEST-FILE containing SRC-DIR as a top-level directory."
  (let* ((src-dir (uiop:ensure-directory-pathname src-dir))
         (parent (uiop:pathname-parent-directory-pathname src-dir))
         (parts (pathname-directory src-dir))
         (leaf (car (last parts))))
    (ensure-directories-exist dest-file)
    (multiple-value-bind (_out err rc)
        (clpm.platform:run-program
         (list (clpm.platform:find-tar)
               "-czf" (namestring dest-file)
               "-C" (namestring parent)
               leaf)
         :output :string
         :error-output :string
         :timeout 60)
      (declare (ignore _out))
      (unless (zerop rc)
        (fail "tar failed: ~A" err)))))

(defun make-ql-project (staging project prefix &key systems files)
  "Create a fake Quicklisp project tarball staging directory.

SYSTEMS is a list of system names. FILES is an alist of (relative-path . contents)."
  (let* ((top (merge-pathnames (format nil "~A/" prefix) staging)))
    (ensure-directories-exist top)
    (dolist (pair files)
      (let* ((rel (car pair))
             (contents (cdr pair))
             (path (merge-pathnames rel top)))
        (write-text path contents)))
    ;; A tiny marker file so it's easy to sanity-check extracted contents.
    (write-text (merge-pathnames "README.txt" top)
                (format nil "fake quicklisp project ~A (~A)~%" project (first systems)))
    top))

(format t "Testing Quicklisp dist workflow end-to-end...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (ws (merge-pathnames "ws/" tmp))
         (dist-root (merge-pathnames "dist-root/" tmp))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist ws)
    (ensure-directories-exist dist-root)

    (multiple-value-bind (base-url stop-server)
        (start-file-http-server dist-root)
      (unwind-protect
           (progn
	             ;; Build a tiny fake Quicklisp dist served over localhost.
	             (let* ((dist-dir (merge-pathnames "dist/" dist-root))
	                    (archive-dir (merge-pathnames "archive/" dist-root))
	                    (distinfo (merge-pathnames "dist/quicklisp.txt" dist-root))
	                    (systems-txt (merge-pathnames "dist/systems.txt" dist-root))
	                    (releases-txt (merge-pathnames "dist/releases.txt" dist-root))
                    (date "20260201")
                    (proj-b "ql-b")
                    (proj-m "ql-multi")
                    (proj-a "ql-a")
	                    (prefix-b (format nil "~A-~A-git" proj-b date))
	                    (prefix-m (format nil "~A-~A-git" proj-m date))
	                    (prefix-a (format nil "~A-~A-git" proj-a date))
	                    (tar-b (merge-pathnames (format nil "archive/~A/~A.tgz" proj-b prefix-b) dist-root))
	                    (tar-m (merge-pathnames (format nil "archive/~A/~A.tgz" proj-m prefix-m) dist-root))
	                    (tar-a (merge-pathnames (format nil "archive/~A/~A.tgz" proj-a prefix-a) dist-root))
	                    (stage (merge-pathnames "stage/" tmp))
	                    (sha1-b nil)
	                    (sha1-m nil)
	                    (sha1-a nil))
	               (ensure-directories-exist dist-dir)
	               (ensure-directories-exist archive-dir)
	               (ensure-directories-exist stage)
	               (labels ((content-sha1-hex (dir)
	                          (clpm.crypto.sha256:bytes-to-hex
	                           (clpm.crypto.sha1:sha1-files
	                            (mapcar #'cdr (clpm.io.fs:walk-files dir))))))

	               ;; ql-b
	               (let* ((dir-b (make-ql-project
	                              stage proj-b prefix-b
                              :systems (list proj-b)
                              :files (list
                                      (cons (format nil "~A.asd" proj-b)
                                            (with-output-to-string (s)
                                              (format s "(asdf:defsystem ~S~%  :version ~S~%  :serial t~%  :components ((:file ~S)))~%"
                                                      proj-b "0.0.0" proj-b)))
                                      (cons (format nil "~A.lisp" proj-b)
                                            (with-output-to-string (s)
	                                              (format s "(defpackage #:~A (:use #:cl) (:export #:value))~%" proj-b)
	                                              (format s "(in-package #:~A)~%" proj-b)
	                                              (format s "(defun value () \"B\")~%")))))))
	                 (ensure-directories-exist tar-b)
	                 (tar-gz-dir dir-b tar-b)
	                 (setf sha1-b (content-sha1-hex dir-b)))

	               ;; ql-multi (two systems in one .asd)
	               (let* ((dir-m (make-ql-project
	                              stage proj-m prefix-m
                              :systems (list proj-m "ql-multi/extra")
                              :files (list
                                      (cons (format nil "~A.asd" proj-m)
                                            (with-output-to-string (s)
                                              (format s "(asdf:defsystem ~S~%  :version ~S~%  :serial t~%  :components ((:file ~S)))~%~%"
                                                      proj-m "0.0.0" "ql-multi")
                                              (format s "(asdf:defsystem ~S~%  :version ~S~%  :depends-on (~S)~%  :serial t~%  :components ((:file ~S)))~%"
                                                      "ql-multi/extra" "0.0.0" proj-m "ql-multi-extra")))
                                      (cons "ql-multi.lisp"
                                            (with-output-to-string (s)
                                              (format s "(defpackage #:ql-multi (:use #:cl) (:export #:multi-value))~%")
                                              (format s "(in-package #:ql-multi)~%")
                                              (format s "(defun multi-value () \"M\")~%")))
                                      (cons "ql-multi-extra.lisp"
                                            (with-output-to-string (s)
	                                              (format s "(defpackage #:ql-multi-extra (:use #:cl) (:export #:extra-value))~%")
	                                              (format s "(in-package #:ql-multi-extra)~%")
	                                              (format s "(defun extra-value () \"MX\")~%")))))))
	                 (ensure-directories-exist tar-m)
	                 (tar-gz-dir dir-m tar-m)
	                 (setf sha1-m (content-sha1-hex dir-m)))

	               ;; ql-a depends on ql-b and ql-multi/extra
	               (let* ((dir-a (make-ql-project
	                              stage proj-a prefix-a
                              :systems (list proj-a)
                              :files (list
                                      (cons (format nil "~A.asd" proj-a)
                                            (with-output-to-string (s)
                                              (format s "(asdf:defsystem ~S~%  :version ~S~%  :depends-on (~S ~S)~%  :serial t~%  :components ((:file ~S)))~%"
                                                      proj-a "0.0.0" proj-b "ql-multi/extra" proj-a)))
                                      (cons (format nil "~A.lisp" proj-a)
                                            (with-output-to-string (s)
                                              (format s "(defpackage #:~A (:use #:cl) (:export #:greeting))~%" proj-a)
                                              (format s "(in-package #:~A)~%" proj-a)
	                                              (format s "(defun greeting (&optional (args nil))~%")
	                                              (format s "  (declare (ignore args))~%")
	                                              (format s "  (concatenate 'string \"A(\" (ql-b:value) \"/\" (ql-multi-extra:extra-value) \")\"))~%")))))))
	                 (ensure-directories-exist tar-a)
	                 (tar-gz-dir dir-a tar-a)
	                 (setf sha1-a (content-sha1-hex dir-a)))

	               ;; releases.txt needs tarball SHA-1
	                 (write-text
	                  distinfo
	                  (with-output-to-string (s)
	                    (format s "system-index-url: ~Adist/systems.txt~%" base-url)
                    (format s "release-index-url: ~Adist/releases.txt~%" base-url)))
                 (write-text
                  systems-txt
                  (with-output-to-string (s)
                    (format s "~A ~A.asd ~A~%" proj-b proj-b proj-b)
                    (format s "~A ~A.asd ~A~%" proj-m proj-m proj-m)
                    (format s "~A ~A.asd ~A ~A~%" proj-m proj-m "ql-multi/extra" proj-m)
                    (format s "~A ~A.asd ~A ~A ~A~%" proj-a proj-a proj-a proj-b "ql-multi/extra")))
                 (write-text
	                  releases-txt
	                  (with-output-to-string (s)
	                    (format s "~A ~Aarchive/~A/~A.tgz 0 0 ~A ~A ~A.asd~%"
	                            proj-b base-url proj-b prefix-b sha1-b prefix-b proj-b)
	                    (format s "~A ~Aarchive/~A/~A.tgz 0 0 ~A ~A ~A.asd~%"
	                            proj-m base-url proj-m prefix-m sha1-m prefix-m proj-m)
	                    (format s "~A ~Aarchive/~A/~A.tgz 0 0 ~A ~A ~A.asd~%"
	                            proj-a base-url proj-a prefix-a sha1-a prefix-a proj-a))))))

	             (unwind-protect
	                  (progn
	                    (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)
                    ;; Configure a quicklisp registry pointed at our local dist.
                    (assert-eql 0 (clpm:run-cli (list "registry" "add"
                                                      "--quicklisp"
                                                      "--name" "quicklisp-local"
                                                      "--url" (format nil "~Adist/quicklisp.txt" base-url))))
                    ;; Smoke: registry update should succeed too.
                    (assert-eql 0 (clpm:run-cli (list "registry" "update" "quicklisp-local")))

                    ;; Create a new project and add a Quicklisp dependency.
                    (assert-eql 0 (clpm:run-cli (list "new" "qlapp" "--bin" "--dir" (namestring ws))))

                    (let* ((app-root (merge-pathnames "qlapp/" ws))
                           (src-path (merge-pathnames "src/qlapp.lisp" app-root))
                           (lock-path (merge-pathnames "clpm.lock" app-root))
                           (dist-bin (merge-pathnames "dist/qlapp" app-root)))
                      (assert-true (uiop:directory-exists-p app-root)
                                   "Missing project root: ~A" (namestring app-root))

                      (uiop:with-current-directory (app-root)
                        ;; Add dependency (from Quicklisp dist) and install everything.
                        (let ((rc (clpm:run-cli (list "add" "ql-a" "--install"))))
                          (unless (eql rc 0)
                            ;; Dump build logs to help diagnose failures before temp cleanup.
                            (let* ((build-log-dir (merge-pathnames "build/" (clpm.platform:log-dir)))
                                   (logs (when (uiop:directory-exists-p build-log-dir)
                                           (directory (merge-pathnames "*.log" build-log-dir)))))
                              (when logs
                                (format *error-output* "~&Build logs:~%")
                                (dolist (p (sort (copy-list logs) #'string< :key #'namestring))
                                  (format *error-output* "~&==> ~A~%" (namestring p))
                                  (ignore-errors
                                    (format *error-output* "~A~%" (uiop:read-file-string p))))))
                            (fail "clpm add --install failed with exit code ~D" rc)))

                        ;; Update main to reference the dependency (ensures it really loads).
                        (write-text
                         src-path
                         (with-output-to-string (s)
                           (format s ";;;; qlapp.lisp~%~%")
                           (format s "(defpackage #:qlapp~%  (:use #:cl)~%  (:export #:main))~%~%")
                           (format s "(in-package #:qlapp)~%~%")
                           (format s "(defun main (&optional (args nil))~%")
                           (format s "  (declare (ignore args))~%")
	                           (format s "  (format t \"~~A~~%\" (uiop:symbol-call :ql-a :greeting))~%")
	                           (format s "  0)~%")))

                        ;; Lockfile should include the Quicklisp systems, with hashes backfilled.
                        (assert-true (uiop:file-exists-p lock-path)
                                     "Missing lockfile: ~A" (namestring lock-path))
                        (let* ((lock (clpm.project:read-lock-file lock-path))
                               (ids (sort (mapcar #'clpm.project:locked-system-id
                                                  (clpm.project:lockfile-resolved lock))
                                          #'string<)))
                          (dolist (id '("ql-a" "ql-b" "ql-multi" "ql-multi/extra"))
                            (assert-true (member id ids :test #'string=)
                                         "Expected ~A in lockfile, got ~S" id ids))
                          ;; Registry kind should be :quicklisp.
                          (let ((kinds (mapcar #'clpm.project:locked-registry-kind
                                               (clpm.project:lockfile-registries lock))))
                            (assert-true (member :quicklisp kinds :test #'eq)
                                         "Expected :quicklisp registry in lockfile, got ~S" kinds))
                          ;; Each Quicklisp tarball should have sha1 and a backfilled sha256.
                          (dolist (locked (clpm.project:lockfile-resolved lock))
                            (let* ((rel (clpm.project:locked-system-release locked))
                                   (src (clpm.project:locked-release-source rel)))
                              (when (and src (eq (clpm.project:locked-source-kind src) :tarball))
                                (assert-true (stringp (clpm.project:locked-source-sha1 src))
                                             "Expected sha1 for ~A"
                                             (clpm.project:locked-system-id locked))
                                (assert-true (stringp (clpm.project:locked-release-artifact-sha256 rel))
                                             "Expected artifact-sha256 backfill for ~A"
                                             (clpm.project:locked-system-id locked))
                                (assert-true (clpm.store:artifact-exists-p
                                              (clpm.project:locked-release-artifact-sha256 rel))
                                             "Artifact not in store for ~A"
                                             (clpm.project:locked-system-id locked))))))

                        ;; Run tests and package (exercises activation + build cache + save-lisp-and-die).
                        (assert-eql 0 (clpm:run-cli (list "test")))
                        (assert-eql 0 (clpm:run-cli (list "package")))
                        (assert-true (uiop:file-exists-p dist-bin)
                                     "Missing packaged binary: ~A" (namestring dist-bin))
                        (multiple-value-bind (out err rc)
                            (clpm.platform:run-program (list (namestring dist-bin))
                                                       :directory app-root
                                                       :output :string
                                                       :error-output :string
                                                       :timeout 60)
                          (declare (ignore err))
                          (assert-eql 0 rc)
                          (assert-true (search "A(B/MX)" out :test #'char-equal)
                                       "Expected output from dependency, got:~%~A"
                                       out)))))
               (progn
                 (if old-home
                     (sb-posix:setenv "CLPM_HOME" old-home 1)
                     (sb-posix:unsetenv "CLPM_HOME"))
                 (funcall stop-server)))))))

(format t "  Quicklisp workflow PASSED~%")
(format t "~%Quicklisp workflow tests PASSED!~%")
(sb-ext:exit :code 0)
