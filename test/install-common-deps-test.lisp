;;;; test/install-common-deps-test.lisp - multi-add install with shared deps

(require :asdf)
(require :sb-posix)

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

(defun assert-true (x fmt &rest args)
  (unless x
    (apply #'fail fmt args)))

(defun assert-eql (expected actual)
  (unless (eql expected actual)
    (fail "Assertion failed: expected ~S, got ~S" expected actual)))

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

(defun sha256-file-hex (path)
  (clpm.crypto.sha256:bytes-to-hex
   (clpm.crypto.sha256:sha256-file path)))

(defun tar-gz-dir (src-dir dest-file)
  (let* ((src-dir (uiop:ensure-directory-pathname src-dir))
         (parent (uiop:pathname-parent-directory-pathname src-dir))
         (parts (pathname-directory src-dir))
         (leaf (car (last parts)))
         (tar (clpm.platform:find-tar)))
    (unless tar
      (fail "tar not found in PATH"))
    (ensure-directories-exist dest-file)
    (multiple-value-bind (_out err rc)
        (clpm.platform:run-program
         (list tar
               "-czf" (namestring dest-file)
               "-C" (namestring parent)
               leaf)
         :output :string
         :error-output :string
         :timeout 60)
      (declare (ignore _out))
      (unless (zerop rc)
        (fail "tar failed: ~A" err)))))

(defun dependency-source (system)
  (cond
    ((string= system "trivial-features")
     "(defpackage #:trivial-features
  (:use #:cl)
  (:export #:feature-name))

(in-package #:trivial-features)

(defun feature-name ()
  \"shared\")
")
    (t
     (with-output-to-string (s)
       (format s "(defpackage #:~A~%" system)
       (format s "  (:use #:cl)~%")
       (format s "  (:export #:value))~%~%")
       (format s "(in-package #:~A)~%~%" system)
       (format s "(defun value ()~%")
       (format s "  (trivial-features:feature-name))~%")))))

(defun write-source-release (stage archive-dir system depends)
  (let* ((version "1.0.0")
         (prefix (format nil "~A-~A/" system version))
         (source-dir (merge-pathnames prefix stage))
         (archive (merge-pathnames (format nil "~A-~A.tgz" system version)
                                   archive-dir))
         (asd (merge-pathnames (format nil "~A.asd" system) source-dir))
         (lisp (merge-pathnames (format nil "~A.lisp" system) source-dir)))
    (write-text
     asd
     (with-output-to-string (s)
       (format s "(asdf:defsystem ~S~%" system)
       (format s "  :version ~S~%" version)
       (when depends
         (format s "  :depends-on (~{~S~^ ~})~%" depends))
       (format s "  :serial t~%")
       (format s "  :components ((:file ~S)))~%" system)))
    (write-text lisp (dependency-source system))
    (tar-gz-dir source-dir archive)
    (values archive (sha256-file-hex archive))))

(defun write-registry (root)
  (let* ((stage (merge-pathnames "stage/" root))
         (archive-dir (merge-pathnames "archive/" root))
         (registry-dir (merge-pathnames "registry/" root))
         (release-specs '(("trivial-features" nil)
                          ("alexandria" ("trivial-features"))
                          ("bordeaux-threads" ("trivial-features")))))
    (ensure-directories-exist stage)
    (ensure-directories-exist archive-dir)
    (ensure-directories-exist registry-dir)
    (let ((release-refs '())
          (provides '()))
      (dolist (spec release-specs)
        (destructuring-bind (system depends) spec
          (multiple-value-bind (archive sha256)
              (write-source-release stage archive-dir system depends)
            (let ((release-ref (format nil "~A@1.0.0" system)))
              (push release-ref release-refs)
              (push (cons system release-ref) provides)
              (write-sexp
               (merge-pathnames (format nil "registry/packages/~A/1.0.0/release.sxp" system)
                                root)
               `(:release
                 :format 1
                 :name ,system
                 :version "1.0.0"
                 :source (:tarball :url ,(format nil "file://~A" (namestring archive))
                          :sha256 ,sha256)
                 :artifact-sha256 ,sha256
                 :systems (,system)
                 :system-deps ,(when depends
                                 (list (cons system
                                             (mapcar (lambda (dep)
                                                       (cons dep nil))
                                                     depends))))))))))
      (write-sexp
       (merge-pathnames "registry/snapshot.sxp" root)
       `(:snapshot
         :format 1
         :generated-at "2026-05-20T00:00:00Z"
         :releases ,(sort release-refs #'string<)
         :provides ,(sort provides #'string< :key #'car))))))

(defun init-git-registry (repo-path)
  (let ((git (clpm.platform:find-git)))
    (unless git
      (fail "git not found in PATH"))
    (multiple-value-bind (o e rc)
        (clpm.platform:run-program (list git "init")
                                   :directory repo-path
                                   :error-output :string)
      (declare (ignore o e))
      (assert-true (zerop rc) "git init failed"))
    (dolist (cfg '(("user.email" "test@example.com")
                   ("user.name" "CLPM Test")))
      (multiple-value-bind (o e rc)
          (clpm.platform:run-program (list git "config" (first cfg) (second cfg))
                                     :directory repo-path
                                     :error-output :string)
        (declare (ignore o e))
        (assert-true (zerop rc) "git config failed")))
    (multiple-value-bind (o e rc)
        (clpm.platform:run-program (list git "add" ".")
                                   :directory repo-path
                                   :error-output :string)
      (declare (ignore o e))
      (assert-true (zerop rc) "git add failed"))
    (multiple-value-bind (o e rc)
        (clpm.platform:run-program (list git "commit" "-m" "registry")
                                   :directory repo-path
                                   :error-output :string)
      (declare (ignore o e))
      (assert-true (zerop rc) "git commit failed"))))

(defun write-project (project-root registry-url)
  (clpm.io.sexp:write-canonical-sexp-to-file
   `(:project
     :name "app"
     :version "0.1.0"
     :systems ("app")
     :depends ()
     :registries ((:git :url ,registry-url :name "main"))
     :scripts ())
   (merge-pathnames "clpm.project" project-root)))

(defun run-cli-captured (args)
  (let ((stdout (make-string-output-stream))
        (stderr (make-string-output-stream))
        (rc nil))
    (let ((*standard-output* stdout)
          (*error-output* stderr))
      (setf rc (clpm:run-cli args)))
    (values rc
            (get-output-stream-string stdout)
            (get-output-stream-string stderr))))

(defun assert-no-mismatch-output (stdout stderr)
  (assert-true (null (search "mismatch" stdout :test #'char-equal))
               "Unexpected mismatch in stdout:~%~A" stdout)
  (assert-true (null (search "mismatch" stderr :test #'char-equal))
               "Unexpected mismatch in stderr:~%~A" stderr))

(defun find-locked (lock system-id)
  (find system-id
        (clpm.project:lockfile-resolved lock)
        :key #'clpm.project:locked-system-id
        :test #'string=))

(format t "Testing install of multiple common dependencies...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (remote (merge-pathnames "remote-registry/" tmp))
         (project-root (merge-pathnames "app/" tmp))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist remote)
    (ensure-directories-exist project-root)
    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)
           (write-registry remote)
           (init-git-registry remote)
           (write-project project-root (format nil "file://~A" (namestring remote)))
           (uiop:with-current-directory (project-root)
             (multiple-value-bind (rc stdout stderr)
                 (run-cli-captured '("add" "alexandria" "bordeaux-threads"))
               (assert-eql 0 rc)
               (assert-no-mismatch-output stdout stderr))
             (multiple-value-bind (rc stdout stderr)
                 (run-cli-captured '("install"))
               (assert-eql 0 rc)
               (assert-no-mismatch-output stdout stderr)))
           (let* ((lock (clpm.project:read-lock-file
                         (merge-pathnames "clpm.lock" project-root)))
                  (ids (sort (mapcar #'clpm.project:locked-system-id
                                     (clpm.project:lockfile-resolved lock))
                             #'string<)))
             (assert-true (equal ids
                                 '("alexandria"
                                   "bordeaux-threads"
                                   "trivial-features"))
                          "Unexpected lockfile systems: ~S" ids)
             (dolist (id ids)
               (let* ((locked (find-locked lock id))
                      (release (clpm.project:locked-system-release locked)))
                 (assert-true (stringp (clpm.project:locked-release-artifact-sha256 release))
                              "Expected artifact SHA-256 for ~A" id)
                 (assert-true (stringp (clpm.project:locked-release-tree-sha256 release))
                              "Expected tree SHA-256 for ~A" id))))
           (assert-true (uiop:file-exists-p
                         (merge-pathnames ".clpm/asdf-config.lisp" project-root))
                        "Expected activation config"))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "  Multiple common dependency install PASSED~%")
(format t "~%Install common deps tests PASSED!~%")
(sb-ext:exit :code 0)
