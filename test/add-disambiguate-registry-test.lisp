;;;; test/add-disambiguate-registry-test.lisp - clpm add --registry disambiguation tests

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

(defun assert-string= (expected actual)
  (unless (string= expected actual)
    (fail "Assertion failed: expected ~S, got ~S" expected actual)))

(defun assert-contains (haystack needle)
  (assert-true (and (stringp haystack)
                    (search needle haystack :test #'char-equal))
               "Expected output to contain ~S, got:~%~A"
               needle
               haystack))

(defun run-cli-captured (args)
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((*standard-output* out)
          (*error-output* err))
      (let ((code (clpm:run-cli args)))
        (values code
                (get-output-stream-string out)
                (get-output-stream-string err))))))

(defun write-sexp (path form)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (let ((*print-case* :downcase))
      (prin1 form s)
      (terpri s))))

(defun write-registry (root tag versions)
  (let ((empty-sha256 "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
    (labels ((release-ref (pkg ver)
               (format nil "~A@~A" pkg ver))
             (release-url (pkg ver)
               (format nil "https://example.invalid/~A/~A-~A.tgz" tag pkg ver))
             (write-release (pkg ver)
               (write-sexp
                (merge-pathnames (format nil "registry/packages/~A/~A/release.sxp" pkg ver)
                                 root)
                `(:release :format 1
                  :name ,pkg
                  :version ,ver
                  :source (:tarball :url ,(release-url pkg ver) :sha256 ,empty-sha256)
                  :artifact-sha256 ,empty-sha256
                  :systems (,pkg)
                  :system-deps ())))
             (ensure-pkg (pkg ver)
               (ensure-directories-exist
                (merge-pathnames (format nil "registry/packages/~A/~A/" pkg ver) root))))
      (ensure-directories-exist (merge-pathnames "registry/" root))
      (dolist (ver versions)
        (ensure-pkg "foo" ver)
        (write-release "foo" ver))
      (let* ((releases (mapcar (lambda (ver) (release-ref "foo" ver)) versions))
             (provides (mapcar (lambda (ver) (cons "foo" (release-ref "foo" ver))) versions)))
        (write-sexp
         (merge-pathnames "registry/snapshot.sxp" root)
         `(:snapshot :format 1 :generated-at "2026-02-01T00:00:00Z"
           :releases ,releases
           :provides ,provides)))))
  nil)

(defun init-git-registry (repo-path)
  (let ((git (clpm.platform:find-git)))
    (unless git
      (fail "git not found in PATH"))
    (multiple-value-bind (o e rc)
        (clpm.platform:run-program (list git "init" "-b" "main")
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

(defun write-project (project-root registry-a-url registry-b-url)
  (clpm.io.sexp:write-canonical-sexp-to-file
   `(:project
     :name "proj"
     :version "0.1.0"
     :systems ("proj")
     :depends ()
     :dev-depends ()
     :test-depends ()
     :registries ((:git :url ,registry-a-url :name "a")
                  (:git :url ,registry-b-url :name "b"))
     :scripts ())
   (merge-pathnames "clpm.project" project-root)))

(defun find-dep (deps system-id)
  (find system-id deps
        :key #'clpm.project:dependency-system
        :test #'string=))

(defun find-locked (lock system-id)
  (find system-id
        (clpm.project:lockfile-resolved lock)
        :key #'clpm.project:locked-system-id
        :test #'string=))

(format t "Testing clpm add --registry disambiguation...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (remote-a (merge-pathnames "remote-a/" tmp))
         (remote-b (merge-pathnames "remote-b/" tmp))
         (proj (merge-pathnames "proj/" tmp))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist remote-a)
    (ensure-directories-exist remote-b)
    (ensure-directories-exist proj)

    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)

           (write-registry remote-a "a" '("1.0.0"))
           (write-registry remote-b "b" '("2.0.0"))
           (init-git-registry remote-a)
           (init-git-registry remote-b)

           (let ((url-a (format nil "file://~A" (namestring remote-a)))
                 (url-b (format nil "file://~A" (namestring remote-b))))
             (write-project proj url-a url-b)

             ;; No --registry: should error and list providers deterministically.
             (uiop:with-current-directory (proj)
               (multiple-value-bind (code stdout stderr)
                   (run-cli-captured '("add" "foo"))
                 (declare (ignore stdout))
                 (assert-eql 1 code)
                 (assert-contains stderr "multiple registries")
                 (assert-contains stderr "  a")
                 (assert-contains stderr "  b")))

             ;; Unknown registry name: should error and list providers.
             (uiop:with-current-directory (proj)
               (multiple-value-bind (code stdout stderr)
                   (run-cli-captured '("add" "--registry" "nope" "foo"))
                 (declare (ignore stdout))
                 (assert-eql 1 code)
                 (assert-contains stderr "does not provide")
                 (assert-contains stderr "  a")
                 (assert-contains stderr "  b")))

             ;; With --registry: should succeed and pin to that registry.
             (uiop:with-current-directory (proj)
               (assert-eql 0 (clpm:run-cli '("add" "--registry" "a" "foo"))))

             (let* ((project (clpm.project:read-project-file (merge-pathnames "clpm.project" proj)))
                    (deps (clpm.project:project-depends project))
                    (dep (find-dep deps "foo")))
               (assert-eql 1 (length deps))
               (assert-true dep "Expected foo in depends")
               (assert-true (null (clpm.project:dependency-constraint dep))
                            "Expected nil constraint by default, got ~S"
                            (clpm.project:dependency-constraint dep))
               (assert-true (equal '(:registry "a")
                                   (clpm.project:dependency-source dep))
                            "Expected dependency source pinned to registry a, got ~S"
                            (clpm.project:dependency-source dep)))

             (let* ((lock (clpm.project:read-lock-file (merge-pathnames "clpm.lock" proj)))
                    (locked (find-locked lock "foo")))
               (assert-true locked "Expected foo in lockfile")
               (assert-string=
                "1.0.0"
                (clpm.project:locked-release-version
                 (clpm.project:locked-system-release locked)))
               (assert-contains
                (clpm.project:locked-source-url
                 (clpm.project:locked-release-source
                  (clpm.project:locked-system-release locked)))
                "/a/"))))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "~%Add --registry tests PASSED!~%")
(sb-ext:exit :code 0)

