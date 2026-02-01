;;;; test/solver-backtrack-test.lisp - Solver backtracking + lockfile preference tests

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

(defun write-sexp (path form)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (let ((*print-case* :downcase))
      (prin1 form s)
      (terpri s))))

(defun write-registry (root)
  (let ((empty-sha256 "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
    (labels ((release-ref (pkg ver)
               (format nil "~A@~A" pkg ver))
             (release-url (pkg ver)
               (format nil "https://example.invalid/~A-~A.tgz" pkg ver))
             (write-release (pkg ver system-deps)
               (write-sexp
                (merge-pathnames (format nil "registry/packages/~A/~A/release.sxp" pkg ver)
                                 root)
                `(:release :format 1
                  :name ,pkg
                  :version ,ver
                  :source (:tarball :url ,(release-url pkg ver) :sha256 ,empty-sha256)
                  :artifact-sha256 ,empty-sha256
                  :systems (,pkg)
                  :system-deps ,system-deps))))
      (ensure-directories-exist (merge-pathnames "registry/" root))
      (dolist (spec '(("b" "1.0.0")
                      ("b" "1.1.0")
                      ("c" "1.0.0")
                      ("d" "1.0.0")
                      ("d" "2.0.0")))
        (destructuring-bind (pkg ver) spec
          (ensure-directories-exist
           (merge-pathnames (format nil "registry/packages/~A/~A/" pkg ver) root))))

      ;; Snapshot: provides systems for multiple versions.
      (let* ((releases (list (release-ref "b" "1.0.0")
                             (release-ref "b" "1.1.0")
                             (release-ref "c" "1.0.0")
                             (release-ref "d" "1.0.0")
                             (release-ref "d" "2.0.0")))
             (provides (list (cons "b" (release-ref "b" "1.0.0"))
                            (cons "b" (release-ref "b" "1.1.0"))
                            (cons "c" (release-ref "c" "1.0.0"))
                            (cons "d" (release-ref "d" "1.0.0"))
                            (cons "d" (release-ref "d" "2.0.0")))))
        (write-sexp
         (merge-pathnames "registry/snapshot.sxp" root)
         `(:snapshot :format 1 :generated-at "2026-02-01T00:00:00Z"
           :releases ,releases
           :provides ,provides)))

      ;; b 1.1.0 depends on d ^2.0.0 (this will conflict with c).
      (write-release "b" "1.1.0"
                     (list (cons "b"
                                 (list (cons "d" '(:semver "^2.0.0"))))))
      ;; b 1.0.0 depends on d ^1.0.0 (compatible with c).
      (write-release "b" "1.0.0"
                     (list (cons "b"
                                 (list (cons "d" '(:semver "^1.0.0"))))))
      ;; c 1.0.0 depends on d ^1.0.0.
      (write-release "c" "1.0.0"
                     (list (cons "c"
                                 (list (cons "d" '(:semver "^1.0.0"))))))
      ;; d has both 1.0.0 and 2.0.0 with no deps.
      (write-release "d" "1.0.0" '())
      (write-release "d" "2.0.0" '()))

    nil))

(defun find-locked (lock system-id)
  (find system-id
        (clpm.project:lockfile-resolved lock)
        :key #'clpm.project:locked-system-id
        :test #'string=))

(format t "Testing solver backtracking...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (remote (merge-pathnames "remote-registry/" tmp))
         (project-root (merge-pathnames "proj/" tmp))
         (old-home (sb-posix:getenv "CLPM_HOME"))
         (git (clpm.platform:find-git)))
    (unless git
      (fail "git not found in PATH"))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist remote)
    (ensure-directories-exist project-root)

    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)

           ;; Create + commit a synthetic registry.
           (write-registry remote)
           (multiple-value-bind (o e rc)
               (clpm.platform:run-program (list git "init")
                                          :directory remote
                                          :error-output :string)
             (declare (ignore o e))
             (assert-true (zerop rc) "git init failed"))
           (dolist (cfg '(("user.email" "test@example.com")
                          ("user.name" "CLPM Test")))
             (multiple-value-bind (o e rc)
                 (clpm.platform:run-program (list git "config" (first cfg) (second cfg))
                                            :directory remote
                                            :error-output :string)
               (declare (ignore o e))
               (assert-true (zerop rc) "git config failed")))
           (multiple-value-bind (o e rc)
               (clpm.platform:run-program (list git "add" ".")
                                          :directory remote
                                          :error-output :string)
             (declare (ignore o e))
             (assert-true (zerop rc) "git add failed"))
           (multiple-value-bind (o e rc)
               (clpm.platform:run-program (list git "commit" "-m" "registry")
                                          :directory remote
                                          :error-output :string)
             (declare (ignore o e))
             (assert-true (zerop rc) "git commit failed"))

           ;; Project depends on b and c; picking b 1.1.0 would force d ^2.0.0
           ;; which conflicts with c -> d ^1.0.0, so the solver must backtrack
           ;; and pick b 1.0.0.
           (let ((url (format nil "file://~A" (namestring remote))))
             (clpm.io.sexp:write-canonical-sexp-to-file
              `(:project
                :name "proj"
                :version "0.1.0"
                :systems ("proj")
                :depends ((:b :system "b" :constraint (:semver "^1.0.0"))
                          (:c :system "c" :constraint (:semver "^1.0.0")))
                :registries ((:git :url ,url :name "main")))
              (merge-pathnames "clpm.project" project-root)))

           (uiop:with-current-directory (project-root)
             (assert-eql 0 (clpm:run-cli '("resolve"))))

           (let* ((lock (clpm.project:read-lock-file (merge-pathnames "clpm.lock" project-root)))
                  (locked-b (find-locked lock "b"))
                  (locked-c (find-locked lock "c"))
                  (locked-d (find-locked lock "d")))
             (assert-true locked-b "Expected b in lockfile")
             (assert-true locked-c "Expected c in lockfile")
             (assert-true locked-d "Expected d in lockfile")
             (assert-true (string= "1.0.0"
                                   (clpm.project:locked-release-version
                                    (clpm.project:locked-system-release locked-b)))
                          "Expected backtracking to select b 1.0.0, got ~S"
                          (clpm.project:locked-release-version
                           (clpm.project:locked-system-release locked-b))))

           (format t "  Backtracking PASSED~%")

           ;; Lockfile preference test: once b@1.0.0 is locked, loosening the
           ;; constraint should keep b@1.0.0 if still valid.
           (let ((url (format nil "file://~A" (namestring remote))))
             (clpm.io.sexp:write-canonical-sexp-to-file
              `(:project
                :name "proj"
                :version "0.1.0"
                :systems ("proj")
                :depends ((:b :system "b" :constraint (:exact "1.0.0")))
                :registries ((:git :url ,url :name "main")))
              (merge-pathnames "clpm.project" project-root)))
           (uiop:with-current-directory (project-root)
             (assert-eql 0 (clpm:run-cli '("resolve"))))
           ;; Loosen to any version; solver should keep locked selection.
           (let ((url (format nil "file://~A" (namestring remote))))
             (clpm.io.sexp:write-canonical-sexp-to-file
              `(:project
                :name "proj"
                :version "0.1.0"
                :systems ("proj")
                :depends ((:b :system "b" :constraint nil))
                :registries ((:git :url ,url :name "main")))
              (merge-pathnames "clpm.project" project-root)))
           (uiop:with-current-directory (project-root)
             (assert-eql 0 (clpm:run-cli '("resolve"))))
           (let* ((lock (clpm.project:read-lock-file (merge-pathnames "clpm.lock" project-root)))
                  (locked-b (find-locked lock "b")))
             (assert-true locked-b "Expected b in lockfile")
             (assert-true (string= "1.0.0"
                                   (clpm.project:locked-release-version
                                    (clpm.project:locked-system-release locked-b)))
                          "Expected lockfile preference to keep b 1.0.0, got ~S"
                          (clpm.project:locked-release-version
                           (clpm.project:locked-system-release locked-b))))
           (format t "  Lockfile preference PASSED~%"))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "~%Solver backtracking tests PASSED!~%")
(sb-ext:exit :code 0)
