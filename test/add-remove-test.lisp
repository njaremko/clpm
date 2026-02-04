;;;; test/add-remove-test.lisp - clpm add/remove command tests

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

      (dolist (spec '(("foo" "1.0.0")
                      ("foo" "2.0.0")
                      ("bar" "1.0.0")
                      ("bar" "2.0.0")))
        (destructuring-bind (pkg ver) spec
          (ensure-pkg pkg ver)
          (write-release pkg ver)))

      (let* ((releases (list (release-ref "foo" "1.0.0")
                             (release-ref "foo" "2.0.0")
                             (release-ref "bar" "1.0.0")
                             (release-ref "bar" "2.0.0")))
             (provides (list (cons "foo" (release-ref "foo" "1.0.0"))
                            (cons "foo" (release-ref "foo" "2.0.0"))
                            (cons "bar" (release-ref "bar" "1.0.0"))
                            (cons "bar" (release-ref "bar" "2.0.0")))))
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

(defun write-empty-project (project-root registry-url)
  (clpm.io.sexp:write-canonical-sexp-to-file
   `(:project
     :name "proj"
     :version "0.1.0"
     :systems ("proj")
     :depends ()
     :dev-depends ()
     :test-depends ()
     :registries ((:git :url ,registry-url :name "main"))
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

(format t "Testing clpm add/remove...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (remote (merge-pathnames "remote-registry/" tmp))
         (proj1 (merge-pathnames "proj1/" tmp))
         (proj2 (merge-pathnames "proj2/" tmp))
         (ws (merge-pathnames "ws/" tmp))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist remote)
    (ensure-directories-exist proj1)
    (ensure-directories-exist proj2)
    (ensure-directories-exist ws)

    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)

           (write-registry remote)
           (init-git-registry remote)
           (let ((url (format nil "file://~A" (namestring remote))))
             ;; Project 1: implicit constraint is nil (any version).
             (write-empty-project proj1 url)
             (uiop:with-current-directory (proj1)
               (assert-eql 0 (clpm:run-cli '("add" "foo"))))
             (let* ((project (clpm.project:read-project-file (merge-pathnames "clpm.project" proj1)))
                    (deps (clpm.project:project-depends project))
                    (dep (find-dep deps "foo")))
               (assert-eql 1 (length deps))
               (assert-true dep "Expected foo in depends")
               (assert-true (null (clpm.project:dependency-constraint dep))
                            "Expected nil constraint by default, got ~S"
                            (clpm.project:dependency-constraint dep)))

             ;; Idempotency: add again doesn't duplicate.
             (uiop:with-current-directory (proj1)
               (assert-eql 0 (clpm:run-cli '("add" "foo"))))
             (let* ((project (clpm.project:read-project-file (merge-pathnames "clpm.project" proj1)))
                    (deps (clpm.project:project-depends project)))
               (assert-eql 1 (length deps)))

             ;; Lockfile should resolve foo 2.0.0.
             (let* ((lock (clpm.project:read-lock-file (merge-pathnames "clpm.lock" proj1)))
                    (locked (find-locked lock "foo")))
               (assert-true locked "Expected foo in lockfile")
               (assert-string=
                "2.0.0"
                (clpm.project:locked-release-version
                 (clpm.project:locked-system-release locked))))

             ;; --caret should choose a caret constraint based on highest version.
             (uiop:with-current-directory (proj1)
               (assert-eql 0 (clpm:run-cli '("add" "--caret" "foo"))))
             (let* ((project (clpm.project:read-project-file (merge-pathnames "clpm.project" proj1)))
                    (deps (clpm.project:project-depends project))
                    (dep (find-dep deps "foo")))
               (assert-eql 1 (length deps))
               (assert-true dep "Expected foo in depends")
               (assert-true (equal '(:semver "^2.0.0")
                                   (clpm.project:dependency-constraint dep))
                            "Expected caret constraint for highest version, got ~S"
                            (clpm.project:dependency-constraint dep)))

             ;; Explicit semver constraint.
             (uiop:with-current-directory (proj1)
               (assert-eql 0 (clpm:run-cli '("add" "foo@^1.0.0"))))
             (let* ((project (clpm.project:read-project-file (merge-pathnames "clpm.project" proj1)))
                    (dep (find-dep (clpm.project:project-depends project) "foo")))
               (assert-true dep "Expected foo in depends after update")
               (assert-true (equal '(:semver "^1.0.0")
                                   (clpm.project:dependency-constraint dep))
                            "Expected updated constraint, got ~S"
                            (clpm.project:dependency-constraint dep)))
             (let* ((lock (clpm.project:read-lock-file (merge-pathnames "clpm.lock" proj1)))
                    (locked (find-locked lock "foo")))
               (assert-true locked "Expected foo in lockfile")
               (assert-string=
                "1.0.0"
                (clpm.project:locked-release-version
                 (clpm.project:locked-system-release locked))))

             ;; Exact constraint.
             (uiop:with-current-directory (proj1)
               (assert-eql 0 (clpm:run-cli '("add" "foo@=2.0.0"))))
             (let* ((lock (clpm.project:read-lock-file (merge-pathnames "clpm.lock" proj1)))
                    (locked (find-locked lock "foo")))
               (assert-true locked "Expected foo in lockfile")
               (assert-string=
                "2.0.0"
                (clpm.project:locked-release-version
                 (clpm.project:locked-system-release locked))))

             ;; Remove.
             (uiop:with-current-directory (proj1)
               (assert-eql 0 (clpm:run-cli '("remove" "foo"))))
             (let* ((project (clpm.project:read-project-file (merge-pathnames "clpm.project" proj1)))
                    (deps (clpm.project:project-depends project)))
               (assert-eql 0 (length deps)))

             ;; Project 2: dev/test sections.
             (write-empty-project proj2 url)
             (uiop:with-current-directory (proj2)
               (assert-eql 0 (clpm:run-cli '("add" "--dev" "foo@=1.0.0"))))
             (uiop:with-current-directory (proj2)
               (assert-eql 0 (clpm:run-cli '("add" "--test" "bar@=1.0.0"))))

             (let* ((project (clpm.project:read-project-file (merge-pathnames "clpm.project" proj2))))
               (assert-eql 0 (length (clpm.project:project-depends project)))
               (assert-true (find-dep (clpm.project:project-dev-depends project) "foo")
                            "Expected foo in dev-depends")
               (assert-true (find-dep (clpm.project:project-test-depends project) "bar")
                            "Expected bar in test-depends"))

             (uiop:with-current-directory (proj2)
               (assert-eql 0 (clpm:run-cli '("remove" "--dev" "foo"))))
             (let* ((lock (clpm.project:read-lock-file (merge-pathnames "clpm.lock" proj2))))
               (assert-true (null (find-locked lock "foo"))
                            "Expected foo removed from lockfile"))
             (uiop:with-current-directory (proj2)
               (assert-eql 0 (clpm:run-cli '("remove" "--test" "bar"))))

             (let* ((project (clpm.project:read-project-file (merge-pathnames "clpm.project" proj2))))
               (assert-eql 0 (length (clpm.project:project-dev-depends project)))
               (assert-eql 0 (length (clpm.project:project-test-depends project))))

             ;; Path + --install (activation config).
             (let* ((dep-root (merge-pathnames "dep/" ws))
                    (app-root (merge-pathnames "app/" ws)))
               (ensure-directories-exist dep-root)
               (ensure-directories-exist app-root)
               (with-open-file (s (merge-pathnames "dep.asd" dep-root)
                                  :direction :output
                                  :if-exists :supersede
                                  :external-format :utf-8)
                 (write-string
                  (format nil "~S~%"
                          '(asdf:defsystem "dep"
                             :version "0.1.0"
                             :serial t
                             :components ((:file "dep"))))
                  s))
               (with-open-file (s (merge-pathnames "dep.lisp" dep-root)
                                  :direction :output
                                  :if-exists :supersede
                                  :external-format :utf-8)
                 (write-string
                  "(defpackage #:dep (:use #:cl) (:export #:hello))\n(in-package #:dep)\n(defun hello () :ok)\n"
                  s))
               (with-open-file (s (merge-pathnames "clpm.project" app-root)
                                  :direction :output
                                  :if-exists :supersede
                                  :external-format :utf-8)
                 (write-string
                  (format nil "~S~%"
                          '(:project
                            :name "app"
                            :version "0.1.0"
                            :systems ("app")
                            :depends ()
                            :registries ()))
                  s))
               (uiop:with-current-directory (app-root)
                 (assert-eql 0 (clpm:run-cli '("add" "dep" "--path" "../dep" "--install"))))
               (let ((config-path (merge-pathnames ".clpm/asdf-config.lisp" app-root)))
                 (assert-true (uiop:file-exists-p config-path)
                              "Missing activation config: ~A" (namestring config-path))
                 (multiple-value-bind (output error-output exit-code)
                     (clpm.platform:run-program
                      (list "sbcl" "--noinform" "--non-interactive" "--disable-debugger"
                            "--load" (namestring config-path)
                            "--eval" "(asdf:load-system \"dep\")")
                      :output :string
                      :error-output :string
                      :timeout 60)
                   (declare (ignore output))
                   (assert-true (zerop exit-code)
                                "SBCL failed to load dep via activation config:~%~A"
                                error-output))))))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "~%Add/remove tests PASSED!~%")
(sb-ext:exit :code 0)
