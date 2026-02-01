;;;; test/solver-explain-test.lisp - Deterministic conflict explanation tests

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
      (dolist (spec '(("b" "1.1.0")
                      ("c" "1.0.0")
                      ("d" "1.0.0")
                      ("d" "2.0.0")))
        (destructuring-bind (pkg ver) spec
          (ensure-directories-exist
           (merge-pathnames (format nil "registry/packages/~A/~A/" pkg ver) root))))

      ;; Snapshot: only b@1.1.0 exists, so resolution must fail.
      (let* ((releases (list (release-ref "b" "1.1.0")
                             (release-ref "c" "1.0.0")
                             (release-ref "d" "1.0.0")
                             (release-ref "d" "2.0.0")))
             (provides (list (cons "b" (release-ref "b" "1.1.0"))
                            (cons "c" (release-ref "c" "1.0.0"))
                            (cons "d" (release-ref "d" "1.0.0"))
                            (cons "d" (release-ref "d" "2.0.0")))))
        (write-sexp
         (merge-pathnames "registry/snapshot.sxp" root)
         `(:snapshot :format 1 :generated-at "2026-02-01T00:00:00Z"
           :releases ,releases
           :provides ,provides)))

      ;; b 1.1.0 depends on d ^2.0.0; c depends on d ^1.0.0: unsatisfiable.
      (write-release "b" "1.1.0"
                     (list (cons "b"
                                 (list (cons "d" '(:semver "^2.0.0"))))))
      (write-release "c" "1.0.0"
                     (list (cons "c"
                                 (list (cons "d" '(:semver "^1.0.0"))))))
      (write-release "d" "1.0.0" '())
      (write-release "d" "2.0.0" '()))

    nil))

(format t "Testing conflict explanation output...~%")

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

           (write-registry remote)
           ;; Init git repo + commit (so clone-registry can clone it).
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

           ;; Project depends on b and c.
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

           (let ((out (make-string-output-stream))
                 (err (make-string-output-stream)))
             (let ((*standard-output* out)
                   (*error-output* err))
               (uiop:with-current-directory (project-root)
                 (assert-eql 2 (clpm:run-cli '("resolve")))))
             (let ((err-str (get-output-stream-string err)))
               (assert-true (search "Conflict chain:" err-str)
                            "Expected 'Conflict chain' in stderr, got:~%~A"
                            err-str)
               (assert-true (search "b@1.1.0 requires d" err-str)
                            "Expected b->d edge in stderr, got:~%~A"
                            err-str)
               (assert-true (search "c@1.0.0 requires d" err-str)
                            "Expected c->d edge in stderr, got:~%~A"
                            err-str))))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "  Conflict explanation output PASSED~%")
(format t "~%Solver explain tests PASSED!~%")
(sb-ext:exit :code 0)

