;;;; update-selective-test.lisp - clpm update <system> only relaxes the named system.

(require :asdf)
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

(defun write-sexp (path form)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (let ((*print-case* :downcase))
      (prin1 form s)
      (terpri s))))

(defparameter *empty-sha*
  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")

(defun release-ref (pkg ver) (format nil "~A@~A" pkg ver))
(defun release-url (pkg ver)
  (format nil "https://example.invalid/~A-~A.tgz" pkg ver))

(defun write-release (root pkg ver system-deps)
  (write-sexp
   (merge-pathnames (format nil "registry/packages/~A/~A/release.sxp" pkg ver) root)
   `(:release :format 1
              :name ,pkg
              :version ,ver
              :source (:tarball :url ,(release-url pkg ver) :sha256 ,*empty-sha*)
              :artifact-sha256 ,*empty-sha*
              :systems (,pkg)
              :system-deps ,system-deps)))

(defun write-snapshot (root specs)
  ;; SPECS: list of (pkg ver)
  (let* ((releases (mapcar (lambda (s) (release-ref (first s) (second s))) specs))
         (provides (mapcar (lambda (s) (cons (first s)
                                             (release-ref (first s) (second s))))
                           specs)))
    (write-sexp
     (merge-pathnames "registry/snapshot.sxp" root)
     `(:snapshot :format 1 :generated-at "2026-02-01T00:00:00Z"
                 :releases ,releases
                 :provides ,provides))))

(defun git! (git args dir)
  (multiple-value-bind (o e rc)
      (clpm.platform:run-program (cons git args)
                                 :directory dir
                                 :error-output :string)
    (declare (ignore o e))
    (assert-true (zerop rc) "git ~{~A~^ ~} failed (rc=~A)" args rc)))

(defun seed-registry (remote git initial-specs)
  ;; Write registry files for INITIAL-SPECS, then git-init + commit.
  (dolist (s initial-specs)
    (ensure-directories-exist
     (merge-pathnames (format nil "registry/packages/~A/~A/"
                              (first s) (second s)) remote)))
  (write-snapshot remote initial-specs)
  (dolist (s initial-specs)
    (write-release remote (first s) (second s) '()))
  (git! git '("init") remote)
  (git! git '("config" "user.email" "test@example.com") remote)
  (git! git '("config" "user.name" "CLPM Test") remote)
  (git! git '("add" ".") remote)
  (git! git '("commit" "-m" "initial") remote))

(defun publish-additional (remote git all-specs)
  ;; Append new releases (snapshot rewritten with the union of specs), commit.
  (dolist (s all-specs)
    (ensure-directories-exist
     (merge-pathnames (format nil "registry/packages/~A/~A/"
                              (first s) (second s)) remote)))
  (write-snapshot remote all-specs)
  (dolist (s all-specs)
    (write-release remote (first s) (second s) '()))
  (git! git '("add" ".") remote)
  (git! git '("commit" "-m" "more releases") remote))

(defun find-locked (lock id)
  (find id (clpm.project:lockfile-resolved lock)
        :key #'clpm.project:locked-system-id :test #'string=))

(defun locked-version (lock id)
  (let ((l (find-locked lock id)))
    (and l (clpm.project:locked-release-version
            (clpm.project:locked-system-release l)))))

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (remote (merge-pathnames "remote-registry/" tmp))
         (proj (merge-pathnames "proj/" tmp))
         (git (clpm.platform:find-git))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (unless git (fail "git not on PATH"))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist remote)
    (ensure-directories-exist proj)
    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)

           ;; Initial registry has a@1.0.0 and b@1.0.0.
           (seed-registry remote git '(("a" "1.0.0") ("b" "1.0.0")))

           ;; Project depends on both with open constraints.
           (let ((url (format nil "file://~A" (namestring remote))))
             (clpm.io.sexp:write-canonical-sexp-to-file
              `(:project :name "proj" :version "0.1.0"
                         :systems ("proj")
                         :depends ((:a :system "a" :constraint nil)
                                   (:b :system "b" :constraint nil))
                         :registries ((:git :url ,url :name "main")))
              (merge-pathnames "clpm.project" proj)))

           (uiop:with-current-directory (proj)
             (assert-eql 0 (clpm:run-cli '("resolve"))))

           (let* ((lock (clpm.project:read-lock-file
                         (merge-pathnames "clpm.lock" proj))))
             (assert-true (string= "1.0.0" (locked-version lock "a"))
                          "expected a@1.0.0, got ~S" (locked-version lock "a"))
             (assert-true (string= "1.0.0" (locked-version lock "b"))
                          "expected b@1.0.0, got ~S" (locked-version lock "b")))

           ;; Publish a@1.1.0 and b@1.1.0.
           (publish-additional remote git
                               '(("a" "1.0.0") ("a" "1.1.0")
                                 ("b" "1.0.0") ("b" "1.1.0")))

           ;; clpm update a -> only a should bump.
           (format t "Selective update: only named system moves... ")
           (uiop:with-current-directory (proj)
             (assert-eql 0 (clpm:run-cli '("update" "a"))))
           (let ((lock (clpm.project:read-lock-file
                        (merge-pathnames "clpm.lock" proj))))
             (assert-true (string= "1.1.0" (locked-version lock "a"))
                          "expected a@1.1.0 after targeted update, got ~S"
                          (locked-version lock "a"))
             (assert-true (string= "1.0.0" (locked-version lock "b"))
                          "expected b@1.0.0 (held), got ~S"
                          (locked-version lock "b")))
           (format t "ok~%")

           ;; clpm update -> bumps everything.
           (format t "Full update: all systems move... ")
           (uiop:with-current-directory (proj)
             (assert-eql 0 (clpm:run-cli '("update"))))
           (let ((lock (clpm.project:read-lock-file
                        (merge-pathnames "clpm.lock" proj))))
             (assert-true (string= "1.1.0" (locked-version lock "b"))
                          "expected b@1.1.0 after full update, got ~S"
                          (locked-version lock "b")))
           (format t "ok~%")

           ;; clpm update unknown-system -> nonzero, lockfile unchanged.
           (format t "Unknown system errors out... ")
           (uiop:with-current-directory (proj)
             (let ((rc (clpm:run-cli '("update" "no-such-system"))))
               (unless (and (integerp rc) (not (zerop rc)))
                 (fail "expected non-zero rc for unknown system, got ~S" rc))))
           (format t "ok~%")

           ;; Forced-bump scenario: c@2 depends on b@^2, c@1 doesn't.
           ;; Lock at c@1 + b@1, then publish c@2 (requires b@^2) and
           ;; `clpm update c` -> b must also move even though it's unlocked.
           (format t "Untargeted system bumps when forced... ")
           ;; Reset registry to a clean two-package state.
           (uiop:delete-directory-tree remote :validate t)
           (ensure-directories-exist remote)
           (seed-registry remote git '(("b" "1.0.0") ("c" "1.0.0")))
           (let ((url (format nil "file://~A" (namestring remote))))
             (clpm.io.sexp:write-canonical-sexp-to-file
              `(:project :name "proj" :version "0.1.0"
                         :systems ("proj")
                         :depends ((:b :system "b" :constraint nil)
                                   (:c :system "c" :constraint nil))
                         :registries ((:git :url ,url :name "main")))
              (merge-pathnames "clpm.project" proj)))
           ;; Clear cached registry checkout so update-registry re-fetches.
           (uiop:delete-directory-tree clpm-home :validate t)
           (ensure-directories-exist clpm-home)
           (uiop:with-current-directory (proj)
             (assert-eql 0 (clpm:run-cli '("resolve"))))
           ;; Publish b@2.0.0 and c@2.0.0 with system-deps c -> b@^2.0.0.
           (dolist (s '(("b" "2.0.0") ("c" "2.0.0")))
             (ensure-directories-exist
              (merge-pathnames (format nil "registry/packages/~A/~A/"
                                       (first s) (second s)) remote)))
           (write-snapshot remote '(("b" "1.0.0") ("b" "2.0.0")
                                    ("c" "1.0.0") ("c" "2.0.0")))
           (write-release remote "b" "1.0.0" '())
           (write-release remote "b" "2.0.0" '())
           (write-release remote "c" "1.0.0" '())
           (write-release remote "c" "2.0.0"
                          (list (cons "c"
                                      (list (cons "b" '(:semver "^2.0.0"))))))
           (git! git '("add" ".") remote)
           (git! git '("commit" "-m" "v2") remote)
           ;; Update only c; b should be forced from 1.0.0 to 2.0.0.
           (uiop:with-current-directory (proj)
             (assert-eql 0 (clpm:run-cli '("update" "c"))))
           (let ((lock (clpm.project:read-lock-file
                        (merge-pathnames "clpm.lock" proj))))
             (assert-true (string= "2.0.0" (locked-version lock "c"))
                          "expected c@2.0.0, got ~S" (locked-version lock "c"))
             (assert-true (string= "2.0.0" (locked-version lock "b"))
                          "expected b@2.0.0 (forced by c@2), got ~S"
                          (locked-version lock "b")))
           (format t "ok~%"))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "~%Update selective tests PASSED!~%")
(sb-ext:exit :code 0)
