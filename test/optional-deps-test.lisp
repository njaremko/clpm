;;;; optional-deps-test.lisp - :optional t deps must be skipped unless opted in.

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

(defparameter *empty-sha*
  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")

(defun write-sexp (path form)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (let ((*print-case* :downcase))
      (prin1 form s)
      (terpri s))))

(defun git! (git args dir)
  (multiple-value-bind (o e rc)
      (clpm.platform:run-program (cons git args)
                                 :directory dir
                                 :error-output :string)
    (declare (ignore o e))
    (assert-true (zerop rc) "git ~{~A~^ ~} failed (rc=~A)" args rc)))

(defun seed-registry (remote git)
  ;; Two systems, no transitive deps. We exercise opt-in/out, not the solver.
  (dolist (pkg '(("a" "1.0.0") ("b" "1.0.0")))
    (ensure-directories-exist
     (merge-pathnames (format nil "registry/packages/~A/~A/"
                              (first pkg) (second pkg))
                      remote)))
  (write-sexp (merge-pathnames "registry/snapshot.sxp" remote)
              `(:snapshot :format 1 :generated-at "2026-02-01T00:00:00Z"
                          :releases ("a@1.0.0" "b@1.0.0")
                          :provides (("a" . "a@1.0.0") ("b" . "b@1.0.0"))))
  (dolist (pkg '(("a" "1.0.0") ("b" "1.0.0")))
    (write-sexp
     (merge-pathnames (format nil "registry/packages/~A/~A/release.sxp"
                              (first pkg) (second pkg))
                      remote)
     `(:release :format 1
                :name ,(first pkg)
                :version ,(second pkg)
                :source (:tarball
                         :url ,(format nil "https://example.invalid/~A-~A.tgz"
                                       (first pkg) (second pkg))
                         :sha256 ,*empty-sha*)
                :artifact-sha256 ,*empty-sha*
                :systems (,(first pkg))
                :system-deps ())))
  (git! git '("init") remote)
  (git! git '("config" "user.email" "test@example.com") remote)
  (git! git '("config" "user.name" "CLPM Test") remote)
  (git! git '("add" ".") remote)
  (git! git '("commit" "-m" "initial") remote))

(defun find-locked (lock id)
  (find id (clpm.project:lockfile-resolved lock)
        :key #'clpm.project:locked-system-id :test #'string=))

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
           (seed-registry remote git)
           ;; a = required, b = :optional t.
           (let ((url (format nil "file://~A" (namestring remote))))
             (clpm.io.sexp:write-canonical-sexp-to-file
              `(:project :name "proj" :version "0.1.0"
                         :systems ("proj")
                         :depends ((:a :system "a" :constraint nil)
                                   (:b :system "b" :constraint nil :optional t))
                         :registries ((:git :url ,url :name "main")))
              (merge-pathnames "clpm.project" proj)))

           ;; Default: no flags. b should be skipped.
           (format t "Optional dep skipped by default... ")
           (uiop:with-current-directory (proj)
             (assert-eql 0 (clpm:run-cli '("deps" "sync" "--to" "lock"))))
           (let ((lock (clpm.project:read-lock-file
                        (merge-pathnames "clpm.lock" proj))))
             (assert-true (find-locked lock "a") "a must be locked")
             (assert-true (not (find-locked lock "b")) "b must NOT be locked")
             (assert-true (null (clpm.project:lockfile-opted-in-optionals lock))
                          "no opt-ins should be persisted"))
           (format t "ok~%")

           ;; --with-optional b: b should be included AND persisted.
           (format t "--with-optional includes and persists... ")
           (uiop:with-current-directory (proj)
             (assert-eql 0 (clpm:run-cli '("--with-optional" "b" "deps" "sync" "--to" "lock"))))
           (let ((lock (clpm.project:read-lock-file
                        (merge-pathnames "clpm.lock" proj))))
             (assert-true (find-locked lock "b") "b must be locked after --with-optional b")
             (assert-true (equal '("b") (clpm.project:lockfile-opted-in-optionals lock))
                          "expected opted-in-optionals = (\"b\"), got ~S"
                          (clpm.project:lockfile-opted-in-optionals lock)))
           (format t "ok~%")

           ;; Subsequent lock sync without flags: persisted opt-in keeps b.
           (format t "Subsequent lock sync keeps persisted opt-in... ")
           (uiop:with-current-directory (proj)
             (assert-eql 0 (clpm:run-cli '("deps" "sync" "--to" "lock"))))
           (let ((lock (clpm.project:read-lock-file
                        (merge-pathnames "clpm.lock" proj))))
             (assert-true (find-locked lock "b")
                          "b must still be locked from persisted opt-in")
             (assert-true (equal '("b") (clpm.project:lockfile-opted-in-optionals lock))
                          "persisted opt-in survives unchanged"))
           (format t "ok~%")

           ;; --with-all-optional from scratch picks every optional.
           (format t "--with-all-optional picks all optionals... ")
           (delete-file (merge-pathnames "clpm.lock" proj))
           (uiop:with-current-directory (proj)
             (assert-eql 0 (clpm:run-cli '("--with-all-optional" "deps" "sync" "--to" "lock"))))
           (let ((lock (clpm.project:read-lock-file
                        (merge-pathnames "clpm.lock" proj))))
             (assert-true (find-locked lock "b") "b must be locked under --with-all-optional"))
           (format t "ok~%"))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "~%Optional deps tests PASSED!~%")
(sb-ext:exit :code 0)
