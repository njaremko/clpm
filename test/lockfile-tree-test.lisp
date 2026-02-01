;;;; test/lockfile-tree-test.lisp - Lockfile tree-sha256 backfill tests

(require :asdf)

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

(defun write-file (path contents)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (write-string contents s)))

(format t "Testing lockfile tree-sha256 backfill...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((project-root (merge-pathnames "proj/" tmp))
         (dep-root (merge-pathnames "dep/" tmp))
         (lock-path (merge-pathnames "clpm.lock" project-root)))
    (ensure-directories-exist project-root)
    (ensure-directories-exist dep-root)
    (write-file (merge-pathnames "dep.lisp" dep-root) "(in-package #:cl-user)\n")

    (let* ((expected-tree (clpm.crypto.sha256:bytes-to-hex
                           (clpm.crypto.sha256:sha256-tree dep-root :exclude nil)))
           (src (clpm.project:make-locked-source
                 :kind :path
                 :path (namestring dep-root)))
           (rel (clpm.project:make-locked-release
                 :name "dep"
                 :version "0.0.0"
                 :source src
                 :artifact-sha256 nil
                 :tree-sha256 nil))
           (sys (clpm.project:make-locked-system
                 :id "dep"
                 :release rel
                 :deps nil))
           (lock (clpm.project:make-lockfile
                  :format 1
                  :generated-at "2026-02-01T00:00:00Z"
                  :project-name "proj"
                  :clpm-version "0.1.0"
                  :registries nil
                  :resolved (list sys))))
      (clpm.project:write-lock-file lock lock-path)

      ;; Backfill tree hash and persist to disk.
      (let ((loaded (clpm.project:read-lock-file lock-path)))
        (clpm.fetch:fetch-lockfile-deps loaded :lockfile-path lock-path))

      (let* ((reloaded (clpm.project:read-lock-file lock-path))
             (locked (first (clpm.project:lockfile-resolved reloaded)))
             (release (clpm.project:locked-system-release locked))
             (tree (clpm.project:locked-release-tree-sha256 release)))
        (assert-true tree "tree-sha256 was not written to clpm.lock")
        (assert-true (string= tree expected-tree)
                     "tree-sha256 mismatch: expected ~A got ~A"
                     expected-tree tree)))))

(format t "  Lockfile tree-sha256 backfill PASSED~%")
(format t "~%Lockfile tree tests PASSED!~%")
(sb-ext:exit :code 0)

