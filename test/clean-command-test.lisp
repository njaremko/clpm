;;;; test/clean-command-test.lisp - `clpm clean` tests

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

(format t "Testing `clpm clean`...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (workspace (merge-pathnames "ws/" tmp))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist workspace)

    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)

           ;; Create a project.
           (uiop:with-current-directory (workspace)
             (assert-eql 0 (clpm:run-cli '("new" "app" "--bin"))))

           (let* ((project-root (merge-pathnames "app/" workspace))
                  (clpm-dir (merge-pathnames ".clpm/" project-root))
                  (dist-dir (merge-pathnames "dist/" project-root))
                  (keep-dir (merge-pathnames "keep/" project-root))
                  (keep-file (merge-pathnames "keep/keep.txt" project-root)))
             (ensure-directories-exist (merge-pathnames "x" clpm-dir))
             (ensure-directories-exist (merge-pathnames "y" dist-dir))
             (ensure-directories-exist (merge-pathnames "z" keep-dir))
             (with-open-file (s keep-file :direction :output :if-exists :supersede)
               (write-string "keep" s))

             ;; Default: removes .clpm only.
             (uiop:with-current-directory (project-root)
               (assert-eql 0 (clpm:run-cli '("clean"))))
             (assert-true (not (uiop:directory-exists-p clpm-dir))
                          "Expected .clpm to be removed")
             (assert-true (uiop:directory-exists-p dist-dir)
                          "Expected dist to remain without --dist")
             (assert-true (uiop:file-exists-p keep-file)
                          "Expected keep file to remain")
             (assert-true (uiop:file-exists-p (merge-pathnames "clpm.project" project-root))
                          "Expected clpm.project to remain")

             ;; With --dist: removes both .clpm and dist.
             (ensure-directories-exist (merge-pathnames "x" clpm-dir))
             (uiop:with-current-directory (project-root)
               (assert-eql 0 (clpm:run-cli '("clean" "--dist"))))
             (assert-true (not (uiop:directory-exists-p clpm-dir))
                          "Expected .clpm to be removed with --dist")
             (assert-true (not (uiop:directory-exists-p dist-dir))
                          "Expected dist to be removed with --dist")
             (assert-true (uiop:file-exists-p keep-file)
                          "Expected keep file to remain after --dist")))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "  `clpm clean` PASSED~%")
(format t "~%Clean command tests PASSED!~%")
(sb-ext:exit :code 0)
