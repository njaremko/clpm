;;;; test/example-workflow-test.lisp - End-to-end example workspace workflow

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

(defun assert-eql (expected actual)
  (unless (eql expected actual)
    (fail "Assertion failed: expected ~S, got ~S" expected actual)))

(defun assert-true (x fmt &rest args)
  (unless x
    (apply #'fail fmt args)))

(defun copy-workspace-tree (src dest)
  (let ((src (uiop:ensure-directory-pathname src))
        (dest (uiop:ensure-directory-pathname dest)))
    (ensure-directories-exist dest)
    (dolist (entry (clpm.io.fs:walk-files src))
      (let* ((rel (car entry))
             (src-path (cdr entry))
             (dest-path (merge-pathnames rel dest)))
        (ensure-directories-exist dest-path)
        (uiop:copy-file src-path dest-path)))))

(format t "Testing example workspace end-to-end...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (ws (merge-pathnames "ws/" tmp))
         (old-home (sb-posix:getenv "CLPM_HOME"))
         (this-file (or *load-truename* *load-pathname*))
         (test-dir (uiop:pathname-directory-pathname this-file))
         (repo-root (uiop:pathname-parent-directory-pathname test-dir))
         (example-root (merge-pathnames "example/workspace/" repo-root)))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist ws)
    (assert-true (uiop:directory-exists-p example-root)
                 "Missing example workspace directory: ~A" (namestring example-root))

    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)
           ;; Copy example workspace into temp so tests don't dirty the repo.
           (copy-workspace-tree example-root ws)

           (let* ((app-root (merge-pathnames "app/" ws))
                  (lock-path (merge-pathnames "clpm.lock" app-root))
                  (dist-bin (merge-pathnames "dist/app" app-root)))
             (assert-true (uiop:directory-exists-p app-root)
                          "Missing app root: ~A" (namestring app-root))

             (uiop:with-current-directory (app-root)
               ;; Doctor should succeed (registries are optional for path-only projects).
               (assert-eql 0 (clpm:run-cli '("doctor")))

               ;; Install with parallel jobs (exercises solver + fetch + build + activation).
               (assert-eql 0 (clpm:run-cli '("-j" "4" "install")))

               ;; Lockfile should include transitive path dependencies (lib-a -> lib-b).
               (assert-true (uiop:file-exists-p lock-path)
                            "Missing lockfile: ~A" (namestring lock-path))
               (let* ((lock (clpm.project:read-lock-file lock-path))
                      (ids (sort (mapcar #'clpm.project:locked-system-id
                                         (clpm.project:lockfile-resolved lock))
                                 #'string<)))
                 (assert-true (member "lib-a" ids :test #'string=)
                              "Expected lib-a in lockfile, got ~S" ids)
                 (assert-true (member "lib-b" ids :test #'string=)
                              "Expected lib-b in lockfile, got ~S" ids))

               ;; Run tests.
               (assert-eql 0 (clpm:run-cli '("test")))

               ;; Run entrypoint with forwarded args.
               (assert-eql 0 (clpm:run-cli '("run" "--" "hello" "world")))

               ;; Exec should set CLPM_PROJECT_ROOT (quick smoke check).
               (assert-eql 0 (clpm:run-cli '("exec" "--" "sh" "-c"
                                             "test -n \"$CLPM_PROJECT_ROOT\"")))

               ;; Package should produce a runnable executable in dist/.
               (assert-eql 0 (clpm:run-cli '("package")))
               (assert-true (uiop:file-exists-p dist-bin)
                            "Missing packaged binary: ~A" (namestring dist-bin))
               (multiple-value-bind (out err rc)
                   (clpm.platform:run-program (list (namestring dist-bin) "hello" "world")
                                              :directory app-root
                                              :output :string
                                              :error-output :string
                                              :timeout 60)
                 (declare (ignore err))
                 (assert-eql 0 rc)
                 (assert-true (search "Example app running" out :test #'char-equal)
                              "Expected packaged binary output, got:~%~A"
                              out))

               ;; Clean project-local outputs (including dist/).
               (assert-eql 0 (clpm:run-cli '("clean" "--dist")))
               (assert-true (not (uiop:directory-exists-p (merge-pathnames "dist/" app-root)))
                            "Expected dist/ to be removed")

               ;; GC should run (root-aware).
               (assert-eql 0 (clpm:run-cli '("gc"))))))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "  Example workspace workflow PASSED~%")
(format t "~%Example workflow tests PASSED!~%")
(sb-ext:exit :code 0)
