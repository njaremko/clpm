;;;; test/run-exec-test.lisp - clpm run/exec command tests

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

(defun write-file (path contents)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (write-string contents s)))

(format t "Testing clpm run/exec...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (project-root (merge-pathnames "app/" tmp))
         (src-dir (merge-pathnames "src/" project-root))
         (dep-root (merge-pathnames "dep/" tmp))
         (dep-src-dir (merge-pathnames "src/" dep-root))
         (asdf-config (merge-pathnames ".clpm/asdf-config.lisp" project-root))
         (env-path (merge-pathnames ".clpm/env.sexp" project-root))
         (lock-path (merge-pathnames "clpm.lock" project-root))
         (args-path (merge-pathnames "run-args.sexp" project-root))
         (loaded-path (merge-pathnames "exec-loaded.txt" project-root))
         (dep-loaded-path (merge-pathnames "dep-loaded.txt" project-root))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist project-root)
    (ensure-directories-exist src-dir)
    (ensure-directories-exist dep-root)
    (ensure-directories-exist dep-src-dir)

    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)

           ;; Minimal ASDF system with an entrypoint that records args.
           (write-file
            (merge-pathnames "app.asd" project-root)
            (format nil "~S~%"
                    '(asdf:defsystem "app"
                       :version "0.1.0"
                       :serial t
                       :pathname "src"
                       :components ((:file "app")))))
           (write-file
            (merge-pathnames "src/app.lisp" project-root)
            (with-output-to-string (s)
              (format s "(defpackage #:app (:use #:cl) (:export #:main))~%")
              (format s "(in-package #:app)~%")
              (format s "(defun main (args)~%")
              (format s "  (with-open-file (s \"run-args.sexp\" :direction :output :if-exists :supersede :external-format :utf-8)~%")
              (format s "    (prin1 args s)~%")
              (format s "    (terpri s))~%")
              (format s "  17)~%")))

           ;; Project manifest with :run entry.
           (clpm.io.sexp:write-canonical-sexp-to-file
            `(:project
              :name "app"
              :version "0.1.0"
              :systems ("app")
              :run (:system "app" :function "app::main")
              :depends ()
              :dev-depends ()
              :test-depends ()
              :registries ()
              :scripts ())
            (merge-pathnames "clpm.project" project-root))

           ;; `clpm run` should auto-install/activate if needed and propagate exit code.
           (uiop:with-current-directory (project-root)
             (assert-eql 17 (clpm:run-cli '("run" "--" "hello" "world"))))
           (assert-true (uiop:file-exists-p env-path)
                        "Missing activation env: ~A" (namestring env-path))
           (assert-true (uiop:file-exists-p lock-path)
                        "Missing lockfile: ~A" (namestring lock-path))
           (let* ((env (clpm.io.sexp:read-safe-sexp-from-file env-path))
                  (env-lock (and (consp env) (eq (car env) :env)
                                 (getf (cdr env) :lockfile-sha256)))
                  (cur-lock (clpm.crypto.sha256:bytes-to-hex
                             (clpm.crypto.sha256:sha256-file lock-path))))
             (assert-true (and (stringp env-lock) (stringp cur-lock))
                          "Expected :lockfile-sha256 in env.sexp")
             (assert-true (string= env-lock cur-lock)
                          "Expected env lock hash to match clpm.lock"))
           (assert-true (uiop:file-exists-p args-path)
                        "Missing args file: ~A" (namestring args-path))
           (let ((args (clpm.io.sexp:read-safe-sexp-from-file args-path)))
             (assert-true (equal '("hello" "world") args)
                          "Unexpected args: ~S" args))

           ;; `clpm exec` should set CLPM_PROJECT_ROOT for non-sbcl commands.
           (uiop:with-current-directory (project-root)
             (assert-eql 0 (clpm:run-cli '("exec" "--" "sh" "-c" "test -n \"$CLPM_PROJECT_ROOT\""))))

           ;; `clpm exec -- sbcl ...` should inject --load asdf-config.lisp.
           (assert-true (uiop:file-exists-p asdf-config)
                        "Missing activation config: ~A" (namestring asdf-config))
           (write-file loaded-path "")
           (delete-file loaded-path)
           (with-open-file (s asdf-config :direction :output
                                          :if-exists :append
                                          :external-format :utf-8)
             (format s "~%~A~%"
                     "(with-open-file (s \"exec-loaded.txt\" :direction :output :if-exists :supersede :external-format :utf-8)\n  (write-string \"loaded\" s))"))
           (uiop:with-current-directory (project-root)
             (assert-eql 0 (clpm:run-cli '("exec" "--" "sbcl" "--noinform" "--non-interactive" "--disable-debugger"
                                           "--eval" "(sb-ext:exit :code 0)"))))
           (assert-true (uiop:file-exists-p loaded-path)
                        "Expected exec-loaded.txt to be created by injected --load"))

           ;; Stale activation should be detected and repaired automatically.
           ;;
           ;; Add a new local dependency via `clpm add --path` (this updates the
           ;; manifest + lockfile but does not activate by default), then `clpm run`
           ;; should run `clpm install` to refresh activation before launching.
           (write-file
            (merge-pathnames "dep.asd" dep-root)
            (format nil "~S~%"
                    '(asdf:defsystem "dep"
                       :version "0.1.0"
                       :serial t
                       :pathname "src"
                       :components ((:file "dep")))))
           (write-file
            (merge-pathnames "src/dep.lisp" dep-root)
            (with-output-to-string (s)
              (format s "(defpackage #:dep (:use #:cl))~%")
              (format s "(in-package #:dep)~%")
              (format s "(with-open-file (s \"dep-loaded.txt\" :direction :output :if-exists :supersede :external-format :utf-8)~%")
              (format s "  (write-string \"loaded\" s))~%")))
           (when (uiop:file-exists-p dep-loaded-path)
             (delete-file dep-loaded-path))
           (uiop:with-current-directory (project-root)
             (assert-eql 0 (clpm:run-cli '("add" "--path" "../dep" "dep"))))
           ;; Ensure lockfile changed but activation did not (yet).
           (let* ((env (clpm.io.sexp:read-safe-sexp-from-file env-path))
                  (env-lock (and (consp env) (eq (car env) :env)
                                 (getf (cdr env) :lockfile-sha256)))
                  (cur-lock (clpm.crypto.sha256:bytes-to-hex
                             (clpm.crypto.sha256:sha256-file lock-path))))
             (assert-true (and (stringp env-lock) (stringp cur-lock))
                          "Expected :lockfile-sha256 in env.sexp (pre-refresh)")
             (assert-true (not (string= env-lock cur-lock))
                          "Expected env to be stale after `clpm add` without install"))
           (uiop:with-current-directory (project-root)
             (assert-eql 17 (clpm:run-cli '("run"))))
           (assert-true (uiop:file-exists-p dep-loaded-path)
                        "Expected dep-loaded.txt to be created after activation refresh")
           (let* ((env (clpm.io.sexp:read-safe-sexp-from-file env-path))
                  (env-lock (and (consp env) (eq (car env) :env)
                                 (getf (cdr env) :lockfile-sha256)))
                  (cur-lock (clpm.crypto.sha256:bytes-to-hex
                             (clpm.crypto.sha256:sha256-file lock-path))))
             (assert-true (string= env-lock cur-lock)
                          "Expected activation env to be refreshed to match clpm.lock"))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "~%Run/exec tests PASSED!~%")
(sb-ext:exit :code 0)
