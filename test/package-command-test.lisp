;;;; test/package-command-test.lisp - `clpm package` command tests

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

(format t "Testing `clpm package`...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (project-root (merge-pathnames "app/" tmp))
         (src-dir (merge-pathnames "src/" project-root))
         (dist-bin (merge-pathnames "dist/app" project-root))
         (meta-path (merge-pathnames "dist/app.meta.sxp" project-root))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist project-root)
    (ensure-directories-exist src-dir)

    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)

           ;; Minimal ASDF system.
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
              (format s "  (format t \"Hello, ~~A!~~%\" (or (first args) \"world\"))~%")
              (format s "  0)~%")))

           ;; Project manifest with :package entry.
           (clpm.io.sexp:write-canonical-sexp-to-file
            '(:project
              :name "app"
              :version "0.1.0"
              :systems ("app")
              :run (:system "app" :function "app::main")
              :test (:systems ("app/test"))
              :package (:output "dist/app" :system "app" :function "app::main")
              :depends ()
              :dev-depends ()
              :test-depends ()
              :registries ()
              :scripts ())
            (merge-pathnames "clpm.project" project-root))

           (uiop:with-current-directory (project-root)
             (assert-eql 0 (clpm:run-cli '("package"))))

           (assert-true (uiop:file-exists-p dist-bin)
                        "Missing packaged binary: ~A" (namestring dist-bin))
           (assert-true (uiop:file-exists-p meta-path)
                        "Missing package metadata: ~A" (namestring meta-path))

           ;; Verify binary runs.
           (multiple-value-bind (output error-output exit-code)
               (clpm.platform:run-program (list (namestring dist-bin) "codex")
                                          :output :string
                                          :error-output :string
                                          :timeout 60)
             (declare (ignore error-output))
             (assert-true (zerop exit-code)
                          "Packaged binary exited non-zero: ~D" exit-code)
             (assert-true (search "Hello, codex!" output)
                          "Unexpected binary output: ~S" output))

           ;; Verify SBCL runtime options aren't consuming app args.
           ;; Without a wrapper, `--version` would print SBCL version and exit.
           (multiple-value-bind (output error-output exit-code)
               (clpm.platform:run-program (list (namestring dist-bin) "--version")
                                          :output :string
                                          :error-output :string
                                          :timeout 60)
             (declare (ignore error-output))
             (assert-true (zerop exit-code)
                          "Packaged binary exited non-zero: ~D" exit-code)
             (assert-true (search "Hello, --version!" output)
                          "Expected wrapper to pass through args, got: ~S" output))

	           ;; Verify metadata includes lock hash.
	           (let* ((meta (clpm.io.sexp:read-safe-sexp-from-file meta-path))
	                  (lock (getf (cdr meta) :lock-sha256)))
	             (assert-true (and (stringp lock) (= (length lock) 64))
	                          "Expected :lock-sha256 64-char hex, got ~S" lock))

	           ;; Non-SBCL packaging is explicitly rejected.
	           (uiop:with-current-directory (project-root)
	             (let ((*error-output* (make-string-output-stream)))
	               (assert-eql 1 (clpm:run-cli '("--lisp" "ccl" "package")))
	               (let ((err (get-output-stream-string *error-output*)))
	                 (assert-true (search "Packaging currently supports SBCL only" err)
	                              "Expected SBCL-only message, got: ~S" err))))))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME"))))

(format t "~%Package command tests PASSED!~%")
(sb-ext:exit :code 0)
