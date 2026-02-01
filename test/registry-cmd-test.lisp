;;;; test/registry-cmd-test.lisp - Registry CLI command tests

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

(format t "Testing registry CLI commands...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)
           (let ((args '("registry" "add"
                         "--name" "main"
                         "--url" "https://example.invalid/registry.git"
                         "--trust" "ed25519:test")))
             (assert-true (zerop (clpm:run-cli args))
                          "Expected registry add to succeed")
             (let* ((path (merge-pathnames "config.sxp" (clpm.platform:config-dir)))
                    (first (uiop:read-file-string path)))
               ;; Second run should be idempotent.
               (assert-true (zerop (clpm:run-cli args))
                            "Expected registry add to be idempotent")
               (let ((second (uiop:read-file-string path)))
                 (assert-true (string= first second)
                              "Expected config file to be unchanged on repeated add"))))
           ;; List should succeed even with no project.
           (assert-true (zerop (clpm:run-cli '("registry" "list")))
                        "Expected registry list to succeed")
           (let* ((cfg (clpm.config:read-config))
                  (regs (clpm.config:config-registries cfg)))
             (assert-true (= (length regs) 1)
                          "Expected one configured registry")
             (assert-true (string= (clpm.project:registry-ref-name (first regs)) "main")
                          "Expected configured registry to be named main")))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "  Registry command tests PASSED~%")
(format t "~%Registry CLI tests PASSED!~%")
(sb-ext:exit :code 0)

