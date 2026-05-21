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

(defun run-cli-captured (args)
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((*standard-output* out)
          (*error-output* err))
      (let ((code (clpm:run-cli args)))
        (values code
                (get-output-stream-string out)
                (get-output-stream-string err))))))

(format t "Testing registry CLI commands...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)
           (multiple-value-bind (code stdout stderr)
               (run-cli-captured '("registry" "add"
                                   "--name" "first"
                                   "--name" "second"
                                   "--url" "https://example.invalid/main.git"
                                   "--trust" "ed25519:test"))
             (declare (ignore stdout))
             (assert-true (= code 1)
                          "Expected duplicate registry add --name to fail")
             (assert-true (search "Duplicate option: --name" stderr
                                  :test #'char-equal)
                          "Expected duplicate --name error, got:~%~A" stderr)
             (let ((cfg (clpm.config:read-config)))
               (assert-true (null (clpm.config:config-registries cfg))
                            "Duplicate registry name should not mutate config")))
           (multiple-value-bind (code stdout stderr)
               (run-cli-captured '("registry" "add"
                                   "--name" "bad"
                                   "--url" "https://example.invalid/bad.git"
                                   "--trust" "none"))
             (declare (ignore stdout))
             (assert-true (= code 1)
                          "Expected registry add --trust none to fail")
             (assert-true (search "Invalid trust" stderr :test #'char-equal)
                          "Expected invalid trust error, got:~%~A" stderr)
             (let ((cfg (clpm.config:read-config)))
               (assert-true (null (clpm.config:config-registries cfg))
                            "Invalid registry trust should not mutate config")))
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
           (multiple-value-bind (code stdout stderr)
               (run-cli-captured '("registry" "list" "extra"))
             (declare (ignore stdout))
             (assert-true (= code 1)
                          "Expected registry list extra arg to fail")
             (assert-true (search "Usage: clpm registry list" stderr
                                  :test #'char-equal)
                          "Expected registry list arity error, got:~%~A"
                          stderr))
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
