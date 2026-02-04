;;;; test/resolve-short-circuit-test.lisp - Resolve caching / short-circuit tests

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

(defun run-cli-captured (args)
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((*standard-output* out)
          (*error-output* err))
      (let ((code (clpm:run-cli args)))
        (values code
                (get-output-stream-string out)
                (get-output-stream-string err))))))

(format t "Testing resolve short-circuit...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (project-root (merge-pathnames "proj/" tmp))
         (manifest-path (merge-pathnames "clpm.project" project-root))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist project-root)
    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)
           ;; Minimal project with no deps; resolve should be fast and deterministic.
           (let ((project (clpm.project:make-project
                          :name "proj"
                          :version "0.1.0"
                          :systems '("proj")
                          :depends '()
                          :dev-depends '()
                          :test-depends '()
                          :registries '()
                          :sbcl-constraints '()
                          :build-options '()
                          :scripts '())))
             (clpm.project:write-project-file project manifest-path))

           (uiop:with-current-directory (project-root)
             ;; First resolve writes lockfile.
             (multiple-value-bind (code out err)
                 (run-cli-captured '("resolve"))
               (declare (ignore err))
               (assert-eql 0 code)
               (assert-true (search "Resolving dependencies" out :test #'char-equal)
                            "Expected resolve to run, got stdout:~%~A"
                            out))

             ;; Second resolve should short-circuit.
             (multiple-value-bind (code out err)
                 (run-cli-captured '("resolve"))
               (declare (ignore err))
               (assert-eql 0 code)
               (assert-true (search "skipping resolve" out :test #'char-equal)
                            "Expected resolve to be skipped, got stdout:~%~A"
                            out))

             ;; Modify manifest (version change) and ensure resolve runs again.
             (let ((project (clpm.project:read-project-file manifest-path)))
               (setf (clpm.project:project-version project) "0.1.1")
               (clpm.project:write-project-file project manifest-path))
             (multiple-value-bind (code out err)
                 (run-cli-captured '("resolve"))
               (declare (ignore err))
               (assert-eql 0 code)
               (assert-true (search "Resolving dependencies" out :test #'char-equal)
                            "Expected resolve to run after manifest change, got stdout:~%~A"
                            out)
               (assert-true (not (search "skipping resolve" out :test #'char-equal))
                            "Did not expect skip message after change, got stdout:~%~A"
                            out)))))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME"))))

(format t "Resolve short-circuit tests PASSED!~%")
(sb-ext:exit :code 0)
