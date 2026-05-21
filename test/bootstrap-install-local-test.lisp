;;;; test/bootstrap-install-local-test.lisp - Bootstrap installer tests

(require :asdf)
(require :sb-posix)

(let* ((this-file (or *load-truename* *load-pathname*))
       (test-dir (uiop:pathname-directory-pathname this-file))
       (repo-root (uiop:pathname-parent-directory-pathname test-dir)))
  (push repo-root asdf:*central-registry*))

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

(defun assert-true (value fmt &rest args)
  (unless value
    (apply #'fail fmt args)))

(defun assert-contains (haystack needle)
  (assert-true (and (stringp haystack)
                    (search needle haystack :test #'char-equal))
               "Expected output to contain ~S, got:~%~A"
               needle
               haystack))

(defun run-captured (program arguments &key directory)
  (let ((out (make-string-output-stream)))
    (let ((process (sb-ext:run-program program arguments
                                       :search t
                                       :directory directory
                                       :output out
                                       :error out
                                       :wait t)))
      (values (get-output-stream-string out)
              (sb-ext:process-exit-code process)))))

(defun restore-env (name old-value)
  (if old-value
      (sb-posix:setenv name old-value 1)
      (sb-posix:unsetenv name)))

(format t "Testing install-local bootstrap completion...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((this-file (or *load-truename* *load-pathname*))
         (test-dir (uiop:pathname-directory-pathname this-file))
         (repo-root (uiop:pathname-parent-directory-pathname test-dir))
         (home (merge-pathnames "home/" tmp))
         (prefix (merge-pathnames "prefix/" tmp))
         (clpm-home (merge-pathnames "clpm-home/" tmp))
         (binary (merge-pathnames "bin/clpm" prefix))
         (binary-image (merge-pathnames "bin/clpm.bin" prefix))
         (data-dir (merge-pathnames ".local/share/clpm/" home))
         (old-home (sb-posix:getenv "HOME"))
         (old-clpm-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist home)
    (ensure-directories-exist clpm-home)
    (unwind-protect
         (progn
           (sb-posix:setenv "HOME" (namestring home) 1)
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)
           (multiple-value-bind (output exit-code)
               (run-captured "sbcl"
                             (list "--script"
                                   (namestring (merge-pathnames "clpm-bootstrap.lisp"
                                                                repo-root))
                                   "install-local"
                                   (namestring repo-root)
                                   "--prefix"
                                   (namestring prefix))
                             :directory repo-root)
             (assert-eql 0 exit-code)
             (assert-contains output "CLPM installed successfully!")
             (assert-contains output "Binary:")
             (assert-true (uiop:file-exists-p binary)
                          "Expected installed wrapper at ~A"
                          binary)
             (assert-true (uiop:file-exists-p binary-image)
                          "Expected installed image at ~A"
                          binary-image)
             (assert-true (uiop:directory-exists-p data-dir)
                          "Expected bootstrap data directory at ~A"
                          data-dir))
           (multiple-value-bind (version-output version-code)
               (run-captured (namestring binary) '("--version"))
             (assert-eql 0 version-code)
             (assert-contains version-output "clpm 0.1.0")))
      (restore-env "HOME" old-home)
      (restore-env "CLPM_HOME" old-clpm-home))))

(format t "  install-local bootstrap completion PASSED~%")

(format t "~%All bootstrap install-local tests PASSED!~%")
(sb-ext:exit :code 0)
