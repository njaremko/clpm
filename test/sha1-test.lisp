;;;; test/sha1-test.lisp - SHA-1 tests

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

(defun assert-string= (expected actual)
  (unless (string= expected actual)
    (fail "Assertion failed: expected ~A, got ~A" expected actual)))

(format t "Testing SHA-1 vectors...~%")

(let ((h-empty (clpm.crypto.sha256:bytes-to-hex
                (clpm.crypto.sha1:sha1 "")))
      (h-abc (clpm.crypto.sha256:bytes-to-hex
              (clpm.crypto.sha1:sha1 "abc"))))
  (assert-string= "da39a3ee5e6b4b0d3255bfef95601890afd80709" h-empty)
  (assert-string= "a9993e364706816aba3e25717850c26c9cd0d89d" h-abc))

(format t "  SHA-1 vectors PASSED~%")
(format t "~%SHA-1 tests PASSED!~%")
(sb-ext:exit :code 0)

