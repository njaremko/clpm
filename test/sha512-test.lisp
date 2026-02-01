;;;; test/sha512-test.lisp - SHA-512 tests

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

(format t "Testing SHA-512 vectors...~%")

(let ((h-empty (clpm.crypto.sha256:bytes-to-hex
                (clpm.crypto.sha512:sha512 "")))
      (h-abc (clpm.crypto.sha256:bytes-to-hex
              (clpm.crypto.sha512:sha512 "abc"))))
  (assert-string=
   "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"
   h-empty)
  (assert-string=
   "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"
   h-abc))

(format t "  SHA-512 vectors PASSED~%")
(format t "~%SHA-512 tests PASSED!~%")
(sb-ext:exit :code 0)

