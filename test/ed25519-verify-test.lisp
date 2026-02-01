;;;; test/ed25519-verify-test.lisp - Ed25519 verification tests (RFC 8032 vectors)

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

(defun assert-true (x fmt &rest args)
  (unless x
    (apply #'fail fmt args)))

(format t "Testing Ed25519 verification vectors...~%")

;; Vector 1
(let* ((a (clpm.crypto.sha256:hex-to-bytes
           "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"))
       (sig (clpm.crypto.sha256:hex-to-bytes
             "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b")))
  (assert-true (clpm.crypto.ed25519:verify-signature "" sig a)
               "RFC8032 vector 1 failed")

  ;; Altered signature must fail.
  (let ((bad (copy-seq sig)))
    (setf (aref bad 0) (logxor (aref bad 0) 1))
    (assert-true (not (clpm.crypto.ed25519:verify-signature "" bad a))
                 "Altered signature unexpectedly verified"))

  ;; Non-canonical S (S = L) must fail.
  (let* ((bad (copy-seq sig))
         (s-bytes (clpm.crypto.ed25519::int-to-le-bytes clpm.crypto.ed25519::+l+ 32)))
    (replace bad s-bytes :start1 32)
    (assert-true (not (clpm.crypto.ed25519:verify-signature "" bad a))
                 "Non-canonical S unexpectedly verified")))

;; Vector 2
(let* ((a (clpm.crypto.sha256:hex-to-bytes
           "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c"))
       (m (clpm.crypto.sha256:hex-to-bytes "72"))
       (sig (clpm.crypto.sha256:hex-to-bytes
             "92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00")))
  (assert-true (clpm.crypto.ed25519:verify-signature m sig a)
               "RFC8032 vector 2 failed"))

(format t "  Verification vectors PASSED~%")
(format t "~%Ed25519 verify tests PASSED!~%")
(sb-ext:exit :code 0)

