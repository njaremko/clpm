;;;; test/ed25519-sign-test.lisp - Ed25519 signing tests (RFC 8032 vectors)

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

(defun assert-bytes= (expected actual label)
  (assert-true (equalp expected actual)
               "Assertion failed (~A): expected ~S, got ~S"
               label
               (clpm.crypto.sha256:bytes-to-hex expected)
               (clpm.crypto.sha256:bytes-to-hex actual)))

(format t "Testing Ed25519 signing vectors...~%")

;; RFC8032 Vector 1
(let* ((seed (clpm.crypto.sha256:hex-to-bytes
              "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"))
       (expected-pub (clpm.crypto.sha256:hex-to-bytes
                      "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"))
       (expected-sig (clpm.crypto.sha256:hex-to-bytes
                      "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e065224901555fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"))
       (pub (clpm.crypto.ed25519:derive-public-key-from-seed seed))
       (sig (clpm.crypto.ed25519:sign "" seed)))
  (assert-bytes= expected-pub pub "public key")
  (assert-bytes= expected-sig sig "signature")
  (assert-true (clpm.crypto.ed25519:verify-signature "" sig pub)
               "Signature did not verify against derived public key"))

(format t "  Signing vectors PASSED~%")
(format t "~%Ed25519 sign tests PASSED!~%")
(sb-ext:exit :code 0)

