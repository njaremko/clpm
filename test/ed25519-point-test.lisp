;;;; test/ed25519-point-test.lisp - Ed25519 point decoding tests

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

(format t "Testing Ed25519 point decoding...~%")

;; Basepoint encoded key (compressed), RFC 8032 compatible.
(let* ((b-enc (clpm.crypto.sha256:hex-to-bytes
               "5866666666666666666666666666666666666666666666666666666666666666"))
       (p (clpm.crypto.ed25519::decode-point b-enc)))
  (assert-true (= (clpm.crypto.ed25519::point-x p) clpm.crypto.ed25519::+bx+)
               "Basepoint X mismatch")
  (assert-true (= (clpm.crypto.ed25519::point-y p) clpm.crypto.ed25519::+by+)
               "Basepoint Y mismatch")
  (assert-true (equalp (clpm.crypto.ed25519::encode-point p) b-enc)
               "Basepoint encode/decode mismatch"))

;; RFC 8032 test vector 1 public key
(let* ((a-enc (clpm.crypto.sha256:hex-to-bytes
               "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"))
       (a (clpm.crypto.ed25519::decode-point a-enc)))
  (assert-true (equalp (clpm.crypto.ed25519::encode-point a) a-enc)
               "RFC8032 A encode/decode mismatch"))

(format t "  Point decoding PASSED~%")
(format t "~%Ed25519 point tests PASSED!~%")
(sb-ext:exit :code 0)

