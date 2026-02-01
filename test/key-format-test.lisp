;;;; test/key-format-test.lisp - Registry key/signature format parsing tests

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

(defun write-bytes (path bytes)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (write-sequence bytes s)))

(defun write-text (path text)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (write-string text s)))

(format t "Testing key/signature encodings...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((dir (merge-pathnames "fmt/" tmp))
         (key-raw (make-array 32 :element-type '(unsigned-byte 8)))
         (sig-raw (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0))
         (key-raw-path (merge-pathnames "key.raw" dir))
         (key-hex-path (merge-pathnames "key.pub" dir))
         (sig-hex-path (merge-pathnames "sig.hex" dir))
         (sig-b64-path (merge-pathnames "sig.b64" dir)))
    (dotimes (i 32)
      (setf (aref key-raw i) i))

    ;; Public key: raw 32 bytes
    (write-bytes key-raw-path key-raw)
    (let ((k (clpm.crypto.ed25519:load-public-key key-raw-path)))
      (assert-true k "Failed to load raw public key")
      (assert-true (equalp (clpm.crypto.ed25519::public-key-bytes k) key-raw)
                   "Raw key bytes mismatch"))

    ;; Public key: ascii hex (64 chars + newline)
    (write-text key-hex-path
                (format nil "~A~%"
                        (clpm.crypto.sha256:bytes-to-hex key-raw)))
    (let ((k (clpm.crypto.ed25519:load-public-key key-hex-path)))
      (assert-true k "Failed to load hex public key")
      (assert-true (equalp (clpm.crypto.ed25519::public-key-bytes k) key-raw)
                   "Hex key bytes mismatch"))

    ;; Signature: ascii hex (128 chars)
    (write-text sig-hex-path (clpm.crypto.sha256:bytes-to-hex sig-raw))
    (let ((s (clpm.crypto.ed25519::read-detached-signature sig-hex-path)))
      (assert-true (equalp (clpm.crypto.ed25519::signature-bytes s) sig-raw)
                   "Hex signature bytes mismatch"))

    ;; Signature: ascii base64 (64 zero bytes)
    (write-text sig-b64-path
                (format nil "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA==~%"))
    (let ((s (clpm.crypto.ed25519::read-detached-signature sig-b64-path)))
      (assert-true (equalp (clpm.crypto.ed25519::signature-bytes s) sig-raw)
                   "Base64 signature bytes mismatch"))))

(format t "  Key/signature encodings PASSED~%")
(format t "~%Key format tests PASSED!~%")
(sb-ext:exit :code 0)
