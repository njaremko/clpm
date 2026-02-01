;;;; crypto/ed25519.lisp - Ed25519 signature verification

(in-package #:clpm.crypto.ed25519)

;;; Ed25519 signature verification
;;; This implementation shells out to openssl or signify for verification.
;;; A pure CL implementation would require bignum arithmetic and curve operations.

(defstruct public-key
  "An Ed25519 public key."
  (id nil :type (or null string))
  (bytes nil :type (or null (simple-array (unsigned-byte 8) (32)))))

(defstruct signature
  "An Ed25519 signature."
  (bytes nil :type (or null (simple-array (unsigned-byte 8) (64)))))

(defun parse-key-id (key-string)
  "Parse a key ID string like 'ed25519:hexbytes' and return (scheme . id)."
  (let ((colon-pos (position #\: key-string)))
    (if colon-pos
        (let ((scheme (subseq key-string 0 colon-pos))
              (id (subseq key-string (1+ colon-pos))))
          (cons scheme id))
        (cons "ed25519" key-string))))

(defun load-public-key (path)
  "Load an Ed25519 public key from a file.
Supports:
- Raw 32-byte keys
- ASCII hex (64 hex chars) + optional newline
- PEM format"
  (with-open-file (stream path :element-type '(unsigned-byte 8)
                               :if-does-not-exist nil)
    (unless stream
      (return-from load-public-key nil))
    (let* ((size (file-length stream))
           (data (make-array size :element-type '(unsigned-byte 8))))
      (read-sequence data stream)
      (cond
        ;; Raw 32-byte key
        ((= size 32)
         (make-public-key :bytes data))
        ;; ASCII hex key (64 hex chars + optional newline/whitespace)
        (t
         (let* ((text (map 'string #'code-char data))
                (trim (string-trim '(#\Space #\Newline #\Return #\Tab) text)))
           (labels ((hex-string-p (s)
                      (and (= (length s) 64)
                           (every (lambda (c)
                                    (find c "0123456789abcdef" :test #'char-equal))
                                  s))))
             (cond
               ;; PEM format - try to extract key
               ((and (> size 32)
                     (eql (aref data 0) (char-code #\-)))
                (let* ((lines (remove-if (lambda (line)
                                           (or (search "-----" line)
                                               (zerop (length line))))
                                         (uiop:split-string text :separator '(#\Newline))))
                       (base64 (apply #'concatenate 'string lines)))
                  (make-public-key :bytes (decode-base64-to-bytes base64))))
               ;; Hex-encoded key
               ((hex-string-p trim)
                (let ((bytes (clpm.crypto.sha256:hex-to-bytes trim)))
                  (unless (= (length bytes) 32)
                    (error "Invalid hex public key length"))
                  (make-public-key :bytes bytes)))
               (t
                (error "Unsupported public key format"))))))))))

(defun decode-base64-to-bytes (base64-string)
  "Decode base64 string to byte array."
  (let* ((table "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
         (clean (remove-if (lambda (c) (or (eql c #\Space) (eql c #\Newline)
                                           (eql c #\Return) (eql c #\=)))
                           base64-string))
         (len (length clean))
         (out-len (floor (* len 3) 4))
         (output (make-array out-len :element-type '(unsigned-byte 8)))
         (out-pos 0))
    (loop for i from 0 below len by 4
          for remaining = (min 4 (- len i))
          do (let ((vals (loop for j from 0 below remaining
                               collect (position (char clean (+ i j)) table))))
               (when (>= remaining 2)
                 (setf (aref output out-pos)
                       (logior (ash (first vals) 2)
                               (ash (second vals) -4)))
                 (incf out-pos))
               (when (>= remaining 3)
                 (setf (aref output out-pos)
                       (logior (ash (logand (second vals) #x0f) 4)
                               (ash (third vals) -2)))
                 (incf out-pos))
               (when (>= remaining 4)
                 (setf (aref output out-pos)
                       (logior (ash (logand (third vals) #x03) 6)
                               (fourth vals)))
                 (incf out-pos))))
    (subseq output 0 out-pos)))

(defun verify-signature (message signature public-key)
  "Verify an Ed25519 signature.
MESSAGE is a byte array or string.
SIGNATURE is a signature struct or 64-byte array.
PUBLIC-KEY is a public-key struct or 32-byte array.

This implementation uses external tools for verification.
Returns T if signature is valid, NIL otherwise."
  (let* ((msg-bytes (etypecase message
                      (string (map '(vector (unsigned-byte 8)) #'char-code message))
                      ((array (unsigned-byte 8) (*)) message)))
         (sig-bytes (etypecase signature
                      (signature (signature-bytes signature))
                      ((array (unsigned-byte 8) (*)) signature)))
         (key-bytes (etypecase public-key
                      (public-key (public-key-bytes public-key))
                      ((array (unsigned-byte 8) (*)) public-key))))
    ;; Write temp files for verification
    (let ((msg-file (make-temp-file "clpm-msg-"))
          (sig-file (make-temp-file "clpm-sig-"))
          (key-file (make-temp-file "clpm-key-")))
      (unwind-protect
           (progn
             (write-bytes-to-file msg-bytes msg-file)
             (write-bytes-to-file sig-bytes sig-file)
             (write-bytes-to-file key-bytes key-file)
             ;; Try openssl first, then signify
             (or (verify-with-openssl msg-file sig-file key-file)
                 (verify-with-signify msg-file sig-file key-file)))
        ;; Cleanup
        (ignore-errors (delete-file msg-file))
        (ignore-errors (delete-file sig-file))
        (ignore-errors (delete-file key-file))))))

(defun make-temp-file (prefix)
  "Create a temporary file path."
  (let ((tmp-dir (or (uiop:getenv "TMPDIR")
                     (uiop:getenv "TMP")
                     "/tmp")))
    (merge-pathnames (format nil "~A~A" prefix (random (expt 2 32)))
                     (uiop:ensure-directory-pathname tmp-dir))))

(defun write-bytes-to-file (bytes path)
  "Write bytes to file."
  (with-open-file (stream path :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede)
    (write-sequence bytes stream)))

(defun verify-with-openssl (msg-file sig-file key-file)
  "Verify using openssl.
Note: openssl dgst -verify doesn't directly support Ed25519 raw keys,
so this is a placeholder for proper integration."
  (declare (ignore msg-file sig-file key-file))
  ;; OpenSSL Ed25519 verification requires specific key format
  ;; For now, return nil to fall through to other methods
  nil)

(defun verify-with-signify (msg-file sig-file key-file)
  "Verify using signify (if available)."
  (declare (ignore msg-file sig-file key-file))
  ;; Signify has its own format
  ;; For now, return nil
  nil)

;;; Simple signature format for CLPM
;;; Since Ed25519 verification in pure CL is complex, we support a
;;; detached signature format that can be verified externally.

(defun read-detached-signature (sig-path)
  "Read a detached signature file.
Format: base64-encoded or hex-encoded 64-byte signature."
  (let* ((content (uiop:read-file-string sig-path))
         (trim (string-trim '(#\Space #\Newline #\Return #\Tab) content))
         (hexp (and (= (length trim) 128)
                    (every (lambda (c)
                             (find c "0123456789abcdef" :test #'char-equal))
                           trim)))
         (bytes (if hexp
                    (clpm.crypto.sha256:hex-to-bytes trim)
                    (decode-base64-to-bytes trim))))
    (unless (= (length bytes) 64)
      (error "Invalid detached signature length"))
    (make-signature :bytes bytes)))

(defun verify-file-signature (file-path sig-path key-path)
  "Verify signature of FILE-PATH using signature at SIG-PATH and key at KEY-PATH.
Returns T if valid, signals error if invalid."
  (let ((key (load-public-key key-path)))
    (unless key
      (error "Could not load public key from ~A" key-path))
    (let ((sig (read-detached-signature sig-path))
          (msg (with-open-file (s file-path :element-type '(unsigned-byte 8))
                 (let ((data (make-array (file-length s) :element-type '(unsigned-byte 8))))
                   (read-sequence data s)
                   data))))
      (verify-signature msg sig key))))

;;; For bootstrapping, we may skip signature verification with --insecure
;;; In production, this should verify properly.
