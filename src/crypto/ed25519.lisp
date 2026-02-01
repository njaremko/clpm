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

;;; Pure CL Ed25519 verification (RFC 8032)

(define-condition invalid-point (error)
  ((message :initarg :message :reader invalid-point-message))
  (:report (lambda (c s)
             (format s "Invalid Ed25519 point: ~A" (invalid-point-message c)))))

(defconstant +p+ (- (expt 2 255) 19))
(defconstant +d+ 37095705934669439343138083508754565189542113879843219016388785533085940283555)
(defconstant +sqrtm1+
  19681161376707505956807079304988542015446066515923890162744021073123829784752)

(defconstant +bx+ 15112221349535400772501151409588531511454012693041857206046113283949847762202)
(defconstant +by+ 46316835694926478169428394003475163141307993866256225615783033603165251855960)

(defconstant +l+
  (+ (expt 2 252) 27742317777372353535851937790883648493))

(defun f+ (a b)
  (mod (+ a b) +p+))

(defun f- (a b)
  (mod (- a b) +p+))

(defun f* (a b)
  (mod (* a b) +p+))

(defun f^ (a e)
  (let ((result 1)
        (base (mod a +p+))
        (exp e))
    (loop while (> exp 0) do
      (when (logbitp 0 exp)
        (setf result (mod (* result base) +p+)))
      (setf base (mod (* base base) +p+)
            exp (ash exp -1)))
    result))

(defun finv (a)
  (f^ a (- +p+ 2)))

(defun fsqrt (u v)
  "Compute sqrt(u/v) in GF(p), or signal INVALID-POINT."
  (let* ((uv (f* u (finv v)))
         (x (f^ uv (/ (+ +p+ 3) 8))))
    (unless (= (f* x x) uv)
      (setf x (f* x +sqrtm1+)))
    (unless (= (f* x x) uv)
      (error 'invalid-point :message "no square root"))
    x))

(defstruct point
  "Extended Edwards coordinates (X, Y, Z, T)."
  (x 0 :type integer)
  (y 1 :type integer)
  (z 1 :type integer)
  (t 0 :type integer))

(defun point-id ()
  (make-point :x 0 :y 1 :z 1 :t 0))

(defun basepoint ()
  (make-point :x +bx+
              :y +by+
              :z 1
              :t (f* +bx+ +by+)))

(defun point-add (p q)
  (let* ((x1 (point-x p)) (y1 (point-y p)) (z1 (point-z p)) (t1 (point-t p))
         (x2 (point-x q)) (y2 (point-y q)) (z2 (point-z q)) (t2 (point-t q))
         (a (f* (f- y1 x1) (f- y2 x2)))
         (b (f* (f+ y1 x1) (f+ y2 x2)))
         (c (f* 2 (f* +d+ (f* t1 t2))))
         (d (f* 2 (f* z1 z2)))
         (e (f- b a))
         (f (f- d c))
         (g (f+ d c))
         (h (f+ b a)))
    (make-point :x (f* e f)
                :y (f* g h)
                :z (f* f g)
                :t (f* e h))))

(defun point-double (p)
  (let* ((x1 (point-x p)) (y1 (point-y p)) (z1 (point-z p)) (t1 (point-t p))
         (a (f* x1 x1))
         (b (f* y1 y1))
         (c (f* 2 (f* z1 z1)))
         (d (f- 0 a))
         (e (f- (f- (f* (f+ x1 y1) (f+ x1 y1)) a) b))
         (g (f+ d b))
         (f (f- g c))
         (h (f- d b)))
    (declare (ignore t1))
    (make-point :x (f* e f)
                :y (f* g h)
                :z (f* f g)
                :t (f* e h))))

(defun scalar-mult (k p)
  "Left-to-right double-and-add scalar multiplication over 256 bits."
  (let ((q (point-id)))
    (loop for i from 255 downto 0 do
      (setf q (point-double q))
      (when (logbitp i k)
        (setf q (point-add q p))))
    q))

(defun le-bytes-to-int (bytes &key (start 0) (end (length bytes)))
  (let ((acc 0))
    (loop for i from (1- end) downto start do
      (setf acc (+ (ash acc 8) (aref bytes i))))
    acc))

(defun int-to-le-bytes (n len)
  (let ((out (make-array len :element-type '(unsigned-byte 8))))
    (dotimes (i len out)
      (setf (aref out i) (ldb (byte 8 (* 8 i)) n)))))

(defun decode-point (enc)
  "Decode a 32-byte compressed Edwards25519 point."
  (unless (= (length enc) 32)
    (error 'invalid-point :message "encoding must be 32 bytes"))
  (let* ((bytes (copy-seq enc))
         (sign (logand (aref bytes 31) #x80)))
    (setf (aref bytes 31) (logand (aref bytes 31) #x7f))
    (let ((y (le-bytes-to-int bytes)))
      (when (>= y +p+)
        (error 'invalid-point :message "y out of range"))
      (let* ((y2 (f* y y))
             (u (f- y2 1))
             (v (f+ (f* +d+ y2) 1))
             (x (fsqrt u v)))
        (when (and (zerop x) (not (zerop sign)))
          (error 'invalid-point :message "invalid x sign"))
        (when (not (eql (logand x 1) (ash sign -7)))
          (setf x (f- +p+ x)))
        (make-point :x x :y y :z 1 :t (f* x y))))))

(defun encode-point (p)
  "Encode point P to 32-byte compressed form."
  (let* ((zinv (finv (point-z p)))
         (x (f* (point-x p) zinv))
         (y (f* (point-y p) zinv))
         (out (int-to-le-bytes y 32)))
    (when (oddp x)
      (setf (aref out 31) (logior (aref out 31) #x80)))
    out))

(defun point= (a b)
  "Check equality of two points without inversion."
  (and (= (f* (point-x a) (point-z b))
          (f* (point-x b) (point-z a)))
       (= (f* (point-y a) (point-z b))
          (f* (point-y b) (point-z a)))))

(defun reduce-scalar (n)
  (mod n +l+))

(defun parse-scalar (bytes)
  (le-bytes-to-int bytes))

(defun verify-signature (message signature public-key)
  "Verify an Ed25519 signature (RFC 8032).

Returns T if signature is valid, NIL otherwise."
  (handler-case
      (let* ((msg-bytes (etypecase message
                          (string (map '(vector (unsigned-byte 8)) #'char-code message))
                          ((array (unsigned-byte 8) (*)) message)))
             (sig-bytes (etypecase signature
                          (signature (signature-bytes signature))
                          ((array (unsigned-byte 8) (*)) signature)))
             (key-bytes (etypecase public-key
                          (public-key (public-key-bytes public-key))
                          ((array (unsigned-byte 8) (*)) public-key))))
        (unless (= (length key-bytes) 32)
          (return-from verify-signature nil))
        (unless (= (length sig-bytes) 64)
          (return-from verify-signature nil))
        (let* ((r-bytes (subseq sig-bytes 0 32))
               (s-bytes (subseq sig-bytes 32 64))
               (s (parse-scalar s-bytes)))
          (when (>= s +l+)
            (return-from verify-signature nil))
          (let* ((a (decode-point key-bytes))
                 (r (decode-point r-bytes))
                 (h-input (make-array (+ 64 (length msg-bytes))
                                      :element-type '(unsigned-byte 8))))
            (replace h-input r-bytes :start1 0)
            (replace h-input key-bytes :start1 32)
            (replace h-input msg-bytes :start1 64)
            (let* ((digest (clpm.crypto.sha512:sha512 h-input))
                   (h (reduce-scalar (le-bytes-to-int digest)))
                   (sb (scalar-mult s (basepoint)))
                   (ha (scalar-mult h a))
                   (rhs (point-add r ha)))
              (point= sb rhs)))))
    (invalid-point ()
      nil)
    (error ()
      nil)))

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
