;;;; crypto/sha512.lisp - SHA-512 implementation

(in-package #:clpm.crypto.sha512)

(defconstant +mask64+ #xffffffffffffffff)

(defun u64 (x)
  (logand x +mask64+))

(defun u64+ (&rest xs)
  (u64 (reduce #'+ xs)))

(defun rotr64 (x n)
  (let ((x (u64 x)))
    (u64 (logior (ash x (- n))
                 (ash x (- 64 n))))))

(defun shr64 (x n)
  (ash (u64 x) (- n)))

(defun ch (x y z)
  (logxor (logand x y)
          (logand (lognot x) z)))

(defun maj (x y z)
  (logxor (logand x y)
          (logand x z)
          (logand y z)))

(defun big-sigma0 (x)
  (logxor (rotr64 x 28)
          (rotr64 x 34)
          (rotr64 x 39)))

(defun big-sigma1 (x)
  (logxor (rotr64 x 14)
          (rotr64 x 18)
          (rotr64 x 41)))

(defun small-sigma0 (x)
  (logxor (rotr64 x 1)
          (rotr64 x 8)
          (shr64 x 7)))

(defun small-sigma1 (x)
  (logxor (rotr64 x 19)
          (rotr64 x 61)
          (shr64 x 6)))

(defun load-u64-be (octets offset)
  (let ((v 0))
    (dotimes (i 8 v)
      (setf v (logior (ash v 8) (aref octets (+ offset i)))))))

(defun store-u64-be (word octets offset)
  (dotimes (i 8)
    (setf (aref octets (+ offset i))
          (ldb (byte 8 (* 8 (- 7 i))) word))))

(defparameter +k+
  ;; FIPS 180-4 SHA-512 round constants
  #(#x428a2f98d728ae22 #x7137449123ef65cd #xb5c0fbcfec4d3b2f #xe9b5dba58189dbbc
    #x3956c25bf348b538 #x59f111f1b605d019 #x923f82a4af194f9b #xab1c5ed5da6d8118
    #xd807aa98a3030242 #x12835b0145706fbe #x243185be4ee4b28c #x550c7dc3d5ffb4e2
    #x72be5d74f27b896f #x80deb1fe3b1696b1 #x9bdc06a725c71235 #xc19bf174cf692694
    #xe49b69c19ef14ad2 #xefbe4786384f25e3 #x0fc19dc68b8cd5b5 #x240ca1cc77ac9c65
    #x2de92c6f592b0275 #x4a7484aa6ea6e483 #x5cb0a9dcbd41fbd4 #x76f988da831153b5
    #x983e5152ee66dfab #xa831c66d2db43210 #xb00327c898fb213f #xbf597fc7beef0ee4
    #xc6e00bf33da88fc2 #xd5a79147930aa725 #x06ca6351e003826f #x142929670a0e6e70
    #x27b70a8546d22ffc #x2e1b21385c26c926 #x4d2c6dfc5ac42aed #x53380d139d95b3df
    #x650a73548baf63de #x766a0abb3c77b2a8 #x81c2c92e47edaee6 #x92722c851482353b
    #xa2bfe8a14cf10364 #xa81a664bbc423001 #xc24b8b70d0f89791 #xc76c51a30654be30
    #xd192e819d6ef5218 #xd69906245565a910 #xf40e35855771202a #x106aa07032bbd1b8
    #x19a4c116b8d2d0c8 #x1e376c085141ab53 #x2748774cdf8eeb99 #x34b0bcb5e19b48a8
    #x391c0cb3c5c95a63 #x4ed8aa4ae3418acb #x5b9cca4f7763e373 #x682e6ff3d6b2b8a3
    #x748f82ee5defb2fc #x78a5636f43172f60 #x84c87814a1f0ab72 #x8cc702081a6439ec
    #x90befffa23631e28 #xa4506cebde82bde9 #xbef9a3f7b2c67915 #xc67178f2e372532b
    #xca273eceea26619c #xd186b8c721c0c207 #xeada7dd6cde0eb1e #xf57d4f7fee6ed178
    #x06f067aa72176fba #x0a637dc5a2c898a6 #x113f9804bef90dae #x1b710b35131c471b
    #x28db77f523047d84 #x32caab7b40c72493 #x3c9ebe0a15c9bebc #x431d67c49c100d4c
    #x4cc5d4becb3e42b6 #x597f299cfc657e2a #x5fcb6fab3ad6faec #x6c44198c4a475817))

(defstruct sha512-ctx
  (h (make-array 8 :initial-contents
                 (list #x6a09e667f3bcc908
                       #xbb67ae8584caa73b
                       #x3c6ef372fe94f82b
                       #xa54ff53a5f1d36f1
                       #x510e527fade682d1
                       #x9b05688c2b3e6c1f
                       #x1f83d9abfb41bd6b
                       #x5be0cd19137e2179)))
  (buffer (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0))
  (buffer-len 0 :type (integer 0 128))
  (total-len 0 :type (integer 0 *))) ; bytes processed

(defun process-block (ctx block offset)
  (let* ((w (make-array 80))
         (h (sha512-ctx-h ctx)))
    ;; Message schedule
    (dotimes (i 16)
      (setf (aref w i) (load-u64-be block (+ offset (* i 8)))))
    (loop for i from 16 below 80 do
      (setf (aref w i)
            (u64+ (small-sigma1 (aref w (- i 2)))
                  (aref w (- i 7))
                  (small-sigma0 (aref w (- i 15)))
                  (aref w (- i 16)))))
    ;; Working variables
    (let ((a (aref h 0))
          (b (aref h 1))
          (c (aref h 2))
          (d (aref h 3))
          (e (aref h 4))
          (f (aref h 5))
          (g (aref h 6))
          (hh (aref h 7)))
      (dotimes (i 80)
        (let* ((t1 (u64+ hh
                         (big-sigma1 e)
                         (ch e f g)
                         (aref +k+ i)
                         (aref w i)))
               (t2 (u64+ (big-sigma0 a)
                         (maj a b c))))
          (setf hh g
                g f
                f e
                e (u64+ d t1)
                d c
                c b
                b a
                a (u64+ t1 t2))))
      ;; Add to hash state
      (setf (aref h 0) (u64+ (aref h 0) a)
            (aref h 1) (u64+ (aref h 1) b)
            (aref h 2) (u64+ (aref h 2) c)
            (aref h 3) (u64+ (aref h 3) d)
            (aref h 4) (u64+ (aref h 4) e)
            (aref h 5) (u64+ (aref h 5) f)
            (aref h 6) (u64+ (aref h 6) g)
            (aref h 7) (u64+ (aref h 7) hh)))))

(defun sha512-update (ctx octets)
  (let* ((buf (sha512-ctx-buffer ctx))
         (buf-len (sha512-ctx-buffer-len ctx))
         (len (length octets))
         (pos 0))
    (incf (sha512-ctx-total-len ctx) len)
    ;; Fill existing buffer.
    (when (plusp buf-len)
      (let ((need (- 128 buf-len)))
        (if (< len need)
            (progn
              (replace buf octets :start1 buf-len)
              (incf (sha512-ctx-buffer-len ctx) len)
              (return-from sha512-update ctx))
            (progn
              (replace buf octets :start1 buf-len :end2 need)
              (process-block ctx buf 0)
              (setf buf-len 0
                    (sha512-ctx-buffer-len ctx) 0
                    pos need)))))
    ;; Process complete 128-byte blocks directly from input.
    (loop while (<= (+ pos 128) len) do
      (process-block ctx octets pos)
      (incf pos 128))
    ;; Buffer remainder.
    (when (< pos len)
      (replace buf octets :start1 0 :start2 pos)
      (setf (sha512-ctx-buffer-len ctx) (- len pos)))
    ctx))

(defun sha512-final (ctx)
  (let* ((buf (sha512-ctx-buffer ctx))
         (buf-len (sha512-ctx-buffer-len ctx))
         (total-bytes (sha512-ctx-total-len ctx))
         (bit-length (* total-bytes 8))
         (hi (ldb (byte 64 64) bit-length))
         (lo (ldb (byte 64 0) bit-length)))
    ;; Append 0x80.
    (setf (aref buf buf-len) #x80)
    (incf buf-len)
    ;; Pad with zeros until length field (16 bytes) can be appended at offset 112.
    (when (> buf-len 112)
      (loop for i from buf-len below 128 do (setf (aref buf i) 0))
      (process-block ctx buf 0)
      (setf buf-len 0))
    (loop for i from buf-len below 112 do (setf (aref buf i) 0))
    ;; Append 128-bit big-endian length.
    (store-u64-be hi buf 112)
    (store-u64-be lo buf 120)
    (process-block ctx buf 0)
    ;; Produce digest
    (let ((digest (make-array 64 :element-type '(unsigned-byte 8)))
          (h (sha512-ctx-h ctx)))
      (dotimes (i 8 digest)
        (store-u64-be (aref h i) digest (* i 8))))))

;;; High-level interface

(defun sha512 (data)
  "Compute SHA-512 hash of DATA (string or octet vector).
Returns a 64-byte array."
  (let ((ctx (make-sha512-ctx))
        (octets (etypecase data
                  (string (map '(vector (unsigned-byte 8)) #'char-code data))
                  ((array (unsigned-byte 8) (*)) data))))
    (sha512-update ctx octets)
    (sha512-final ctx)))

(defun sha512-file (path)
  "Compute SHA-512 hash of file at PATH.
Returns a 64-byte array."
  (let ((ctx (make-sha512-ctx))
        (buffer (make-array 8192 :element-type '(unsigned-byte 8))))
    (with-open-file (stream path :element-type '(unsigned-byte 8))
      (loop for n = (read-sequence buffer stream)
            while (> n 0)
            do (sha512-update ctx (subseq buffer 0 n))))
    (sha512-final ctx)))

