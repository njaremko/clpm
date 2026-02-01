;;;; crypto/sha256.lisp - Pure Common Lisp SHA256 implementation

(in-package #:clpm.crypto.sha256)

;;; SHA-256 Constants

(defconstant +sha256-block-size+ 64)
(defconstant +sha256-digest-size+ 32)

(defparameter *sha256-k*
  (make-array 64
              :element-type '(unsigned-byte 32)
              :initial-contents
              '(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5
                #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
                #xd807aa98 #x12835b01 #x243185be #x550c7dc3
                #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
                #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc
                #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
                #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7
                #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
                #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
                #x650a7354 #x766a0abb #x81c2c92e #x92722c85
                #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3
                #xd192e819 #xd6990624 #xf40e3585 #x106aa070
                #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5
                #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
                #x748f82ee #x78a5636f #x84c87814 #x8cc70208
                #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))
  "SHA-256 round constants")

(defparameter *sha256-init*
  (make-array 8
              :element-type '(unsigned-byte 32)
              :initial-contents
              '(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))
  "SHA-256 initial hash values")

;;; Helper functions

(declaim (inline u32+ rotr32 shr32))

(defun u32+ (&rest args)
  "32-bit addition with wrap-around."
  (ldb (byte 32 0) (apply #'+ args)))

(defun rotr32 (x n)
  "Rotate 32-bit value X right by N bits."
  (declare (type (unsigned-byte 32) x)
           (type (integer 0 31) n))
  (logior (ldb (byte 32 0) (ash x (- n)))
          (ldb (byte 32 0) (ash x (- 32 n)))))

(defun shr32 (x n)
  "Shift 32-bit value X right by N bits."
  (declare (type (unsigned-byte 32) x)
           (type (integer 0 31) n))
  (ldb (byte 32 0) (ash x (- n))))

(declaim (inline ch maj sigma0 sigma1 lsigma0 lsigma1))

(defun ch (x y z)
  (logxor (logand x y) (logand (lognot x) z)))

(defun maj (x y z)
  (logxor (logand x y) (logand x z) (logand y z)))

(defun sigma0 (x)
  (logxor (rotr32 x 2) (rotr32 x 13) (rotr32 x 22)))

(defun sigma1 (x)
  (logxor (rotr32 x 6) (rotr32 x 11) (rotr32 x 25)))

(defun lsigma0 (x)
  (logxor (rotr32 x 7) (rotr32 x 18) (shr32 x 3)))

(defun lsigma1 (x)
  (logxor (rotr32 x 17) (rotr32 x 19) (shr32 x 10)))

;;; SHA-256 context

(defstruct sha256-ctx
  (state (make-array 8 :element-type '(unsigned-byte 32)
                       :initial-contents (coerce *sha256-init* 'list))
         :type (simple-array (unsigned-byte 32) (8)))
  (buffer (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0)
          :type (simple-array (unsigned-byte 8) (64)))
  (buflen 0 :type (integer 0 64))
  (total 0 :type (unsigned-byte 64)))

(defun sha256-transform (ctx block)
  "Process a 64-byte block."
  (declare (type sha256-ctx ctx)
           (type (simple-array (unsigned-byte 8) (*)) block))
  (let ((w (make-array 64 :element-type '(unsigned-byte 32) :initial-element 0))
        (state (sha256-ctx-state ctx)))
    ;; Prepare message schedule
    (dotimes (i 16)
      (setf (aref w i)
            (logior (ash (aref block (* i 4)) 24)
                    (ash (aref block (+ (* i 4) 1)) 16)
                    (ash (aref block (+ (* i 4) 2)) 8)
                    (aref block (+ (* i 4) 3)))))
    (loop for i from 16 below 64 do
      (setf (aref w i)
            (u32+ (lsigma1 (aref w (- i 2)))
                  (aref w (- i 7))
                  (lsigma0 (aref w (- i 15)))
                  (aref w (- i 16)))))
    ;; Initialize working variables
    (let ((a (aref state 0))
          (b (aref state 1))
          (c (aref state 2))
          (d (aref state 3))
          (e (aref state 4))
          (f (aref state 5))
          (g (aref state 6))
          (h (aref state 7)))
      ;; Main loop
      (dotimes (i 64)
        (let ((t1 (u32+ h (sigma1 e) (ch e f g) (aref *sha256-k* i) (aref w i)))
              (t2 (u32+ (sigma0 a) (maj a b c))))
          (setf h g
                g f
                f e
                e (u32+ d t1)
                d c
                c b
                b a
                a (u32+ t1 t2))))
      ;; Update state
      (setf (aref state 0) (u32+ a (aref state 0))
            (aref state 1) (u32+ b (aref state 1))
            (aref state 2) (u32+ c (aref state 2))
            (aref state 3) (u32+ d (aref state 3))
            (aref state 4) (u32+ e (aref state 4))
            (aref state 5) (u32+ f (aref state 5))
            (aref state 6) (u32+ g (aref state 6))
            (aref state 7) (u32+ h (aref state 7))))))

(defun sha256-update (ctx data)
  "Update SHA-256 context with DATA (octet sequence)."
  (declare (type sha256-ctx ctx))
  (let ((data (coerce data '(simple-array (unsigned-byte 8) (*))))
        (buffer (sha256-ctx-buffer ctx))
        (buflen (sha256-ctx-buflen ctx))
        (pos 0)
        (len (length data)))
    (incf (sha256-ctx-total ctx) len)
    ;; Fill buffer if there's data left over
    (when (> buflen 0)
      (let ((space (- 64 buflen)))
        (if (>= len space)
            (progn
              (replace buffer data :start1 buflen :end1 64 :start2 0 :end2 space)
              (sha256-transform ctx buffer)
              (setf pos space
                    buflen 0))
            (progn
              (replace buffer data :start1 buflen :start2 0)
              (setf (sha256-ctx-buflen ctx) (+ buflen len))
              (return-from sha256-update ctx)))))
    ;; Process complete blocks
    (loop while (>= (- len pos) 64) do
      (let ((block (make-array 64 :element-type '(unsigned-byte 8))))
        (replace block data :start2 pos :end2 (+ pos 64))
        (sha256-transform ctx block)
        (incf pos 64)))
    ;; Buffer remaining data
    (when (< pos len)
      (replace buffer data :start1 0 :start2 pos)
      (setf (sha256-ctx-buflen ctx) (- len pos)))
    ctx))

(defun sha256-final (ctx)
  "Finalize SHA-256 context and return digest."
  (declare (type sha256-ctx ctx))
  (let* ((buffer (sha256-ctx-buffer ctx))
         (buflen (sha256-ctx-buflen ctx))
         (total-bits (* 8 (sha256-ctx-total ctx))))
    ;; Padding
    (setf (aref buffer buflen) #x80)
    (incf buflen)
    (when (> buflen 56)
      (fill buffer 0 :start buflen :end 64)
      (sha256-transform ctx buffer)
      (setf buflen 0))
    (fill buffer 0 :start buflen :end 56)
    ;; Append length in bits (big-endian)
    (loop for i from 0 below 8 do
      (setf (aref buffer (+ 56 i))
            (ldb (byte 8 (* 8 (- 7 i))) total-bits)))
    (sha256-transform ctx buffer)
    ;; Extract digest
    (let ((digest (make-array 32 :element-type '(unsigned-byte 8))))
      (dotimes (i 8)
        (let ((word (aref (sha256-ctx-state ctx) i)))
          (setf (aref digest (* i 4)) (ldb (byte 8 24) word)
                (aref digest (+ (* i 4) 1)) (ldb (byte 8 16) word)
                (aref digest (+ (* i 4) 2)) (ldb (byte 8 8) word)
                (aref digest (+ (* i 4) 3)) (ldb (byte 8 0) word))))
      digest)))

;;; High-level interface

(defun sha256 (data)
  "Compute SHA-256 hash of DATA (string or octet vector).
Returns a 32-byte array."
  (let ((ctx (make-sha256-ctx))
        (octets (etypecase data
                  (string (map '(vector (unsigned-byte 8)) #'char-code data))
                  ((array (unsigned-byte 8) (*)) data))))
    (sha256-update ctx octets)
    (sha256-final ctx)))

(defun sha256-file (path)
  "Compute SHA-256 hash of file at PATH.
Returns a 32-byte array."
  (let ((ctx (make-sha256-ctx))
        (buffer (make-array 8192 :element-type '(unsigned-byte 8))))
    (with-open-file (stream path :element-type '(unsigned-byte 8))
      (loop for n = (read-sequence buffer stream)
            while (> n 0)
            do (sha256-update ctx (subseq buffer 0 n))))
    (sha256-final ctx)))

(defun sha256-tree (directory &key (exclude '(".git" ".hg" ".svn" ".clpm")))
  "Compute deterministic SHA-256 hash of a directory tree.
Files are hashed in lexicographic order by path with metadata."
  (let ((ctx (make-sha256-ctx))
        (files (clpm.io.fs:walk-files directory :exclude exclude)))
    ;; Hash each file with metadata
    (dolist (file-entry files)
      (let* ((rel-path (car file-entry))
             (full-path (cdr file-entry))
             (contents (with-open-file (s full-path :element-type '(unsigned-byte 8))
                         (let ((data (make-array (file-length s)
                                                 :element-type '(unsigned-byte 8))))
                           (read-sequence data s)
                           data))))
        ;; Format: path \0 mode \0 size \0 contents
        (sha256-update ctx (map '(vector (unsigned-byte 8)) #'char-code rel-path))
        (sha256-update ctx #(0))  ; null separator
        (sha256-update ctx (map '(vector (unsigned-byte 8)) #'char-code "644"))  ; mode
        (sha256-update ctx #(0))  ; null separator
        (sha256-update ctx (map '(vector (unsigned-byte 8)) #'char-code
                                (princ-to-string (length contents))))
        (sha256-update ctx #(0))  ; null separator
        (sha256-update ctx contents)))
    (sha256-final ctx)))

;;; Hex conversion

(defun bytes-to-hex (bytes)
  "Convert byte array to lowercase hex string."
  (with-output-to-string (s)
    (loop for byte across bytes do
      (format s "~(~2,'0x~)" byte))))

(defun hex-to-bytes (hex)
  "Convert hex string to byte array."
  (let* ((len (length hex))
         (bytes (make-array (/ len 2) :element-type '(unsigned-byte 8))))
    (loop for i from 0 below len by 2 do
      (setf (aref bytes (/ i 2))
            (parse-integer hex :start i :end (+ i 2) :radix 16)))
    bytes))
