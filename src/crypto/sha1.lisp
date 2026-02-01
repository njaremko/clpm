;;;; crypto/sha1.lisp - Pure Common Lisp SHA1 implementation

(in-package #:clpm.crypto.sha1)

;;; SHA-1 Constants

(defconstant +sha1-block-size+ 64)
(defconstant +sha1-digest-size+ 20)

(defparameter *sha1-init*
  (make-array 5
              :element-type '(unsigned-byte 32)
              :initial-contents
              '(#x67452301
                #xefcdab89
                #x98badcfe
                #x10325476
                #xc3d2e1f0))
  "SHA-1 initial hash values.")

;;; Helper functions

(declaim (inline u32+ rotl32))

(defun u32+ (&rest args)
  "32-bit addition with wrap-around."
  (ldb (byte 32 0) (apply #'+ args)))

(defun rotl32 (x n)
  "Rotate 32-bit value X left by N bits."
  (declare (type (unsigned-byte 32) x)
           (type (integer 0 31) n))
  (logior (ldb (byte 32 0) (ash x n))
          (ldb (byte 32 0) (ash x (- n 32)))))

;;; SHA-1 context

(defstruct sha1-ctx
  (state (make-array 5 :element-type '(unsigned-byte 32)
                       :initial-contents (coerce *sha1-init* 'list))
         :type (simple-array (unsigned-byte 32) (5)))
  (buffer (make-array 64 :element-type '(unsigned-byte 8) :initial-element 0)
          :type (simple-array (unsigned-byte 8) (64)))
  (buflen 0 :type (integer 0 64))
  (total 0 :type (unsigned-byte 64)))

(defun sha1-transform (ctx block)
  "Process a 64-byte BLOCK."
  (declare (type sha1-ctx ctx)
           (type (simple-array (unsigned-byte 8) (*)) block))
  (let ((w (make-array 80 :element-type '(unsigned-byte 32) :initial-element 0))
        (state (sha1-ctx-state ctx)))
    ;; Prepare message schedule.
    (dotimes (i 16)
      (setf (aref w i)
            (logior (ash (aref block (* i 4)) 24)
                    (ash (aref block (+ (* i 4) 1)) 16)
                    (ash (aref block (+ (* i 4) 2)) 8)
                    (aref block (+ (* i 4) 3)))))
    (loop for i from 16 below 80 do
      (setf (aref w i)
            (rotl32 (logxor (aref w (- i 3))
                            (aref w (- i 8))
                            (aref w (- i 14))
                            (aref w (- i 16)))
                    1)))
    ;; Working variables.
    (let ((a (aref state 0))
          (b (aref state 1))
          (c (aref state 2))
          (d (aref state 3))
          (e (aref state 4)))
      (dotimes (i 80)
        (let* ((f (cond
                    ((<= 0 i 19)
                     (logior (logand b c)
                             (logand (lognot b) d)))
                    ((<= 20 i 39)
                     (logxor b c d))
                    ((<= 40 i 59)
                     (logior (logand b c)
                             (logand b d)
                             (logand c d)))
                    (t
                     (logxor b c d))))
               (k (cond
                    ((<= 0 i 19) #x5a827999)
                    ((<= 20 i 39) #x6ed9eba1)
                    ((<= 40 i 59) #x8f1bbcdc)
                    (t #xca62c1d6)))
               (temp (u32+ (rotl32 a 5) f e k (aref w i))))
          (setf e d
                d c
                c (rotl32 b 30)
                b a
                a temp)))
      ;; Update state.
      (setf (aref state 0) (u32+ (aref state 0) a)
            (aref state 1) (u32+ (aref state 1) b)
            (aref state 2) (u32+ (aref state 2) c)
            (aref state 3) (u32+ (aref state 3) d)
            (aref state 4) (u32+ (aref state 4) e)))))

(defun sha1-update (ctx data)
  "Update SHA-1 context with DATA (octet sequence)."
  (declare (type sha1-ctx ctx))
  (let ((data (coerce data '(simple-array (unsigned-byte 8) (*))))
        (buffer (sha1-ctx-buffer ctx))
        (buflen (sha1-ctx-buflen ctx))
        (pos 0)
        (len (length data)))
    (incf (sha1-ctx-total ctx) len)
    ;; Fill buffer if there's data left over.
    (when (> buflen 0)
      (let ((space (- 64 buflen)))
        (if (>= len space)
            (progn
              (replace buffer data :start1 buflen :end1 64 :start2 0 :end2 space)
              (sha1-transform ctx buffer)
              (setf pos space
                    buflen 0))
            (progn
              (replace buffer data :start1 buflen :start2 0)
              (setf (sha1-ctx-buflen ctx) (+ buflen len))
              (return-from sha1-update ctx)))))
    ;; Process complete blocks.
    (loop while (>= (- len pos) 64) do
      (let ((block (make-array 64 :element-type '(unsigned-byte 8))))
        (replace block data :start2 pos :end2 (+ pos 64))
        (sha1-transform ctx block)
        (incf pos 64)))
    ;; Buffer remaining data.
    (when (< pos len)
      (replace buffer data :start1 0 :start2 pos)
      (setf (sha1-ctx-buflen ctx) (- len pos)))
    ctx))

(defun sha1-final (ctx)
  "Finalize SHA-1 context and return digest."
  (declare (type sha1-ctx ctx))
  (let* ((buffer (sha1-ctx-buffer ctx))
         (buflen (sha1-ctx-buflen ctx))
         (total-bits (* 8 (sha1-ctx-total ctx))))
    ;; Padding.
    (setf (aref buffer buflen) #x80)
    (incf buflen)
    (when (> buflen 56)
      (fill buffer 0 :start buflen :end 64)
      (sha1-transform ctx buffer)
      (setf buflen 0))
    (fill buffer 0 :start buflen :end 56)
    ;; Append length in bits (big-endian).
    (loop for i from 0 below 8 do
      (setf (aref buffer (+ 56 i))
            (ldb (byte 8 (* 8 (- 7 i))) total-bits)))
    (sha1-transform ctx buffer)
    ;; Extract digest.
    (let ((digest (make-array 20 :element-type '(unsigned-byte 8))))
      (dotimes (i 5)
        (let ((word (aref (sha1-ctx-state ctx) i)))
          (setf (aref digest (* i 4)) (ldb (byte 8 24) word)
                (aref digest (+ (* i 4) 1)) (ldb (byte 8 16) word)
                (aref digest (+ (* i 4) 2)) (ldb (byte 8 8) word)
                (aref digest (+ (* i 4) 3)) (ldb (byte 8 0) word))))
      digest)))

;;; High-level interface

(defun sha1 (data)
  "Compute SHA-1 hash of DATA (string or octet vector).
Returns a 20-byte array."
  (let ((ctx (make-sha1-ctx))
        (octets (etypecase data
                  (string (map '(vector (unsigned-byte 8)) #'char-code data))
                  ((array (unsigned-byte 8) (*)) data))))
    (sha1-update ctx octets)
    (sha1-final ctx)))

(defun sha1-stream (stream &key (buffer-size 8192))
  "Compute SHA-1 hash of STREAM.

STREAM must be a binary input stream of octets.
Returns a 20-byte array."
  (let ((ctx (make-sha1-ctx))
        (buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
    (loop for n = (read-sequence buffer stream)
          while (> n 0)
          do (sha1-update ctx (subseq buffer 0 n)))
    (sha1-final ctx)))

(defun sha1-file (path)
  "Compute SHA-1 hash of file at PATH.
Returns a 20-byte array."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (sha1-stream stream)))

