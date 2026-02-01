;;;; solver/version.lisp - SemVer version parsing and comparison

(in-package #:clpm.solver.version)

;;; Version structure

(defstruct version
  "A semantic version."
  (major 0 :type (integer 0))
  (minor 0 :type (integer 0))
  (patch 0 :type (integer 0))
  (prerelease nil :type (or null string))
  (build nil :type (or null string))
  (raw nil :type (or null string)))

;;; Parsing

(defun parse-version (string)
  "Parse a version string into a version struct.
Supports:
  - SemVer: MAJOR.MINOR.PATCH[-PRERELEASE][+BUILD]
  - Simple: MAJOR.MINOR or MAJOR
  - Git-style: 0.0.0+git.SHORTSHA"
  (when (null string)
    (return-from parse-version nil))
  (let ((v (make-version :raw string))
        (pos 0)
        (len (length string)))
    ;; Skip leading 'v' if present
    (when (and (> len 0) (char-equal (char string 0) #\v))
      (incf pos))
    ;; Parse major
    (multiple-value-bind (major next-pos)
        (parse-integer string :start pos :junk-allowed t)
      (unless major
        (return-from parse-version (make-version :raw string)))
      (setf (version-major v) major
            pos (or next-pos pos)))
    ;; Parse .minor if present
    (when (and (< pos len) (char= (char string pos) #\.))
      (incf pos)
      (multiple-value-bind (minor next-pos)
          (parse-integer string :start pos :junk-allowed t)
        (when minor
          (setf (version-minor v) minor
                pos (or next-pos pos)))))
    ;; Parse .patch if present
    (when (and (< pos len) (char= (char string pos) #\.))
      (incf pos)
      (multiple-value-bind (patch next-pos)
          (parse-integer string :start pos :junk-allowed t)
        (when patch
          (setf (version-patch v) patch
                pos (or next-pos pos)))))
    ;; Parse -prerelease if present
    (when (and (< pos len) (char= (char string pos) #\-))
      (incf pos)
      (let ((end (or (position-if (lambda (c) (char= c #\+)) string :start pos)
                     len)))
        (setf (version-prerelease v) (subseq string pos end)
              pos end)))
    ;; Parse +build if present
    (when (and (< pos len) (char= (char string pos) #\+))
      (incf pos)
      (setf (version-build v) (subseq string pos)))
    v))

;;; Comparison

(defun compare-prerelease (a b)
  "Compare prerelease strings.
Returns -1, 0, or 1.
nil sorts higher (release > prerelease)."
  (cond
    ((and (null a) (null b)) 0)
    ((null a) 1)   ; release > prerelease
    ((null b) -1)
    (t
     ;; Compare prerelease identifiers
     (let ((a-parts (split-prerelease a))
           (b-parts (split-prerelease b)))
       (loop for a-part in a-parts
             for b-part in b-parts
             for cmp = (compare-prerelease-part a-part b-part)
             unless (zerop cmp) return cmp
             finally (return (- (length a-parts) (length b-parts))))))))

(defun split-prerelease (s)
  "Split prerelease string into parts by dots."
  (let ((parts '())
        (start 0))
    (loop for i from 0 to (length s)
          when (or (= i (length s))
                   (char= (char s i) #\.))
            do (push (subseq s start i) parts)
               (setf start (1+ i)))
    (nreverse parts)))

(defun compare-prerelease-part (a b)
  "Compare two prerelease parts.
Numeric parts sort lower than alphanumeric."
  (let ((a-num (ignore-errors (parse-integer a)))
        (b-num (ignore-errors (parse-integer b))))
    (cond
      ((and a-num b-num) (- a-num b-num))
      (a-num -1)  ; numeric < alphanumeric
      (b-num 1)
      (t (cond
           ((string< a b) -1)
           ((string> a b) 1)
           (t 0))))))

(defun version-compare (a b)
  "Compare two versions.
Returns -1, 0, or 1."
  (unless (version-p a) (setf a (parse-version a)))
  (unless (version-p b) (setf b (parse-version b)))
  (cond
    ((< (version-major a) (version-major b)) -1)
    ((> (version-major a) (version-major b)) 1)
    ((< (version-minor a) (version-minor b)) -1)
    ((> (version-minor a) (version-minor b)) 1)
    ((< (version-patch a) (version-patch b)) -1)
    ((> (version-patch a) (version-patch b)) 1)
    (t (compare-prerelease (version-prerelease a) (version-prerelease b)))))

(defun version< (a b)
  "Return true if version A < version B."
  (minusp (version-compare a b)))

(defun version<= (a b)
  "Return true if version A <= version B."
  (not (plusp (version-compare a b))))

(defun version> (a b)
  "Return true if version A > version B."
  (plusp (version-compare a b)))

(defun version>= (a b)
  "Return true if version A >= version B."
  (not (minusp (version-compare a b))))

(defun version= (a b)
  "Return true if version A = version B."
  (zerop (version-compare a b)))

;;; Serialization

(defun version-to-string (v)
  "Convert version struct to string."
  (if (version-raw v)
      (version-raw v)
      (with-output-to-string (s)
        (format s "~D.~D.~D"
                (version-major v)
                (version-minor v)
                (version-patch v))
        (when (version-prerelease v)
          (format s "-~A" (version-prerelease v)))
        (when (version-build v)
          (format s "+~A" (version-build v))))))

;;; Utility

(defun bump-major (v)
  "Return new version with major bumped, minor/patch reset."
  (make-version :major (1+ (version-major v))
                :minor 0
                :patch 0))

(defun bump-minor (v)
  "Return new version with minor bumped, patch reset."
  (make-version :major (version-major v)
                :minor (1+ (version-minor v))
                :patch 0))

(defun bump-patch (v)
  "Return new version with patch bumped."
  (make-version :major (version-major v)
                :minor (version-minor v)
                :patch (1+ (version-patch v))))
