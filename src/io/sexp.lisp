;;;; io/sexp.lisp - Safe S-expression reading and canonical writing

(in-package #:clpm.io.sexp)

;;; Safe S-expression reader
;;; Reads with *read-eval* = nil and validates structure

(defparameter *allowed-packages*
  '("KEYWORD")
  "Packages allowed in safe sexp reading.")

(define-condition sexp-read-error (error)
  ((message :initarg :message :reader sexp-read-error-message)
   (position :initarg :position :initform nil :reader sexp-read-error-position))
  (:report (lambda (c s)
             (format s "S-expression read error~@[ at position ~D~]: ~A"
                     (sexp-read-error-position c)
                     (sexp-read-error-message c)))))

(defun validate-sexp (form)
  "Validate that FORM contains only allowed elements.
Only keywords, strings, numbers, and lists thereof are allowed."
  (labels ((check (x)
             (cond
               ((null x) t)
               ((eq x t) t)
               ((keywordp x) t)
               ((stringp x) t)
               ((integerp x) t)
               ((floatp x) t)
               ((consp x)
                (check (car x))
                (check (cdr x)))
               ((symbolp x)
                (if (member (package-name (symbol-package x))
                            *allowed-packages*
                            :test #'string=)
                    t
                    (error 'sexp-read-error
                           :message (format nil "Symbol ~S from package ~A not allowed"
                                            x (package-name (symbol-package x))))))
               (t
                (error 'sexp-read-error
                       :message (format nil "Value ~S of type ~A not allowed"
                                        x (type-of x)))))))
    (check form)
    form))

(defun read-safe-sexp (stream &key (validate t))
  "Read a single S-expression safely from STREAM.
*read-eval* is bound to nil and the result is validated."
  (let ((*read-eval* nil)
        ;; Read keywords normally (they are prefixed with : in the file) while
        ;; allowing literal NIL and T to be read as CL:NIL and CL:T.
        (*package* (find-package "COMMON-LISP")))
    (handler-case
        (let ((form (read stream nil :eof)))
          (when (eq form :eof)
            (error 'sexp-read-error :message "Unexpected end of file"))
          (if validate
              (validate-sexp form)
              form))
      (reader-error (c)
        (error 'sexp-read-error
               :message (format nil "Reader error: ~A" c)
               :position (when (typep stream 'file-stream)
                           (file-position stream)))))))

(defun read-safe-sexp-from-string (string &key (validate t))
  "Read a single S-expression safely from STRING."
  (with-input-from-string (stream string)
    (read-safe-sexp stream :validate validate)))

(defun read-safe-sexp-from-file (path &key (validate t))
  "Read a single S-expression safely from file at PATH."
  (with-open-file (stream path :direction :input
                               :external-format :utf-8)
    (read-safe-sexp stream :validate validate)))

;;; Canonical S-expression writer
;;; Produces deterministic output for stable diffs

(defun write-canonical-sexp (form stream &key (indent 0) (pretty t))
  "Write FORM to STREAM in canonical format.
When PRETTY is true, format with indentation for readability."
  (labels ((indent-to (level)
             (when pretty
               (fresh-line stream)
               (dotimes (_ (* level 2))
                 (write-char #\Space stream))))
           (write-string-literal (s)
             (write-char #\" stream)
             (loop for c across s do
               (case c
                 (#\" (write-string "\\\"" stream))
                 (#\\ (write-string "\\\\" stream))
                 (#\Newline (write-string "\\n" stream))
                 (#\Tab (write-string "\\t" stream))
                 (#\Return (write-string "\\r" stream))
                 (t (write-char c stream))))
             (write-char #\" stream))
           (write-form (f level)
             (cond
               ((null f)
                (write-string "nil" stream))
               ((eq f t)
                (write-string "t" stream))
               ((keywordp f)
                (write-char #\: stream)
                (write-string (string-downcase (symbol-name f)) stream))
               ((stringp f)
                (write-string-literal f))
               ((integerp f)
                (format stream "~D" f))
               ((floatp f)
                (format stream "~F" f))
               ((consp f)
                ;; General list (supports dotted pairs)
                (write-char #\( stream)
                (let ((first t)
                      (tail f))
                  (loop
                    (cond
                      ((null tail)
                       (return))
                      ((consp tail)
                       (let ((item (car tail)))
                         (cond
                           ((and (consp item)
                                 (keywordp (car item)))
                            ;; Nested structure - indent
                            (unless first
                              (indent-to (1+ level)))
                            (write-form item (1+ level)))
                           (first
                            (write-form item (1+ level))
                            (setf first nil))
                           (t
                            (write-char #\Space stream)
                            (write-form item (1+ level))))
                         (setf tail (cdr tail))))
                      (t
                       (write-string " . " stream)
                       (write-form tail (1+ level))
                       (return)))))
                (write-char #\) stream))
               (t
                (error "Cannot write ~S in canonical format" f)))))
    (write-form form indent)
    (when pretty
      (fresh-line stream))))

(defun write-canonical-sexp-to-string (form &key (pretty t))
  "Write FORM to a string in canonical format."
  (with-output-to-string (stream)
    (write-canonical-sexp form stream :pretty pretty)))

(defun write-canonical-sexp-to-file (form path &key (pretty t))
  "Write FORM to file at PATH in canonical format."
  (with-open-file (stream path :direction :output
                               :if-exists :supersede
                               :external-format :utf-8)
    (write-canonical-sexp form stream :pretty pretty)))

;;; Specialized readers for CLPM data formats

(defun read-manifest (path)
  "Read a clpm.project manifest file."
  (let ((form (read-safe-sexp-from-file path)))
    (unless (and (consp form) (eq (car form) :project))
      (error 'sexp-read-error
             :message "Manifest must start with (project ...)"))
    form))

(defun read-lockfile (path)
  "Read a clpm.lock lockfile."
  (let ((form (read-safe-sexp-from-file path)))
    (unless (and (consp form) (eq (car form) :lock))
      (error 'sexp-read-error
             :message "Lockfile must start with (lock ...)"))
    form))

(defun write-lockfile (lockfile path)
  "Write a lockfile to PATH in canonical format."
  (write-canonical-sexp-to-file lockfile path :pretty t))

(defun read-registry-snapshot (path)
  "Read a registry snapshot.sxp file."
  (let ((form (read-safe-sexp-from-file path)))
    (unless (and (consp form) (eq (car form) :snapshot))
      (error 'sexp-read-error
             :message "Snapshot must start with (snapshot ...)"))
    form))

(defun read-release-metadata (path)
  "Read a release.sxp metadata file."
  (let ((form (read-safe-sexp-from-file path)))
    (unless (and (consp form) (eq (car form) :release))
      (error 'sexp-read-error
             :message "Release metadata must start with (release ...)"))
    form))
