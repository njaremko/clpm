;;;; io/json.lisp - Minimal deterministic JSON writer

(in-package #:clpm.io.json)

(defun %write-json-string (s stream)
  (write-char #\" stream)
  (loop for c across s do
    (case c
      (#\" (write-string "\\\"" stream))
      (#\\ (write-string "\\\\" stream))
      (#\Backspace (write-string "\\b" stream))
      (#\FormFeed (write-string "\\f" stream))
      (#\Newline (write-string "\\n" stream))
      (#\Return (write-string "\\r" stream))
      (#\Tab (write-string "\\t" stream))
      (t
       (let ((code (char-code c)))
         (if (< code 32)
             (format stream "\\u~4,'0X" code)
             (write-char c stream))))))
  (write-char #\" stream))

(defun %write-json-array (values stream)
  (write-char #\[ stream)
  (loop for v in values
        for first = t then nil do
          (unless first
            (write-char #\, stream))
          (write-json v stream))
  (write-char #\] stream))

(defun %write-json-object (entries stream)
  (write-char #\{ stream)
  (loop for entry in entries
        for first = t then nil do
          (unless first
            (write-char #\, stream))
          (etypecase entry
            (cons
             (%write-json-string (car entry) stream)
             (write-char #\: stream)
             (write-json (cdr entry) stream))
            (list
             (unless (= (length entry) 2)
               (error "JSON object entry must be (key value), got: ~S" entry))
             (%write-json-string (first entry) stream)
             (write-char #\: stream)
             (write-json (second entry) stream))))
  (write-char #\} stream))

(defun write-json (value stream)
  "Write VALUE to STREAM as JSON.

Supported VALUE encodings:
- NIL              => null
- T                => true
- :FALSE           => false
- string           => JSON string
- integer/float    => JSON number
- (:array (..))    => JSON array
- (:object ((\"k\" . v) ...)) or (:object ((\"k\" v) ...)) => JSON object"
  (cond
    ((null value) (write-string "null" stream))
    ((eq value t) (write-string "true" stream))
    ((eq value :false) (write-string "false" stream))
    ((stringp value) (%write-json-string value stream))
    ((integerp value) (format stream "~D" value))
    ((floatp value) (format stream "~F" value))
    ((and (consp value) (eq (car value) :array))
     (%write-json-array (cadr value) stream))
    ((and (consp value) (eq (car value) :object))
     (%write-json-object (cadr value) stream))
    (t
     (error "Unsupported JSON value: ~S" value))))

(defun write-json-to-string (value)
  "Write VALUE to a JSON string."
  (with-output-to-string (s)
    (write-json value s)))

