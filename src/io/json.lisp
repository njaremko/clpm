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

;;; -----------------------------------------------------------------------------
;;; Reader
;;;
;;; The reader returns values in the same encoding `write-json` accepts:
;;;   object   -> (:object ((key . val) ...))
;;;   array    -> (:array (val ...))
;;;   string   -> string
;;;   integer  -> integer
;;;   float    -> single/double float
;;;   true     -> t
;;;   false    -> :false
;;;   null     -> nil
;;;
;;; `read-json` consumes one value from a stream, leaving anything after it
;;; intact. `read-json-from-string` parses exactly one value and signals a
;;; `clpm-parse-error` if anything besides whitespace follows.
;;; -----------------------------------------------------------------------------

(defun %json-parse-error (stream fmt &rest args)
  (declare (ignore stream))
  (apply #'clpm.errors:signal-error
         'clpm.errors:clpm-parse-error
         (concatenate 'string "JSON: " fmt)
         args))

(declaim (inline %json-whitespace-p))
(defun %json-whitespace-p (c)
  (and c (or (char= c #\Space) (char= c #\Tab)
             (char= c #\Newline) (char= c #\Return))))

(defun %json-skip-ws (stream)
  (loop for c = (peek-char nil stream nil nil)
        while (%json-whitespace-p c)
        do (read-char stream)))

(defun %json-peek (stream)
  (peek-char nil stream nil nil))

(defun %json-expect-char (stream expected)
  (let ((c (read-char stream nil nil)))
    (unless (and c (char= c expected))
      (%json-parse-error stream "expected ~S, got ~:[EOF~;~:*~S~]" expected c))))

(defun %json-read-literal (stream first expected value)
  "Having read FIRST, read remaining chars of EXPECTED and return VALUE."
  (declare (ignore first))
  (loop for i from 1 below (length expected)
        for want = (char expected i)
        for got = (read-char stream nil nil)
        unless (and got (char= got want))
          do (%json-parse-error
              stream "bad literal: expected ~S" expected))
  value)

(defun %json-read-hex4 (stream)
  (let ((result 0))
    (dotimes (i 4)
      (let* ((c (read-char stream nil nil))
             (d (cond
                  ((null c) (%json-parse-error stream "short \\u escape"))
                  ((and (char>= c #\0) (char<= c #\9)) (- (char-code c) (char-code #\0)))
                  ((and (char>= c #\a) (char<= c #\f)) (+ 10 (- (char-code c) (char-code #\a))))
                  ((and (char>= c #\A) (char<= c #\F)) (+ 10 (- (char-code c) (char-code #\A))))
                  (t (%json-parse-error stream "bad hex digit in \\u escape: ~S" c)))))
        (setf result (+ (ash result 4) d))))
    result))

(defun %json-read-string (stream)
  ;; Caller already consumed the opening quote.
  (let ((buf (make-string-output-stream)))
    (loop
      (let ((c (read-char stream nil nil)))
        (cond
          ((null c)
           (%json-parse-error stream "unterminated string"))
          ((char= c #\")
           (return (get-output-stream-string buf)))
          ((char= c #\\)
           (let ((esc (read-char stream nil nil)))
             (case esc
               ((nil) (%json-parse-error stream "unterminated escape"))
               (#\"  (write-char #\" buf))
               (#\\  (write-char #\\ buf))
               (#\/  (write-char #\/ buf))
               (#\b  (write-char #\Backspace buf))
               (#\f  (write-char #\Page buf))
               (#\n  (write-char #\Newline buf))
               (#\r  (write-char #\Return buf))
               (#\t  (write-char #\Tab buf))
               (#\u
                (let ((code (%json-read-hex4 stream)))
                  ;; Surrogate pairs: if high surrogate, demand low surrogate.
                  (cond
                    ((<= #xD800 code #xDBFF)
                     (unless (and (eql (read-char stream nil nil) #\\)
                                  (eql (read-char stream nil nil) #\u))
                       (%json-parse-error stream "lone high surrogate"))
                     (let ((low (%json-read-hex4 stream)))
                       (unless (<= #xDC00 low #xDFFF)
                         (%json-parse-error stream "bad low surrogate"))
                       (write-char (code-char (+ #x10000
                                                 (ash (- code #xD800) 10)
                                                 (- low #xDC00)))
                                   buf)))
                    ((<= #xDC00 code #xDFFF)
                     (%json-parse-error stream "lone low surrogate"))
                    (t (write-char (code-char code) buf)))))
               (t (%json-parse-error stream "bad escape: \\~A" esc)))))
          ((< (char-code c) #x20)
           (%json-parse-error stream "unescaped control char in string"))
          (t (write-char c buf)))))))

(defun %json-digit-p (c)
  (and c (char>= c #\0) (char<= c #\9)))

(defun %json-read-number (stream first)
  "Caller saw FIRST (already consumed); read the rest of the number.
Returns an integer when no decimal point or exponent is present, float otherwise."
  (let ((buf (make-string-output-stream))
        (floatp nil))
    (write-char first buf)
    (let ((leading-zero (and (char= first #\0))))
      ;; Negative sign: read digits after it.
      (when (char= first #\-)
        (let ((n (read-char stream nil nil)))
          (unless (%json-digit-p n)
            (%json-parse-error stream "expected digit after `-`"))
          (write-char n buf)
          (setf leading-zero (char= n #\0))))
      ;; Integer part.
      (unless leading-zero
        (loop for c = (%json-peek stream)
              while (%json-digit-p c)
              do (write-char (read-char stream) buf)))
      ;; If the first digit was 0 (or -0), the next char must not be a digit.
      (when leading-zero
        (let ((c (%json-peek stream)))
          (when (%json-digit-p c)
            (%json-parse-error stream "leading-zero in number"))))
      ;; Fraction.
      (when (eql (%json-peek stream) #\.)
        (setf floatp t)
        (write-char (read-char stream) buf)
        (let ((c (%json-peek stream)))
          (unless (%json-digit-p c)
            (%json-parse-error stream "expected digit after `.`")))
        (loop for c = (%json-peek stream)
              while (%json-digit-p c)
              do (write-char (read-char stream) buf)))
      ;; Exponent.
      (let ((c (%json-peek stream)))
        (when (and c (or (char= c #\e) (char= c #\E)))
          (setf floatp t)
          (write-char (read-char stream) buf)
          (let ((sign (%json-peek stream)))
            (when (and sign (or (char= sign #\+) (char= sign #\-)))
              (write-char (read-char stream) buf)))
          (let ((d (%json-peek stream)))
            (unless (%json-digit-p d)
              (%json-parse-error stream "expected digit in exponent")))
          (loop for c = (%json-peek stream)
                while (%json-digit-p c)
                do (write-char (read-char stream) buf)))))
    (let ((text (get-output-stream-string buf)))
      (handler-case
          (if floatp
              (let ((*read-default-float-format* 'double-float))
                (with-input-from-string (s text)
                  (read s)))
              (parse-integer text))
        (error () (%json-parse-error stream "bad number: ~S" text))))))

(defun %json-read-value (stream)
  (%json-skip-ws stream)
  (let ((c (read-char stream nil nil)))
    (cond
      ((null c) (%json-parse-error stream "unexpected EOF"))
      ((char= c #\{) (%json-read-object stream))
      ((char= c #\[) (%json-read-array stream))
      ((char= c #\") (%json-read-string stream))
      ((char= c #\t) (%json-read-literal stream c "true" t))
      ((char= c #\f) (%json-read-literal stream c "false" :false))
      ((char= c #\n) (%json-read-literal stream c "null" nil))
      ((or (char= c #\-) (%json-digit-p c)) (%json-read-number stream c))
      (t (%json-parse-error stream "unexpected character: ~S" c)))))

(defun %json-read-array (stream)
  ;; Opening `[` already consumed.
  (%json-skip-ws stream)
  (when (eql (%json-peek stream) #\])
    (read-char stream)
    (return-from %json-read-array (list :array '())))
  (let ((items '()))
    (loop
      (push (%json-read-value stream) items)
      (%json-skip-ws stream)
      (let ((c (read-char stream nil nil)))
        (cond
          ((null c) (%json-parse-error stream "unterminated array"))
          ((char= c #\]) (return (list :array (nreverse items))))
          ((char= c #\,)
           (%json-skip-ws stream)
           ;; Trailing comma not allowed.
           (when (eql (%json-peek stream) #\])
             (%json-parse-error stream "trailing comma in array")))
          (t (%json-parse-error stream "expected `,` or `]`, got ~S" c)))))))

(defun %json-read-object (stream)
  ;; Opening `{` already consumed.
  (%json-skip-ws stream)
  (when (eql (%json-peek stream) #\})
    (read-char stream)
    (return-from %json-read-object (list :object '())))
  (let ((entries '()))
    (loop
      (%json-skip-ws stream)
      (let ((c (read-char stream nil nil)))
        (unless (and c (char= c #\"))
          (%json-parse-error stream "expected `\"` to start object key, got ~:[EOF~;~:*~S~]" c)))
      (let ((key (%json-read-string stream)))
        (%json-skip-ws stream)
        (%json-expect-char stream #\:)
        (push (cons key (%json-read-value stream)) entries))
      (%json-skip-ws stream)
      (let ((c (read-char stream nil nil)))
        (cond
          ((null c) (%json-parse-error stream "unterminated object"))
          ((char= c #\}) (return (list :object (nreverse entries))))
          ((char= c #\,)
           (%json-skip-ws stream)
           (when (eql (%json-peek stream) #\})
             (%json-parse-error stream "trailing comma in object")))
          (t (%json-parse-error stream "expected `,` or `}`, got ~S" c)))))))

(defun read-json (stream)
  "Read one JSON value from STREAM and return it in the encoding `write-json`
accepts. Leaves anything after the value (including trailing whitespace) in
the stream."
  (%json-read-value stream))

(defun read-json-from-string (string)
  "Parse STRING as one JSON value. Signals `clpm-parse-error` for malformed
input, missing input, or trailing non-whitespace content."
  (with-input-from-string (s string)
    (let ((value (%json-read-value s)))
      (%json-skip-ws s)
      (let ((c (peek-char nil s nil nil)))
        (when c
          (%json-parse-error s "trailing content after value: ~S" c)))
      value)))

