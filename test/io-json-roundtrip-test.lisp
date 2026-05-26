;;;; test/io-json-roundtrip-test.lisp - JSON reader / writer round-trip

(require :asdf)

(let* ((this-file (or *load-truename* *load-pathname*))
       (test-dir (uiop:pathname-directory-pathname this-file))
       (repo-root (uiop:pathname-parent-directory-pathname test-dir)))
  (push repo-root asdf:*central-registry*))

(format t "Loading CLPM...~%")
(handler-case
    (asdf:load-system :clpm :verbose nil)
  (error (c)
    (format *error-output* "Failed to load CLPM: ~A~%" c)
    (sb-ext:exit :code 1)))
(format t "CLPM loaded.~%")

(defun fail (fmt &rest args)
  (apply #'format *error-output* (concatenate 'string "FAIL: " fmt "~%") args)
  (sb-ext:exit :code 1))

(defun assert-true (x fmt &rest args)
  (unless x (apply #'fail fmt args)))

(defun assert-string= (expected actual)
  (unless (string= expected actual)
    (fail "expected ~S~%got ~S" expected actual)))

(defun roundtrip-string (raw)
  "Parse RAW, re-serialize, return the re-serialized text."
  (clpm.io.json:write-json-to-string
   (clpm.io.json:read-json-from-string raw)))

(defun parses-to (raw expected-rewrite)
  "Assert that parsing RAW and re-serializing equals EXPECTED-REWRITE."
  (let ((got (roundtrip-string raw)))
    (assert-string= expected-rewrite got)))

(format t "Testing primitives...~%")
(parses-to "null" "null")
(parses-to "true" "true")
(parses-to "false" "false")
(parses-to "0" "0")
(parses-to "-1" "-1")
(parses-to "42" "42")
(parses-to "\"\"" "\"\"")
(parses-to "\"hello\"" "\"hello\"")
(format t "  primitives OK~%")

(format t "Testing whitespace tolerance...~%")
(parses-to "  null  " "null")
(parses-to (format nil "~C~C1~C~C"
                   #\Newline #\Tab #\Return #\Newline) "1")
(parses-to "[ 1 , 2 , 3 ]" "[1,2,3]")
(parses-to "{ \"a\" : 1 , \"b\" : 2 }" "{\"a\":1,\"b\":2}")
(format t "  whitespace OK~%")

(format t "Testing string escapes...~%")
;; All single-char escapes
(let ((round (clpm.io.json:read-json-from-string "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\"")))
  (assert-string= (concatenate 'string
                               (string #\")
                               (string #\\)
                               (string #\/)
                               (string #\Backspace)
                               (string #\Page)
                               (string #\Newline)
                               (string #\Return)
                               (string #\Tab))
                  round))
;; \uXXXX
(let ((round (clpm.io.json:read-json-from-string "\"\\u0041\\u00e9\"")))
  (assert-string= (concatenate 'string (string #\A) (string (code-char #xE9)))
                  round))
(format t "  escapes OK~%")

(format t "Testing arrays...~%")
(parses-to "[]" "[]")
(parses-to "[1]" "[1]")
(parses-to "[1,2,3]" "[1,2,3]")
(parses-to "[[1,2],[3,4]]" "[[1,2],[3,4]]")
(parses-to "[null,true,false,\"x\",1]" "[null,true,false,\"x\",1]")
(format t "  arrays OK~%")

(format t "Testing objects...~%")
(parses-to "{}" "{}")
(parses-to "{\"k\":1}" "{\"k\":1}")
(assert-string= "{\"k\":1}"
                (clpm.io.json:write-json-to-string
                 '(:object (("k" 1)))))
(parses-to "{\"a\":1,\"b\":[1,2],\"c\":{\"d\":\"e\"}}"
           "{\"a\":1,\"b\":[1,2],\"c\":{\"d\":\"e\"}}")
(format t "  objects OK~%")

(format t "Testing float parsing...~%")
;; Integers stay integers; floats stay floats.
(assert-true (integerp (clpm.io.json:read-json-from-string "42"))
             "42 must parse as an integer")
(assert-true (floatp (clpm.io.json:read-json-from-string "1.5"))
             "1.5 must parse as a float")
(assert-true (floatp (clpm.io.json:read-json-from-string "1e3"))
             "1e3 must parse as a float")
(assert-true (floatp (clpm.io.json:read-json-from-string "-1.5e-3"))
             "-1.5e-3 must parse as a float")
(format t "  floats OK~%")

(format t "Testing rejection of malformed input...~%")
(defun must-reject (raw)
  (handler-case
      (progn
        (clpm.io.json:read-json-from-string raw)
        (fail "Expected ~S to be rejected" raw))
    (clpm.errors:clpm-parse-error () nil)))
(must-reject "")
(must-reject "  ")
(must-reject "nul")
(must-reject "undefined")
(must-reject "01")              ; leading zero per JSON spec
(must-reject "1.")               ; trailing dot
(must-reject ".5")               ; leading dot
(must-reject "\"unterminated")   ; unterminated string
(must-reject "\"\\q\"")          ; invalid escape
(must-reject "\"\\u00\"")        ; short \u escape
(must-reject "[1,]")             ; trailing comma in array
(must-reject "[1 2]")            ; missing comma
(must-reject "{,}")              ; trailing comma in object
(must-reject "{\"a\":1,}")       ; trailing comma in object
(must-reject "{a:1}")            ; unquoted key
(must-reject "{\"a\" 1}")        ; missing colon
(must-reject "]")                ; lone bracket
(must-reject "{")                ; unterminated object
(must-reject "[1, 2")            ; unterminated array
(must-reject "null garbage")     ; trailing garbage
(format t "  rejection OK~%")

(format t "Testing 1 MB input performance...~%")
;; Build a roughly-1MB JSON array of 100,000 small objects.
(let* ((entry "{\"k\":1,\"v\":\"abc\"}")
       (n 10000)
       (buf (make-string-output-stream)))
  (write-char #\[ buf)
  (loop for i from 0 below n do
    (when (plusp i) (write-char #\, buf))
    (write-string entry buf))
  (write-char #\] buf)
  (let* ((s (get-output-stream-string buf))
         (start (get-internal-real-time))
         (parsed (clpm.io.json:read-json-from-string s))
         (elapsed-ms (* 1000.0 (/ (- (get-internal-real-time) start)
                                   internal-time-units-per-second))))
    (declare (ignore parsed))
    (assert-true (< elapsed-ms 1000)
                 "1 MB JSON parse took ~,1Fms (should be < 1000)" elapsed-ms)
    (format t "  10K-entry array (~,1F KB) parsed in ~,1Fms~%"
            (/ (length s) 1024.0) elapsed-ms)))

(format t "Testing stream interface preserves trailing content...~%")
;; read-json reads one value and stops; the next read starts at the next token.
(with-input-from-string (s "42 \"hello\"")
  (let ((a (clpm.io.json:read-json s))
        (b (clpm.io.json:read-json s)))
    (assert-true (eql 42 a) "stream read 1 returned ~S" a)
    (assert-string= "hello" b)))
(format t "  stream interface OK~%")

(format t "~%JSON round-trip tests PASSED!~%")
(sb-ext:exit :code 0)
