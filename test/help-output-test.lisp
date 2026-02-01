;;;; test/help-output-test.lisp - Help output tests

(require :asdf)

;; Add repo root directory to ASDF load path.
(let* ((this-file (or *load-truename* *load-pathname*))
       (test-dir (uiop:pathname-directory-pathname this-file))
       (repo-root (uiop:pathname-parent-directory-pathname test-dir)))
  (push repo-root asdf:*central-registry*))

;; Load CLPM
(format t "Loading CLPM...~%")
(handler-case
    (asdf:load-system :clpm)
  (error (c)
    (format *error-output* "Failed to load CLPM: ~A~%" c)
    (sb-ext:exit :code 1)))

(format t "CLPM loaded successfully.~%~%")

(defun fail (fmt &rest args)
  (apply #'format *error-output* (concatenate 'string fmt "~%") args)
  (sb-ext:exit :code 1))

(defun assert-eql (expected actual)
  (unless (eql expected actual)
    (fail "Assertion failed: expected ~S, got ~S" expected actual)))

(defun assert-true (x fmt &rest args)
  (unless x
    (apply #'fail fmt args)))

(defun assert-contains (haystack needle)
  (assert-true (and (stringp haystack)
                    (search needle haystack :test #'char-equal))
               "Expected output to contain ~S, got:~%~A"
               needle
               haystack))

(defun run-cli-captured (args)
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((*standard-output* out)
          (*error-output* err))
      (let ((code (clpm:run-cli args)))
        (values code
                (get-output-stream-string out)
                (get-output-stream-string err))))))

(format t "Testing `--help` output...~%")
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("--help"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Commands:")
  (assert-contains stdout "help")
  (assert-contains stdout "doctor")
  (assert-contains stdout "new")
  (assert-contains stdout "install"))
(format t "  `--help` output PASSED~%")

(format t "Testing `clpm help new` output...~%")
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "new"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage: clpm new")
  (assert-contains stdout "--bin")
  (assert-contains stdout "--lib"))
(format t "  `clpm help new` PASSED~%")

(format t "Testing `<cmd> --help` output...~%")
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("new" "--help"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage: clpm new"))
(format t "  `<cmd> --help` PASSED~%")

(format t "Testing unknown help target...~%")
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "does-not-exist"))
  (declare (ignore stdout))
  (assert-eql 1 code)
  (assert-contains stderr "Unknown command"))
(format t "  Unknown help target PASSED~%")

(format t "~%Help output tests PASSED!~%")
(sb-ext:exit :code 0)
