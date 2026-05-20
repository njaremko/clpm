;;;; test/skill-command-test.lisp - `clpm skill` command tests

(require :asdf)

(let* ((this-file (or *load-truename* *load-pathname*))
       (test-dir (uiop:pathname-directory-pathname this-file))
       (repo-root (uiop:pathname-parent-directory-pathname test-dir)))
  (push repo-root asdf:*central-registry*))

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

(format t "Testing `clpm skill` output...~%")
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("skill"))
  (assert-eql 0 code)
  (assert-true (string= "" stderr)
               "expected empty stderr, got:~%~A" stderr)
  (assert-contains stdout "---")
  (assert-contains stdout "name: clpm")
  (assert-contains stdout "# CLPM Agent Skill")
  (assert-contains stdout "clpm install")
  (assert-contains stdout "Condition-system workflow")
  (assert-contains stdout "clpm repl-bridge debug")
  (assert-contains stdout "clpm skill"))
(format t "  `clpm skill` output PASSED~%")

(format t "Testing `clpm skill` rejects extra args...~%")
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("skill" "extra"))
  (assert-eql 1 code)
  (assert-true (string= "" stdout)
               "expected empty stdout, got:~%~A" stdout)
  (assert-contains stderr "Usage: clpm skill"))
(format t "  extra args rejection PASSED~%")

(format t "~%Skill command tests PASSED!~%")
(sb-ext:exit :code 0)
