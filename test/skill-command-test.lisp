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
  (assert-contains stdout "clpm deps sync")
  (assert-contains stdout "Strongly prefer the daemon-backed")
  (assert-contains stdout "Debug-first workflow")
  (assert-contains stdout "clpm repl eval '(error")
  (assert-contains stdout "terminal it starts a foreground project Lisp")
  (assert-contains stdout "clpm repl")
  (assert-contains stdout "clpm repl eval FORM [--package P] [--worker W] [--no-autostart] [--json]")
  (assert-contains stdout "clpm repl eval FORM [--package P] [--worker W] [--no-autostart] --debug [debug-options]")
  (assert-contains stdout "clpm repl call load-system --name my-app")
  (assert-contains stdout "clpm repl call test-system --name my-app")
  (assert-contains stdout "clpm repl call xref --symbol my-function --direction callers")
  (assert-true (not (search "--direction calls" stdout :test #'char-equal))
               "skill output still advertises invalid xref direction:~%~A"
               stdout)
  (assert-contains stdout "clpm repl call macroexpand --form '(my-macro x)' --recursive true")
  (assert-true (not (search "clpm repl call macroexpand --form '(my-macro x)' --full true" stdout :test #'char-equal))
               "skill output still advertises invalid macroexpand parameter:~%~A"
               stdout)
  (assert-contains stdout "clpm repl call watch --dir /absolute/path/to/src --glob '*.lisp' --auto-revert true")
  (assert-contains stdout "clpm repl call watch-system --name my-app --glob '*.lisp' --auto-revert true")
  (assert-true (not (search "clpm repl call watch --dir src" stdout :test #'char-equal))
               "skill output still advertises a relative watch directory:~%~A"
               stdout)
  (assert-contains stdout "clpm repl call list-watches")
  (assert-contains stdout "clpm repl call debug-abort --session 1")
  (assert-contains stdout "Do not leave kept debug sessions")
  (assert-contains stdout "scoped `-p <member>`")
  (assert-contains stdout "Scoped options must appear before the command token.")
  (assert-contains stdout "clpm skill")
  (assert-true (not (search "global `-p" stdout :test #'char-equal))
               "skill output still describes workspace targeting as global:~%~A"
               stdout)
  (assert-true (not (search "clpm run repl" stdout))
                "skill output still advertises ordinary REPL:~%~A"
                stdout)
  (assert-contains stdout "Do not use `clpm run exec -- sbcl ...` for iterative Lisp work")
  (assert-true (not (search "clpm run exec -- sbcl --script" stdout :test #'char-equal))
                "skill output still gives an sbcl run-exec example:~%~A"
                stdout)
  (assert-true (not (search "repl-bridge" stdout :test #'char-equal))
               "skill output still advertises obsolete repl-bridge:~%~A"
               stdout))
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
