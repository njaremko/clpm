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
  (assert-contains stdout "project")
  (assert-contains stdout "deps")
  (assert-contains stdout "registry")
  (assert-contains stdout "run")
  (assert-contains stdout "store")
  (assert-contains stdout "skill")
  (assert-contains stdout "repl")
  (assert-true (not (search "repl-bridge" stdout :test #'char-equal))
               "top-level help still advertises obsolete repl-bridge:~%~A"
               stdout))
(format t "  `--help` output PASSED~%")

(format t "Testing bare `clpm` (no args) prints help...~%")
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '())
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Commands:")
  (assert-contains stdout "Usage: clpm"))
(format t "  bare `clpm` PASSED~%")

(format t "Testing `clpm help project new` output...~%")
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "project" "new"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage:")
  (assert-contains stdout "clpm project new")
  (assert-contains stdout "--bin")
  (assert-contains stdout "--lib"))
(format t "  `clpm help project new` PASSED~%")

(format t "Testing `clpm help deps search` output...~%")
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "deps" "search"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage: clpm deps search"))
(format t "  `clpm help deps search` PASSED~%")

(format t "Testing `clpm help skill` output...~%")
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "skill"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage: clpm skill")
  (assert-contains stdout "SKILL.md"))
(format t "  `clpm help skill` PASSED~%")

(format t "Testing `<cmd> --help` output...~%")
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("project" "new" "--help"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "clpm project new"))
(format t "  `<cmd> --help` PASSED~%")

(format t "Testing per-subcommand help...~%")

;; workspace add: dedicated page with the add usage line.
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "project" "workspace" "add"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage: clpm project workspace add")
  (assert-true (not (search "workspace init" stdout))
               "workspace add help leaked the umbrella usage:~%~A" stdout))

;; workspace remove: dedicated page.
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "project" "workspace" "remove"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage: clpm project workspace remove"))

;; keys generate: focused on the generate subcommand.
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "registry" "key" "generate"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage: clpm registry key generate")
  (assert-true (not (search "clpm registry key verify" stdout))
               "keys generate help leaked the umbrella usage:~%~A" stdout))

;; scripts run: focused on the run subcommand.
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "run" "script"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage: clpm run script"))

;; registry trust set: leaf page (drills two levels).
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "registry" "trust" "set"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage: clpm registry trust set")
  (assert-contains stdout "none"))

;; repl umbrella: lists the three public commands.
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "repl"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "clpm repl daemon")
  (assert-contains stdout "clpm repl eval")
  (assert-contains stdout "clpm repl call"))

;; repl daemon: focused page.
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "repl" "daemon"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage: clpm repl daemon")
  (assert-contains stdout "--detach")
  (assert-true (not (search "clpm repl eval" stdout))
               "repl daemon help leaked the umbrella usage:~%~A" stdout))

;; repl eval: focused page, mentions --package and --no-autostart.
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "repl" "eval"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage: clpm repl eval")
  (assert-contains stdout "--package")
  (assert-contains stdout "--no-autostart"))

;; repl call: focused page.
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "repl" "call"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage: clpm repl call")
  (assert-contains stdout "--params-json"))

(format t "  Per-subcommand help PASSED~%")

(format t "Testing ordinary REPL surface is collapsed into repl...~%")
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "run"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-true (not (search "clpm run repl" stdout))
               "run help still advertises ordinary REPL:~%~A" stdout))
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "run" "repl"))
  (declare (ignore stdout))
  (assert-eql 1 code)
  (assert-contains stderr "clpm repl"))
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("run" "repl"))
  (declare (ignore stdout))
  (assert-eql 1 code)
  (assert-contains stderr "clpm repl"))
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "repl-bridge"))
  (declare (ignore stdout))
  (assert-eql 1 code)
  (assert-contains stderr "Unknown command"))
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("repl-bridge" "eval" "(+ 1 2)"))
  (declare (ignore stdout))
  (assert-eql 1 code)
  (assert-contains stderr "Unknown command"))
(format t "  Ordinary REPL collapse checks PASSED~%")

(format t "Testing unknown help target...~%")
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "does-not-exist"))
  (declare (ignore stdout))
  (assert-eql 1 code)
  (assert-contains stderr "Unknown command"))
(format t "  Unknown help target PASSED~%")

(format t "Testing removed top-level commands are not public...~%")
(dolist (cmd '("add" "install" "keys" "publish" "test" "gc" "repl-bridge"))
  (multiple-value-bind (code stdout stderr)
      (run-cli-captured (list cmd))
    (declare (ignore stdout))
    (assert-eql 1 code)
    (assert-contains stderr "Unknown command")))
(format t "  Removed top-level command checks PASSED~%")

(format t "~%Help output tests PASSED!~%")
(sb-ext:exit :code 0)
