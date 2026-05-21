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

(defun assert-not-contains (haystack needle fmt &rest args)
  (when (search needle haystack :test #'char-equal)
    (apply #'fail fmt args)))

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
  (assert-contains stdout "Usage: clpm [options] [command [args]]")
  (assert-contains stdout "Commands:")
  (assert-contains stdout "help [cmd ...]")
  (assert-contains stdout "doctor")
  (assert-contains stdout "project")
  (assert-contains stdout "deps")
  (assert-contains stdout "registry")
  (assert-contains stdout "run")
  (assert-contains stdout "store")
  (assert-contains stdout "skill")
  (assert-contains stdout "repl")
  (assert-contains stdout "Scoped options must appear before the command token.")
  (assert-contains stdout "Use `clpm help [command [subcommand ...]]`")
  (assert-not-contains stdout "ed25519:example-key-id"
                       "top-level help still uses fake trust key example:~%~A"
                       stdout)
  (assert-not-contains stdout "help [cmd]      "
                       "top-level help under-advertises nested help selectors:~%~A"
                       stdout)
  (assert-not-contains stdout "Usage: clpm [options] <command> [args]"
                       "top-level help still requires a command even though bare clpm is valid:~%~A"
                       stdout)
  (dolist (scoped-option '("-j, --jobs"
                           "--lisp"
                           "-p, --package"
                           "--offline"
                           "--insecure"
                           "--fetch-retries"
                           "--fetch-timeout"
                           "--with-optional"
                           "--with-all-optional"))
    (assert-not-contains stdout scoped-option
                         "top-level help still advertises scoped option ~S:~%~A"
                         scoped-option
                         stdout))
  (assert-not-contains stdout "repl-bridge"
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
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help"))
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

(format t "Testing `clpm help project` output...~%")
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "project"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "--workspace")
  (assert-contains stdout "--member-of <workspace-dir>")
  (assert-contains stdout "clpm project workspace init [--dir <path>]")
  (assert-contains stdout "clpm project workspace add <member> [--dir <path>]")
  (assert-contains stdout "clpm project workspace remove <member> [--dir <path>]")
  (assert-contains stdout "clpm project workspace list [--dir <path>]")
  (assert-not-contains stdout "clpm project workspace <init|add|remove|list> ..."
                       "project help still exposes workspace as a residual subgrammar:~%~A"
                       stdout))
(format t "  `clpm help project` PASSED~%")

(format t "Testing `clpm project` usage output...~%")
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("project"))
  (declare (ignore stdout))
  (assert-eql 1 code)
  (assert-contains stderr "clpm project workspace init [--dir <path>]")
  (assert-contains stderr "clpm project workspace add <member> [--dir <path>]")
  (assert-not-contains stderr "clpm project workspace <init|add|remove|list> ..."
                       "project arity error still exposes workspace as a residual subgrammar:~%~A"
                       stderr))
(format t "  `clpm project` usage PASSED~%")

(format t "Testing `clpm help deps search` output...~%")
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "deps" "search"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage: clpm deps search"))
(format t "  `clpm help deps search` PASSED~%")

(format t "Testing `clpm help help` output...~%")
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "help"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage: clpm help [command [subcommand ...]]")
  (assert-not-contains stdout "Usage: clpm help <command>"
                       "help help still requires a selector:~%~A"
                       stdout))
(format t "  `clpm help help` PASSED~%")

(format t "Testing scoped option help...~%")
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "deps" "sync"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "--offline")
  (assert-contains stdout "--jobs")
  (assert-contains stdout "--lisp")
  (assert-contains stdout "--fetch-retries")
  (assert-contains stdout "--with-optional")
  (assert-contains stdout "--insecure"))
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "registry" "update"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "--insecure")
  (assert-contains stdout "--fetch-retries"))
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "project" "package"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "--lisp"))
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "run"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "--lisp"))
(format t "  Scoped option help PASSED~%")

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

(format t "Testing command-local help aliases...~%")
(dolist (command '("project" "deps" "registry" "run" "store"))
  (multiple-value-bind (expected-code expected-stdout expected-stderr)
      (run-cli-captured (list "help" command))
    (declare (ignore expected-stderr))
    (assert-eql 0 expected-code)
    (multiple-value-bind (code stdout stderr)
        (run-cli-captured (list command "help"))
      (assert-eql 0 code)
      (assert-true (string= expected-stdout stdout)
                   "~A help diverged from clpm help ~A:~%expected:~%~A~%actual:~%~A"
                   command command expected-stdout stdout)
      (assert-true (string= "" stderr)
                   "~A help wrote to stderr:~%~A" command stderr))))
(format t "  command-local help aliases PASSED~%")

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

;; deps remove: leaf page uses the same system-target spelling as parser
;; errors and README.
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "deps" "remove"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage: clpm deps remove [--dev|--test] <system>")
  (assert-not-contains stdout "<dep>"
                       "deps remove help leaked alternate target name:~%~A"
                       stdout))

;; registry trust set: leaf page (drills two levels).
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "registry" "trust"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage:")
  (assert-contains stdout "  clpm registry trust list")
  (assert-contains stdout "  clpm registry trust set <name> <trust>")
  (assert-contains stdout "  clpm registry trust refresh <name>")
  (assert-not-contains stdout "[args]"
                       "registry trust umbrella help still exposes an untyped residual args slot:~%~A"
                       stdout))

(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "registry" "trust" "set"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage: clpm registry trust set")
  (assert-contains stdout "ed25519")
  (assert-true (not (search "none" stdout :test #'char-equal))
               "trust set help still advertises clearing trust:~%~A" stdout))

;; registry trust refresh: Quicklisp-only, not a maybe-no-op.
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "registry" "trust" "refresh"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage: clpm registry trust refresh")
  (assert-contains stdout "Only Quicklisp registries")
  (assert-contains stdout "--fetch-retries")
  (assert-true (not (search "no-op" stdout :test #'char-equal))
               "trust refresh help still describes an ambiguous no-op:~%~A"
               stdout))

;; registry update: snapshot update only, not trust refresh.
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "registry" "update"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage: clpm registry update")
  (assert-true (not (search "--refresh-trust" stdout :test #'char-equal))
               "registry update help still advertises trust refresh:~%~A" stdout))

;; registry umbrella help: exact leaf rows, no hidden option bucket.
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "registry"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage:")
  (assert-contains stdout "  clpm registry add --name <name> --url <git-url> --trust ed25519:<key-id>")
  (assert-contains stdout "  clpm registry trust refresh <name>")
  (assert-contains stdout "  clpm registry key verify --pub <path> --file <path> --sig <path>")
  (assert-not-contains stdout "Usage: clpm registry <add|list|update|trust|init|key|publish> [options]"
                       "registry help still exposes hidden subcommand/options grammar:~%~A"
                       stdout))

;; registry missing subcommand: same exact leaf rows.
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("registry"))
  (declare (ignore stdout))
  (assert-eql 1 code)
  (assert-contains stderr "Usage:")
  (assert-contains stderr "  clpm registry add --name <name> --url <git-url> --trust ed25519:<key-id>")
  (assert-contains stderr "  clpm registry trust refresh <name>")
  (assert-not-contains stderr "Usage: clpm registry <list|add|update|trust|init|key|publish> [options]"
                       "registry arity error still exposes hidden subcommand/options grammar:~%~A"
                       stderr))

;; registry trust missing subcommand: error text mirrors the same closed forms.
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("registry" "trust"))
  (declare (ignore stdout))
  (assert-eql 1 code)
  (assert-contains stderr "Usage:")
  (assert-contains stderr "  clpm registry trust list")
  (assert-contains stderr "  clpm registry trust set <name> <trust>")
  (assert-contains stderr "  clpm registry trust refresh <name>")
  (assert-not-contains stderr "[args]"
                       "registry trust arity error still exposes an untyped residual args slot:~%~A"
                       stderr))

;; registry add: Quicklisp add accepts explicit trust, so help must say so.
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "registry" "add"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage: clpm registry add")
  (assert-contains stdout "ed25519:<key-id>")
  (assert-contains stdout "--quicklisp")
  (assert-contains stdout "--trust tofu|sha256:<64-hex-digest>")
  (assert-not-contains stdout "ed25519:abcd"
                       "registry add help still uses fake trust key example:~%~A"
                       stdout))

;; registry publish: writes CLPM artifacts only, no VCS side effects.
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "registry" "publish"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage: clpm registry publish")
  (assert-true (not (search "--git-commit" stdout :test #'char-equal))
               "registry publish help still advertises git commits:~%~A" stdout))

;; repl umbrella: lists bare terminal/non-interactive default plus commands.
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "repl"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "clpm repl")
  (assert-contains stdout "stdin/stdout")
  (assert-contains stdout "detached project daemon")
  (assert-contains stdout "clpm repl daemon")
  (assert-contains stdout "[--status [--json]]")
  (assert-contains stdout "clpm repl eval <form>")
  (assert-contains stdout "[--json]")
  (assert-contains stdout "clpm repl call")
  (assert-contains stdout "public callable RPC")
  (assert-not-contains stdout "lists the RPC registry"
                       "repl help still suggests full internal RPC discovery:~%~A"
                       stdout))

;; repl daemon: focused page.
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "repl" "daemon"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage: clpm repl daemon")
  (assert-contains stdout "--detach")
  (assert-true (not (search "clpm repl eval" stdout))
               "repl daemon help leaked the umbrella usage:~%~A" stdout))

;; repl eval: focused page, mentions accepted debug selectors.
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "repl" "eval"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage: clpm repl eval")
  (assert-contains stdout "[--json]")
  (assert-contains stdout "--debug [debug-options]")
  (assert-contains stdout "--package")
  (assert-contains stdout "  --json")
  (assert-contains stdout "--handler T=R[:A,...]")
  (assert-contains stdout "--break-on")
  (assert-contains stdout "--timeout-ms")
  (assert-contains stdout "--no-autostart")
  (assert-not-contains stdout "[--debug] [--no-autostart] [--json]"
                       "repl eval help still complects debug and JSON modes:~%~A"
                       stdout))

;; repl call: focused page.
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("help" "repl" "call"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "Usage: clpm repl call")
  (assert-contains stdout "--params-json"))

;; repl resource help mirrors the nested help page.
(multiple-value-bind (code stdout stderr)
    (run-cli-captured '("repl" "--help"))
  (declare (ignore stderr))
  (assert-eql 0 code)
  (assert-contains stdout "clpm repl eval <form>")
  (assert-contains stdout "[--json]")
  (assert-contains stdout "--debug [debug-options]"))

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
(dolist (args '(("help" "deps" "bogus")
                ("help" "registry" "trust" "bogus")
                ("help" "registry" "trust" "set" "extra")
                ("help" "project" "workspace" "bogus")))
  (multiple-value-bind (code stdout stderr)
      (run-cli-captured args)
    (declare (ignore stdout))
    (assert-eql 1 code)
    (assert-contains stderr "Unknown help target")))
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
