;;;; test/cli-test.lisp - CLI behavior tests

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

(defun assert-string= (expected actual)
  (unless (string= expected actual)
    (fail "Assertion failed: expected ~S, got ~S" expected actual)))

(defun run-cli-captured (args)
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((*standard-output* out)
          (*error-output* err))
      (let ((code (clpm:run-cli args)))
        (values code
                (get-output-stream-string out)
                (get-output-stream-string err))))))

(format t "Testing --help...~%")
(assert-eql 0 (clpm:run-cli '("--help")))
(format t "  --help PASSED~%")

(format t "Testing bare clpm is help only...~%")
(multiple-value-bind (bare-code bare-out bare-err)
    (run-cli-captured '())
  (multiple-value-bind (help-code help-out help-err)
      (run-cli-captured '("help"))
    (assert-eql 0 bare-code)
    (assert-eql help-code bare-code)
    (assert-string= help-out bare-out)
    (assert-string= help-err bare-err)))
(format t "  Bare help PASSED~%")

(format t "Testing unknown command...~%")
(assert-eql 1 (clpm:run-cli '("unknown-command")))
(format t "  Unknown command PASSED~%")

(format t "Testing public command handler exports...~%")
(dolist (name '("CMD-PROJECT" "CMD-DEPS" "CMD-REGISTRY" "CMD-RUN"
                "CMD-STORE" "CMD-REPL" "CMD-SKILL" "CMD-HELP"
                "CMD-DOCTOR"))
  (multiple-value-bind (_symbol status)
      (find-symbol name "CLPM.COMMANDS")
    (declare (ignore _symbol))
    (assert-eql :external status)))
(dolist (name '("CMD-INIT" "CMD-NEW" "CMD-ADD" "CMD-REMOVE"
                "CMD-SEARCH" "CMD-INFO" "CMD-TREE" "CMD-WHY"
                "CMD-RESOLVE" "CMD-FETCH" "CMD-BUILD" "CMD-INSTALL"
                "CMD-UPDATE" "CMD-WORKSPACE" "CMD-EXEC" "CMD-TEST"
                "CMD-PACKAGE" "CMD-CLEAN" "CMD-GC" "CMD-SCRIPTS"
                "CMD-AUDIT" "CMD-SBOM" "CMD-KEYS" "CMD-PUBLISH"))
  (multiple-value-bind (_symbol status)
      (find-symbol name "CLPM.COMMANDS")
    (declare (ignore _symbol))
    (assert-eql :internal status)))
(format t "  Public handler exports PASSED~%")

(format t "Testing JSON option scope...~%")
(dolist (args '(("repl" "--json")
                ("--json" "repl")))
  (multiple-value-bind (code _out err)
      (run-cli-captured args)
    (declare (ignore _out))
    (assert-eql 1 code)
    (unless (search "Unknown subcommand: --json" err)
      (fail "Expected repl resource-level --json to be rejected, got: ~A" err))))
(format t "  JSON option scope PASSED~%")

(format t "Testing insecure option scope...~%")
(dolist (args '(("--insecure" "help")
                ("--insecure" "--help")
                ("--insecure" "--version")
                ("repl" "--insecure")))
  (multiple-value-bind (code _out err)
      (run-cli-captured args)
    (declare (ignore _out))
    (assert-eql 1 code)
    (unless (search "--insecure only applies" err)
      (fail "Expected inert --insecure to be rejected, got: ~A" err))))
(format t "  Insecure option scope PASSED~%")

(format t "Testing offline option scope...~%")
(dolist (args '(("--offline" "help")
                ("repl" "--offline")
                ("--offline" "deps" "sync" "--to" "lock")))
  (multiple-value-bind (code _out err)
      (run-cli-captured args)
    (declare (ignore _out))
    (assert-eql 1 code)
    (unless (search "--offline only applies" err)
      (fail "Expected inert --offline to be rejected, got: ~A" err))))
(format t "  Offline option scope PASSED~%")

(format t "Testing jobs option scope...~%")
(dolist (args '(("--jobs" "4" "help")
                ("-j" "4" "repl")
                ("--jobs" "2" "deps" "sync" "--to" "lock")))
  (multiple-value-bind (code _out err)
      (run-cli-captured args)
    (declare (ignore _out))
    (assert-eql 1 code)
    (unless (search "--jobs only applies" err)
      (fail "Expected inert --jobs to be rejected, got: ~A" err))))
(format t "  Jobs option scope PASSED~%")

(format t "Testing lisp option scope...~%")
(dolist (args '(("--lisp" "sbcl" "help")
                ("--lisp" "sbcl" "repl")
                ("--lisp" "sbcl" "deps" "sync" "--to" "source")))
  (multiple-value-bind (code _out err)
      (run-cli-captured args)
    (declare (ignore _out))
    (assert-eql 1 code)
    (unless (search "--lisp only applies" err)
      (fail "Expected inert --lisp to be rejected, got: ~A" err))))
(format t "  Lisp option scope PASSED~%")

(format t "Testing optional dependency option scope...~%")
(dolist (args '(("--with-optional" "foo" "help")
                ("--with-all-optional" "repl")))
  (multiple-value-bind (code _out err)
      (run-cli-captured args)
    (declare (ignore _out))
    (assert-eql 1 code)
    (unless (search "optional dependency flags only apply" err)
      (fail "Expected inert optional-dependency flag to be rejected, got: ~A" err))))
(format t "  Optional dependency option scope PASSED~%")

(format t "Testing run-program :timeout keyword...~%")
(multiple-value-bind (output error-output exit-code)
    (clpm.platform:run-program (list "sh" "-c" "exit 0") :timeout 1)
  (declare (ignore output error-output))
  (assert-eql 0 exit-code))
(format t "  run-program :timeout PASSED~%")

(format t "~%CLI tests PASSED!~%")
(sb-ext:exit :code 0)
