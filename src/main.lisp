;;;; main.lisp - Main entry point and CLI parsing

(in-package #:clpm)

;;; Version

(defparameter *version* "0.1.0")

;;; Global options

(defvar *verbose* nil)
(defvar *offline* nil)
(defvar *insecure* nil)
(defvar *jobs* 1)

;;; Usage text

(defun print-usage ()
  "Print usage information."
  (format t "~
clpm ~A - Common Lisp Package Manager

Usage: clpm [options] <command> [args]

Commands:
  help [cmd]      Show help for a command
  doctor          Check your environment
  new <name> ...   Create a new project
  init [name]      Initialize new project
  add <dep> ...    Add a dependency
  remove <dep>     Remove a dependency
  search <query>   Search registries
  info <system>    Show system details
  tree             Show dependency tree
  why <system>     Explain why a system is included
  registry ...     Manage registries
  resolve          Resolve dependencies and write lockfile
  fetch            Download dependencies
  build            Build dependencies
  install          Resolve, fetch, and build (default)
  update [sys...]  Update dependencies
  repl             Start SBCL with project loaded
  run              Run the project entrypoint
  exec -- <cmd...> Run a command in the project env
  test             Run project tests
  package          Build a distributable executable
  clean            Remove project-local outputs
  gc               Garbage collect store
  scripts ...      Run project scripts
  audit            Show provenance report
  sbom             Generate an SBOM for the lockfile
  keys ...         Key management (registry signing)
  publish ...      Publish to a registry
  workspace ...    Workspace management

Options:
  -v, --verbose    Verbose output
  -j, --jobs N     Parallel jobs (default: 1)
  --lisp <impl>    Lisp implementation (sbcl|ccl|ecl)
  -p, --package M  Workspace member to target
  --offline        Fail if artifacts not in cache
  --insecure       Skip signature verification (dangerous)
  -h, --help       Show this help
  --version        Show version

Examples:
  clpm doctor
  clpm registry add --name main --url https://example.invalid/registry.git --trust ed25519:example-key-id
  clpm new myproject --bin
  clpm init myproject
  clpm add alexandria@^1.4.0
  clpm install
  clpm repl
  clpm update alexandria
" *version*))

(defun print-version ()
  "Print version information."
  (format t "clpm ~A~%" *version*))

;;; Argument parsing

(defun parse-args (args)
  "Parse command line arguments.
Returns (values command command-args options)."
  (let ((command nil)
        (command-args '())
        (options '())
        (end-of-options nil)
        (i 0))
    (loop while (< i (length args)) do
      (let ((arg (nth i args)))
        (cond
          (end-of-options
           (push arg command-args))
          ((and command (string= arg "--"))
           ;; After a command is chosen, treat `--` as an "end of options"
           ;; sentinel for forwarding args verbatim (e.g. `clpm exec -- <cmd...>`).
           (setf end-of-options t)
           (push arg command-args))
          ;; Global options
          ((or (string= arg "-v") (string= arg "--verbose"))
           (push :verbose options))
          ((or (string= arg "-j") (string= arg "--jobs"))
           (incf i)
           (when (>= i (length args))
             (clpm.errors:signal-error 'clpm.errors:clpm-user-error
                                       "Missing value for ~A" arg))
           (let* ((raw (nth i args))
                  (n (ignore-errors (parse-integer raw :junk-allowed nil))))
             (unless (and (integerp n) (plusp n))
               (clpm.errors:signal-error 'clpm.errors:clpm-user-error
                                         "Invalid value for --jobs: ~A" raw))
             (push (cons :jobs n) options)))
          ((string= arg "--lisp")
           (incf i)
           (when (>= i (length args))
             (clpm.errors:signal-error 'clpm.errors:clpm-user-error
                                       "Missing value for ~A" arg))
           (when (some (lambda (opt)
                         (and (consp opt) (eq (car opt) :lisp)))
                       options)
             (clpm.errors:signal-error 'clpm.errors:clpm-user-error
                                       "Duplicate option: ~A" arg))
           (let* ((raw (nth i args))
                  (kind (clpm.lisp:parse-lisp-kind raw)))
             (push (cons :lisp kind) options)))
          ((or (string= arg "-p") (string= arg "--package"))
           (incf i)
           (when (>= i (length args))
             (clpm.errors:signal-error 'clpm.errors:clpm-user-error
                                       "Missing value for ~A" arg))
           (when (some (lambda (opt)
                         (and (consp opt) (eq (car opt) :package)))
                       options)
             (clpm.errors:signal-error 'clpm.errors:clpm-user-error
                                       "Duplicate option: ~A" arg))
           (let ((raw (nth i args)))
             (unless (and (stringp raw) (plusp (length raw)))
               (clpm.errors:signal-error 'clpm.errors:clpm-user-error
                                         "Invalid value for --package: ~S" raw))
             (push (cons :package raw) options)))
          ((string= arg "--offline")
           (push :offline options))
          ((string= arg "--insecure")
           (push :insecure options))
          ((or (string= arg "-h") (string= arg "--help"))
           (if command
               (return-from parse-args
                 (values :help
                         (list (string-downcase (symbol-name command)))
                         options))
               (return-from parse-args (values :help nil nil))))
          ((string= arg "--version")
           (return-from parse-args (values :version nil nil)))
          ;; Command
          ((and (null command) (not (char= (char arg 0) #\-)))
           (setf command (intern (string-upcase arg) :keyword)))
          ;; Command args
          (t
           (push arg command-args))))
      (incf i))
    (values (or command :install)
            (nreverse command-args)
            options)))

(defun apply-options (options)
  "Apply parsed options to global variables."
  (dolist (opt options)
    (cond
      ((eq opt :verbose) (setf *verbose* t))
      ((eq opt :offline) (setf *offline* t))
      ((eq opt :insecure) (setf *insecure* t))
      ((and (consp opt) (eq (car opt) :jobs))
       (setf *jobs* (cdr opt))))))

;;; Main entry point

(defun run-cli (args)
  "Run CLPM with ARGS and return an integer exit code.

This function must not call `sb-ext:exit` so it can be used from tests."
  (handler-case
      (multiple-value-bind (command command-args options)
          (parse-args args)
        (apply-options options)
        ;; Bind command module variables
        (let ((clpm.commands:*verbose* *verbose*)
              (clpm.commands:*offline* *offline*)
              (clpm.commands:*insecure* *insecure*)
              (clpm.commands:*jobs* *jobs*)
              (clpm.commands:*lisp*
                (loop for opt in options
                      when (and (consp opt) (eq (car opt) :lisp))
                        do (return (cdr opt))
                      finally (return nil)))
              (clpm.commands:*target-package*
                (loop for opt in options
                      when (and (consp opt) (eq (car opt) :package))
                        do (return (cdr opt))
                      finally (return nil))))
          ;; Dispatch command
          (case command
            (:help
             (if command-args
                 (apply #'clpm.commands:cmd-help command-args)
                 (progn
                   (print-usage)
                   0)))
            (:version
             (print-version)
             0)
            (:doctor
             (clpm.commands:cmd-doctor))
            (:init
             (clpm.commands:cmd-init
              :name (first command-args)))
            (:new
             (apply #'clpm.commands:cmd-new command-args))
            (:add
             (apply #'clpm.commands:cmd-add command-args))
            (:remove
             (apply #'clpm.commands:cmd-remove command-args))
            (:search
             (apply #'clpm.commands:cmd-search command-args))
            (:info
             (apply #'clpm.commands:cmd-info command-args))
            (:tree
             (apply #'clpm.commands:cmd-tree command-args))
            (:why
             (apply #'clpm.commands:cmd-why command-args))
            (:registry
             (apply #'clpm.commands:cmd-registry command-args))
            (:resolve
             (clpm.commands:cmd-resolve))
            (:fetch
             (clpm.commands:cmd-fetch))
            (:build
             (clpm.commands:cmd-build))
            (:install
             (clpm.commands:cmd-install))
            (:update
             (apply #'clpm.commands:cmd-update command-args))
            (:repl
             (clpm.commands:cmd-repl
              :load-system (first command-args)))
            (:run
             (apply #'clpm.commands:cmd-run command-args))
            (:exec
             (apply #'clpm.commands:cmd-exec command-args))
            (:test
             (apply #'clpm.commands:cmd-test command-args))
            (:package
             (apply #'clpm.commands:cmd-package command-args))
            (:clean
             (apply #'clpm.commands:cmd-clean command-args))
            (:gc
             (clpm.commands:cmd-gc
              :dry-run (member "--dry-run" command-args
                               :test #'string=)))
            (:scripts
             (apply #'clpm.commands:cmd-scripts command-args))
            (t
             (format *error-output* "Unknown command: ~A~%" command)
             (print-usage)
             1))))
    (clpm.errors:clpm-error (c)
      (clpm.errors:format-error c)
      (cond
        ((typep c 'clpm.errors:clpm-resolve-error) 2)
        (t 1)))
    (error (c)
      (format *error-output* "~&internal error: ~A~%" c)
      (when *verbose*
        (format *error-output* "~&Backtrace:~%")
        (sb-debug:print-backtrace :stream *error-output* :count 20))
      1)))

(defun main (&optional (args (uiop:command-line-arguments)))
  "Main entry point for CLPM."
  (sb-ext:exit :code (run-cli args)))

;;; Build standalone executable

(defun build-executable (output-path)
  "Build standalone CLPM executable."
  (sb-ext:save-lisp-and-die
   output-path
   :toplevel #'main
   :executable t
   :compression t))
