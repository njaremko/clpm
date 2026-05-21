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
  help [cmd ...]  Show help for a command or subcommand
  doctor          Check your environment
  project ...      Create projects, workspaces, and packages
  deps ...         Manage, realize, and inspect dependencies
  registry ...     Manage registries, keys, trust, and publishing
  run ...          Run entrypoints, tests, scripts, or commands
  store ...        Clean project outputs and garbage collect the store
  skill            Print an agent SKILL.md for using clpm
  repl ...         Persistent Lisp image (for LLM-driven dev)

Options:
  -v, --verbose    Verbose output
  -h, --help       Show this help
  --version        Show version

Scoped options must appear before the command token.
Command-scoped options are documented on the commands that accept them.
Use `clpm help <command> [subcommand ...]` before passing scoped options.

Examples:
  clpm doctor
  clpm registry add --name main --url https://example.invalid/registry.git --trust ed25519:<key-id>
  clpm project new myproject --bin
  clpm project init myproject
  clpm deps add alexandria bordeaux-threads
  clpm deps sync
  clpm repl eval '(asdf:load-system \"myproject\")'
  clpm deps update alexandria
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
           ;; sentinel for forwarding args verbatim (e.g. `clpm run exec -- <cmd...>`).
           (setf end-of-options t)
           (push arg command-args))
          ;; Global options
          ((and (null command)
                (or (string= arg "-v") (string= arg "--verbose")))
           (push :verbose options))
          ((and (null command)
                (or (string= arg "-j") (string= arg "--jobs")))
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
          ((and (null command)
                (string= arg "--lisp"))
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
          ;; Workspace-member target: `-p` / `--package`. Only consumed as a
          ;; global *before* the command, so subcommands are free to define
          ;; their own `--package` flag (the repl introspection commands
          ;; mean it as a Common Lisp package).
          ((and (null command)
                (or (string= arg "-p") (string= arg "--package")))
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
          ((and (null command)
                (string= arg "--offline"))
           (push :offline options))
          ((and (null command)
                (string= arg "--insecure"))
           (push :insecure options))
          ((and (null command)
                (string= arg "--with-optional"))
           (incf i)
           (when (>= i (length args))
             (clpm.errors:signal-error 'clpm.errors:clpm-user-error
                                       "Missing value for ~A" arg))
           (let ((raw (nth i args)))
             (unless (and (stringp raw) (plusp (length raw)))
               (clpm.errors:signal-error 'clpm.errors:clpm-user-error
                                         "Invalid value for --with-optional: ~S" raw))
             (push (cons :with-optional raw) options)))
          ((and (null command)
                (string= arg "--with-all-optional"))
           (push (cons :with-optional :all) options))
          ((and (null command)
                (string= arg "--fetch-retries"))
           (incf i)
           (when (>= i (length args))
             (clpm.errors:signal-error 'clpm.errors:clpm-user-error
                                       "Missing value for ~A" arg))
           (let* ((raw (nth i args))
                  (n (ignore-errors (parse-integer raw :junk-allowed nil))))
             (unless (and (integerp n) (plusp n))
               (clpm.errors:signal-error 'clpm.errors:clpm-user-error
                                         "Invalid value for --fetch-retries: ~A" raw))
             (push (cons :fetch-retries n) options)))
          ((and (null command)
                (string= arg "--fetch-timeout"))
           (incf i)
           (when (>= i (length args))
             (clpm.errors:signal-error 'clpm.errors:clpm-user-error
                                       "Missing value for ~A" arg))
           (let* ((raw (nth i args))
                  (n (ignore-errors (parse-integer raw :junk-allowed nil))))
             (unless (and (integerp n) (plusp n))
               (clpm.errors:signal-error 'clpm.errors:clpm-user-error
                                         "Invalid value for --fetch-timeout: ~A" raw))
             (push (cons :fetch-timeout n) options)))
          ((or (string= arg "-h") (string= arg "--help"))
           (when (< (1+ i) (length args))
             (clpm.errors:signal-error
              'clpm.errors:clpm-user-error
              "Unexpected argument after ~A: ~A" arg (nth (1+ i) args)))
           (if command
               (return-from parse-args
                 (values :help
                         (cons (string-downcase (symbol-name command))
                               (nreverse command-args))
                         options))
               (return-from parse-args (values :help nil options))))
          ((and (null command) (string= arg "--version"))
           (when (< (1+ i) (length args))
             (clpm.errors:signal-error
              'clpm.errors:clpm-user-error
              "Unexpected argument after --version: ~A" (nth (1+ i) args)))
           (return-from parse-args (values :version nil options)))
          ((and (null command)
                (plusp (length arg))
                (char= (char arg 0) #\-))
           (clpm.errors:signal-error 'clpm.errors:clpm-user-error
                                     "Unknown option: ~A" arg))
          ;; Command
          ((and (null command)
                (plusp (length arg))
                (not (char= (char arg 0) #\-)))
           (setf command (intern (string-upcase arg) :keyword)))
          ;; Command args
          (t
           (push arg command-args))))
      (incf i))
    (if command
        (values command (nreverse command-args) options)
        (values :help nil options))))

(defun option-present-p (option options)
  "Return true when OPTION is present in parsed OPTIONS."
  (some (lambda (opt)
          (if (consp opt)
              (eq (car opt) option)
              (eq opt option)))
        options))

(defun registry-verification-command-p (command command-args)
  "Return true when COMMAND may load signed registry data."
  (case command
    (:deps
     (let ((subcommand (first command-args)))
       (and (stringp subcommand)
            (member subcommand '("sync" "update" "search" "info")
                    :test #'string=))))
    (:registry
     (let ((subcommand (first command-args)))
       (and (stringp subcommand)
            (string= subcommand "update"))))
    (t nil)))

(defun sync-stage-selectors (command-args)
  "Return every raw value passed to deps sync --to, in command order."
  (let ((stages '()))
    (loop for rest on command-args
          for arg = (first rest)
          when (and (stringp arg) (string= arg "--to"))
            do (push (second rest) stages))
    (nreverse stages)))

(defun sync-stage-option (command-args)
  "Return the raw value passed to deps sync --to, or NIL when absent."
  (first (sync-stage-selectors command-args)))

(defun duplicate-sync-stage-p (command command-args)
  "Return true when deps sync receives more than one --to selector."
  (and (eq command :deps)
       (stringp (first command-args))
       (string= (first command-args) "sync")
       (> (length (sync-stage-selectors command-args)) 1)))

(defun sync-lock-stage-p (command-args)
  "Return true when dependency sync explicitly targets only the lock stage."
  (let ((stage (sync-stage-option command-args)))
    (and (stringp stage) (string= stage "lock"))))

(defun sync-build-stage-p (command-args)
  "Return true when dependency sync may build with a selected Lisp."
  (let ((stage (sync-stage-option command-args)))
    (cond
      ((null stage) t)
      ((member stage '("build" "active") :test #'string=) t)
      ((member stage '("lock" "source") :test #'string=) nil)
      (t t))))

(defun artifact-cache-command-p (command command-args)
  "Return true when COMMAND may consult artifact cache/offline realization."
  (case command
    (:deps
     (let ((subcommand (first command-args)))
       (cond
         ((not (stringp subcommand)) nil)
         ((string= subcommand "sync")
          (not (sync-lock-stage-p command-args)))
         ((string= subcommand "sbom") t)
         (t nil))))
    (t nil)))

(defun lisp-selection-command-p (command command-args)
  "Return true when COMMAND may choose a Lisp implementation."
  (case command
    (:deps
     (let ((subcommand (first command-args)))
       (and (stringp subcommand)
            (string= subcommand "sync")
            (sync-build-stage-p command-args))))
    (:project
     (let ((subcommand (first command-args)))
       (and (stringp subcommand)
            (string= subcommand "package"))))
    (:run
     (let ((subcommand (first command-args)))
       (cond
         ((null subcommand) t)
         ((string= subcommand "--") t)
         ((member subcommand '("exec" "scripts" "help" "--help" "repl")
                  :test #'string=)
          nil)
         (t t))))
    (t nil)))

(defun optional-dependency-command-p (command command-args)
  "Return true when COMMAND may change optional dependency resolution."
  (case command
    (:deps
     (let ((subcommand (first command-args)))
       (and (stringp subcommand)
            (member subcommand '("sync" "update") :test #'string=))))
    (t nil)))

(defun fetch-tuning-command-p (command command-args)
  "Return true when COMMAND may perform CLPM-managed HTTP fetches."
  (case command
    (:deps
     (let ((subcommand (first command-args)))
       (and (stringp subcommand)
            (member subcommand '("sync" "update" "search" "info" "sbom")
                    :test #'string=))))
    (:registry
     (let ((subcommand (first command-args))
           (nested (second command-args)))
       (or (and (stringp subcommand)
                (string= subcommand "update"))
           (and (stringp subcommand)
                (string= subcommand "trust")
                (stringp nested)
                (string= nested "refresh")))))
    (t nil)))

(defun parallel-realization-command-p (command command-args)
  "Return true when COMMAND may use the parallel dependency job budget."
  (case command
    (:deps
     (let ((subcommand (first command-args)))
       (and (stringp subcommand)
            (string= subcommand "sync")
            (not (sync-lock-stage-p command-args)))))
    (t nil)))

(defun workspace-target-command-p (command command-args)
  "Return true when COMMAND may resolve a workspace member target."
  (let ((subcommand (first command-args)))
    (case command
      (:project
       (and (stringp subcommand)
            (string= subcommand "package")))
      (:deps
       (and (stringp subcommand)
            (member subcommand
                    '("add" "remove" "sync" "update" "search" "info"
                      "tree" "why" "audit" "sbom")
                    :test #'string=)))
      (:registry
       (and (stringp subcommand)
            (string= subcommand "publish")))
      (:run
       (or (null subcommand)
           (string= subcommand "--")
           (member subcommand
                   '("exec" "test" "script" "scripts")
                   :test #'string=)))
      (:repl
       (and (stringp subcommand)
            (member subcommand '("daemon" "eval" "call")
                    :test #'string=)))
      (:store
       (and (stringp subcommand)
            (string= subcommand "clean")))
      (t nil))))

(defun validate-option-scope (command command-args options)
  "Reject global options that have no denotation for COMMAND."
  (when (duplicate-sync-stage-p command command-args)
    (clpm.errors:signal-error
     'clpm.errors:clpm-user-error
     "Duplicate option: --to"))
  (when (and (option-present-p :package options)
             (not (workspace-target-command-p command command-args)))
    (clpm.errors:signal-error
     'clpm.errors:clpm-user-error
     "workspace member target only applies to project-scoped commands: clpm project package, clpm deps ..., clpm registry publish, clpm run ..., clpm repl ..., or clpm store clean"))
  (when (and (option-present-p :insecure options)
             (not (registry-verification-command-p command command-args)))
    (clpm.errors:signal-error
     'clpm.errors:clpm-user-error
     "--insecure only applies to commands that load signed registry data: clpm deps sync, clpm deps update, clpm deps search, clpm deps info, or clpm registry update"))
  (when (and (option-present-p :offline options)
             (not (artifact-cache-command-p command command-args)))
    (clpm.errors:signal-error
     'clpm.errors:clpm-user-error
     "--offline only applies to artifact/cache commands: clpm deps sync --to source|build|active, clpm deps sync, or clpm deps sbom"))
  (when (and (option-present-p :jobs options)
             (not (parallel-realization-command-p command command-args)))
    (clpm.errors:signal-error
     'clpm.errors:clpm-user-error
     "--jobs only applies to parallel dependency realization: clpm deps sync --to source|build|active or clpm deps sync"))
  (when (and (option-present-p :lisp options)
             (not (lisp-selection-command-p command command-args)))
    (clpm.errors:signal-error
     'clpm.errors:clpm-user-error
     "--lisp only applies where CLPM selects a Lisp implementation: clpm deps sync --to build|active, clpm deps sync, clpm project package, or clpm run ..."))
  (when (and (option-present-p :with-optional options)
             (not (optional-dependency-command-p command command-args)))
    (clpm.errors:signal-error
     'clpm.errors:clpm-user-error
     "optional dependency flags only apply to dependency resolution: clpm deps sync or clpm deps update"))
  (when (and (or (option-present-p :fetch-retries options)
                 (option-present-p :fetch-timeout options))
             (not (fetch-tuning-command-p command command-args)))
    (clpm.errors:signal-error
     'clpm.errors:clpm-user-error
     "fetch tuning only applies to CLPM-managed fetches: clpm deps sync/update/search/info/sbom, clpm registry update, or clpm registry trust refresh")))

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
  (let ((*verbose* nil)
        (*offline* nil)
        (*insecure* nil)
        (*jobs* 1))
    (handler-case
        (multiple-value-bind (command command-args options)
            (parse-args args)
          (validate-option-scope command command-args options)
          (apply-options options)
        ;; Bind command module variables
        (let ((clpm.commands::*verbose* *verbose*)
              (clpm.commands::*offline* *offline*)
              (clpm.commands::*insecure* *insecure*)
              (clpm.commands::*jobs* *jobs*)
              (clpm.commands::*lisp*
                (loop for opt in options
                      when (and (consp opt) (eq (car opt) :lisp))
                        do (return (cdr opt))
                      finally (return nil)))
              (clpm.commands::*target-package*
                (loop for opt in options
                      when (and (consp opt) (eq (car opt) :package))
                        do (return (cdr opt))
                      finally (return nil)))
              (clpm.commands::*with-optional*
                (let ((vals (loop for opt in options
                                  when (and (consp opt) (eq (car opt) :with-optional))
                                    collect (cdr opt))))
                  (cond
                    ((null vals) nil)
                    ((member :all vals :test #'eq) :all)
                    (t (remove-duplicates vals :test #'string=)))))
              (clpm.fetch:*fetch-retries*
                (loop for opt in options
                      when (and (consp opt) (eq (car opt) :fetch-retries))
                        do (return (cdr opt))
                      finally (return clpm.fetch:*fetch-retries*)))
              (clpm.fetch:*fetch-timeout*
                (loop for opt in options
                      when (and (consp opt) (eq (car opt) :fetch-timeout))
                        do (return (cdr opt))
                      finally (return clpm.fetch:*fetch-timeout*))))
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
             (cond
               (command-args
                (format *error-output* "Usage: clpm doctor~%")
                1)
               (t
                (clpm.commands:cmd-doctor))))
            (:project
             (apply #'clpm.commands:cmd-project command-args))
            (:deps
             (apply #'clpm.commands:cmd-deps command-args))
            (:registry
             (apply #'clpm.commands:cmd-registry command-args))
            (:run
             (apply #'clpm.commands:cmd-run command-args))
            (:store
             (apply #'clpm.commands:cmd-store command-args))
            (:skill
             (apply #'clpm.commands:cmd-skill command-args))
            (:repl
             (apply #'clpm.commands:cmd-repl command-args))
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
      1))))

(defun main (&optional (args (uiop:command-line-arguments)))
  "Main entry point for CLPM."
  (sb-ext:exit :code (run-cli args)))

;;; Build standalone executable

(defun %chmod-755 (path)
  (let ((path (namestring (uiop:ensure-pathname path :want-existing nil))))
    (when (uiop:os-windows-p)
      (return-from %chmod-755 t))
    (handler-case
        (let ((proc (sb-ext:run-program "chmod" (list "+x" path)
                                        :search t
                                        :output nil
                                        :error nil
                                        :wait t)))
          (zerop (sb-ext:process-exit-code proc)))
      (error ()
        nil))))

(defun %write-sbcl-wrapper-script (wrapper-path bin-basename)
  "Write a small POSIX sh wrapper that runs BIN-BASENAME with SBCL runtime
option processing disabled so user flags like --help/--version reach Lisp."
  (let ((wrapper-path (uiop:ensure-pathname wrapper-path :want-existing nil :want-file t)))
    (ensure-directories-exist wrapper-path)
    (with-open-file (s wrapper-path :direction :output
                            :if-exists :supersede
                            :external-format :utf-8)
      (write-line "#!/bin/sh" s)
      (write-line "set -e" s)
      (format s "exec \"$(dirname \"$0\")/~A\" --end-runtime-options \"$@\"~%" bin-basename))
    (unless (%chmod-755 wrapper-path)
      (error "Failed to mark wrapper executable: ~A" (namestring wrapper-path)))))

(defun build-executable (output-path)
  "Build a standalone CLPM executable.

On POSIX, this produces:
- OUTPUT-PATH: a tiny sh wrapper that forwards args to OUTPUT-PATH.bin with
  `--end-runtime-options` so SBCL doesn't consume flags like --help/--version.
- OUTPUT-PATH.bin: the actual SBCL-based executable."
  ;; Ensure common SBCL contribs that UIOP/ASDF may use are embedded in the
  ;; saved image, so the resulting executable doesn't depend on SBCL_HOME to
  ;; locate them at runtime.
  (ignore-errors (require :sb-posix))
  (let* ((output-path (uiop:ensure-pathname output-path :want-existing nil :want-file t))
         (bin-path (uiop:ensure-pathname (format nil "~A.bin" (namestring output-path))
                                         :want-existing nil :want-file t)))
    (if (uiop:os-windows-p)
        (sb-ext:save-lisp-and-die
         (namestring output-path)
         :toplevel #'main
         :executable t
         :compression t)
        (progn
          (%write-sbcl-wrapper-script output-path (file-namestring bin-path))
          (sb-ext:save-lisp-and-die
           (namestring bin-path)
           :toplevel #'main
           :executable t
           :compression t)))))
