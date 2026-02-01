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
  init [name]      Initialize new project
  resolve          Resolve dependencies and write lockfile
  fetch            Download dependencies
  build            Build dependencies
  install          Resolve, fetch, and build (default)
  update [sys...]  Update dependencies
  repl             Start SBCL with project loaded
  run <script>     Run a project script
  gc               Garbage collect store

Options:
  -v, --verbose    Verbose output
  -j, --jobs N     Parallel jobs (default: 1)
  --offline        Fail if artifacts not in cache
  --insecure       Skip signature verification (dangerous)
  -h, --help       Show this help
  --version        Show version

Examples:
  clpm init myproject
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
        (i 0))
    (loop while (< i (length args)) do
      (let ((arg (nth i args)))
        (cond
          ;; Global options
          ((or (string= arg "-v") (string= arg "--verbose"))
           (push :verbose options))
          ((or (string= arg "-j") (string= arg "--jobs"))
           (incf i)
           (when (< i (length args))
             (let ((n (parse-integer (nth i args) :junk-allowed t)))
               (when n (push (cons :jobs n) options)))))
          ((string= arg "--offline")
           (push :offline options))
          ((string= arg "--insecure")
           (push :insecure options))
          ((or (string= arg "-h") (string= arg "--help"))
           (return-from parse-args (values :help nil nil)))
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

(defun main (&optional (args (uiop:command-line-arguments)))
  "Main entry point for CLPM."
  (handler-case
      (multiple-value-bind (command command-args options)
          (parse-args args)
        (apply-options options)
        ;; Bind command module variables
        (let ((clpm.commands:*verbose* *verbose*)
              (clpm.commands:*offline* *offline*)
              (clpm.commands:*insecure* *insecure*)
              (clpm.commands:*jobs* *jobs*))
          ;; Dispatch command
          (let ((exit-code
                  (case command
                    (:help
                     (print-usage)
                     0)
                    (:version
                     (print-version)
                     0)
                    (:init
                     (clpm.commands:cmd-init
                      :name (first command-args)))
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
                    (:gc
                     (clpm.commands:cmd-gc
                      :dry-run (member "--dry-run" command-args
                                       :test #'string=)))
                    (t
                     (format *error-output* "Unknown command: ~A~%" command)
                     (print-usage)
                     1))))
            (sb-ext:exit :code exit-code))))
    (error (c)
      (format *error-output* "~&Fatal error: ~A~%" c)
      (when *verbose*
        (format *error-output* "~&Backtrace:~%")
        (sb-debug:print-backtrace :stream *error-output* :count 20))
      (sb-ext:exit :code 1))))

;;; Build standalone executable

(defun build-executable (output-path)
  "Build standalone CLPM executable."
  (sb-ext:save-lisp-and-die
   output-path
   :toplevel #'main
   :executable t
   :compression t))
