;;;; test/repl-cli-subcommands-test.lisp -- end-to-end test for the
;;;; small repl CLI algebra: daemon, eval, and call.

(require :asdf)
(require :sb-posix)

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

(defvar *last-cli-args* nil)
(defvar *last-cli-stdout* nil)
(defvar *last-cli-stderr* nil)

(defun assert-eql (expected actual)
  (unless (eql expected actual)
    (fail "expected ~S, got ~S~@[~%args: ~S~]~@[~%stdout:~%~A~]~@[~%stderr:~%~A~]"
          expected actual *last-cli-args* *last-cli-stdout* *last-cli-stderr*)))

(defun assert-true (x fmt &rest args)
  (unless x (apply #'fail fmt args)))

(defun assert-contains (haystack needle)
  (unless (and (stringp haystack) (search needle haystack))
    (fail "expected ~S in:~%~A" needle haystack)))

(defun assert-not-contains (haystack needle)
  (when (and (stringp haystack) (search needle haystack))
    (fail "did not expect ~S in:~%~A" needle haystack)))

(defun run-cli-captured (args)
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((*standard-output* out) (*error-output* err))
      (let ((rc (clpm:run-cli args)))
        (setf *last-cli-args* args
              *last-cli-stdout* (get-output-stream-string out)
              *last-cli-stderr* (get-output-stream-string err))
        (values rc
                *last-cli-stdout*
                *last-cli-stderr*)))))

(defun make-short-temp-dir ()
  "Create a short temp directory so Unix-domain socket paths stay portable."
  (let* ((template (namestring
                    (merge-pathnames "rbXXXXXX" (uiop:temporary-directory))))
         (dir (sb-posix:mkdtemp template)))
    (uiop:ensure-directory-pathname dir)))

(defmacro with-short-temp-dir ((var) &body body)
  `(let ((,var (make-short-temp-dir)))
     (unwind-protect
          (progn ,@body)
       (ignore-errors
         (uiop:delete-directory-tree ,var :validate t)))))

(with-short-temp-dir (tmp)
  (let* ((proj (merge-pathnames "myproj/" tmp))
         (manifest (merge-pathnames "clpm.project" proj))
         (clpm-dir (merge-pathnames ".clpm/" proj))
         (asdf-config (merge-pathnames "asdf-config.lisp" clpm-dir)))
    (ensure-directories-exist clpm-dir)
    (with-open-file (s manifest :direction :output :if-exists :supersede
                                :external-format :utf-8)
      (write-string "(:project :name \"myproj\" :version \"0.1.0\" :systems () :registries ())" s))
    (with-open-file (s asdf-config :direction :output :if-exists :supersede
                                   :external-format :utf-8)
      (write-string ";; empty for test~%" s))

    (uiop:with-current-directory (proj)
      (let ((srv (sb-thread:make-thread
                  (lambda ()
                    (handler-case (run-cli-captured '("repl" "daemon"))
                      (error (c)
                        (format *error-output* "daemon thread died: ~A~%" c)
                        (force-output *error-output*))))
                  :name "test-cli-small-daemon"))
            (sock (namestring (merge-pathnames ".clpm/repl.sock" proj))))
        (declare (ignorable srv))
        (sleep 0.05)
        (unwind-protect
             (progn
               (loop for i from 0 below 100
                     while (not (probe-file sock))
                     do (sleep 0.1))
               (assert-true (probe-file sock)
                            "daemon socket did not appear: ~A" sock)
               (format t "  daemon up~%")

               (format t "Test: call methods + help expose registry schema~%")
	               (multiple-value-bind (rc stdout)
	                   (run-cli-captured '("repl" "call" "methods"))
	                 (assert-eql 0 rc)
	                 (assert-contains stdout "\"methods\"")
	                 (assert-true (not (search "\"eval\"" stdout))
	                              "methods should not advertise eval: ~A"
	                              stdout))
	               (multiple-value-bind (rc stdout)
	                   (run-cli-captured '("repl" "call" "help"
	                                       "--method" "gc"))
	                 (assert-eql 0 rc)
	                 (assert-contains stdout "\"params\"")
	                 (assert-contains stdout "\"full\""))
	               (multiple-value-bind (rc stdout)
	                   (run-cli-captured
	                    '("repl" "call" "help"
	                      "--params-json" "{\"method\":\"gc\"}"))
	                 (assert-eql 0 rc)
	                 (assert-contains stdout "\"params\"")
	                 (assert-contains stdout "\"full\""))
	               (multiple-value-bind (rc stdout)
	                   (run-cli-captured '("repl" "call" "help"
	                                       "--method" "eval"))
	                 (assert-eql 1 rc)
	                 (assert-contains stdout "\"error\"")
	                 (assert-contains stdout "unknown method"))
	               (format t "  registry discovery OK~%")

               (format t "Test: call does not alias eval~%")
               (multiple-value-bind (rc _stdout stderr)
                   (run-cli-captured '("repl" "call" "eval"
                                       "--form" "(+ 1 2)"))
                 (declare (ignore _stdout))
                 (assert-eql 1 rc)
                 (assert-contains stderr "Use `clpm repl eval FORM`"))
               (format t "  eval alias rejection OK~%")

               (format t "Test: call params are checked by method schema~%")
               (multiple-value-bind (rc stdout stderr)
                   (run-cli-captured '("repl" "call" "help"
                                       "--method" "eval"
                                       "--bogus" "true"))
                 (declare (ignore stderr))
                 (assert-eql 1 rc)
                 (assert-contains stdout "\"error\"")
                 (assert-contains stdout "unknown param"))
               (multiple-value-bind (rc _stdout stderr)
                   (run-cli-captured '("repl" "call" "ping"
                                       "--project-root" "/tmp/not-this-project"))
                 (declare (ignore _stdout))
                 (assert-eql 1 rc)
                 (assert-contains stderr "Reserved repl call parameter: project_root"))
               (multiple-value-bind (rc _stdout stderr)
                   (run-cli-captured
                    '("repl" "call" "ping"
                      "--params-json" "{\"token\":\"abc\"}"))
                 (declare (ignore _stdout))
                 (assert-eql 1 rc)
                 (assert-contains stderr "Reserved repl call parameter: token"))
               (format t "  schema rejection OK~%")

               (format t "Test: call dispatches ordinary RPCs~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl" "call" "image-info"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "\"pid\"")
                 (assert-contains stdout "\"lisp\""))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl" "call" "current-package"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "COMMON-LISP-USER")
                 (assert-not-contains stdout "CLPM.REPL.USER.")
                 (assert-not-contains stdout (namestring (truename proj))))
               (run-cli-captured '("repl" "call" "set-package"
                                   "--name" "COMMON-LISP"))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl" "call" "current-package"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "COMMON-LISP"))
                (run-cli-captured '("repl" "call" "set-package"
                                    "--name" "COMMON-LISP-USER"))
                (multiple-value-bind (rc stdout)
                    (run-cli-captured '("repl" "eval" "(in-package :cl-user)"))
                  (assert-eql 0 rc)
                  (assert-contains stdout "COMMON-LISP-USER"))
                (multiple-value-bind (rc stdout)
                    (run-cli-captured
                     '("repl" "eval"
                       "(eq *package* (find-package \"COMMON-LISP-USER\"))"))
                  (assert-eql 0 rc)
                  (assert-contains stdout "=> NIL"))
                (multiple-value-bind (rc stdout)
                    (run-cli-captured '("repl" "call" "list-workers"))
                  (assert-eql 0 rc)
                 (assert-contains stdout "default")
                 (assert-not-contains stdout "CLPM.REPL.USER.")
                 (assert-not-contains stdout (namestring (truename proj))))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl" "call" "ping"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "\"pid\"")
                 (assert-not-contains stdout "\"project_root\"")
                 (assert-not-contains stdout (namestring (truename proj))))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl" "daemon" "--status" "--json"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "\"state\":\"running\"")
                 (assert-not-contains stdout ".clpm/repl.sock")
                 (assert-not-contains stdout ".clpm/repl.log")
                 (assert-not-contains stdout (namestring (truename proj))))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl" "call" "gc"
                                       "--full" "true"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "\"result\""))
               (format t "  ordinary RPC call OK~%")

               (format t "Test: eval remains human-readable and persistent~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl" "eval" "(+ 1 2)"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "=> 3")
                 (assert-true (not (search "\"result\"" stdout))
                              "default eval should not be JSON: ~A" stdout))
               (multiple-value-bind (rc _stdout stderr)
                   (run-cli-captured '("repl" "eval" "(+ 1 2)" "--pretty"))
                 (declare (ignore _stdout))
                 (assert-eql 1 rc)
                 (assert-contains stderr "Unknown eval option: --pretty"))
               (run-cli-captured '("repl" "eval"
                                   "(defparameter *cli-x* 41)"))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl" "eval" "*cli-x*"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "=> 41"))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl" "call" "ping"))
                 (assert-eql 0 rc)
                 (assert-true (not (search "\"eval\"" stdout))
                              "ping method_counts leaked hidden eval method: ~A"
                              stdout))
               (multiple-value-bind (rc stdout stderr)
                   (run-cli-captured '("repl" "eval"
                                       "(error \"token\")"
                                       "--no-autostart"))
                 (declare (ignore stderr))
                 (assert-eql 1 rc)
                 (assert-contains stdout "token")
                 (assert-true (probe-file sock)
                              "eval error mentioning token removed daemon socket"))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl" "call" "ping"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "\"pid\""))
               (format t "  eval OK~%")

               (format t "Test: eval machine output mode is not debug output~%")
               (multiple-value-bind (rc _stdout stderr)
                   (run-cli-captured '("repl" "eval"
                                       "(error \"json debug ambiguity\")"
                                       "--debug" "--json"))
                 (declare (ignore _stdout))
                 (assert-eql 1 rc)
                 (assert-contains stderr "--debug cannot be combined with --json"))
               (format t "  eval output mode OK~%")

               (format t "Test: eval --debug handles debugger continuations~%")
               (multiple-value-bind (rc stdout stderr)
                   (run-cli-captured '("repl" "eval"
                                       "(error \"boom\")" "--debug"))
                 (declare (ignore stdout))
                 (assert-eql 3 rc)
                 (assert-contains stderr "debugger entered"))
                (multiple-value-bind (rc stdout)
                    (run-cli-captured '("repl" "eval"
                                        "(restart-case (/ 1 0) (use-value (v) v))"
                                       "--debug" "--restart" "USE-VALUE"
                                       "--arg" "42"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "=> 42"))
               (multiple-value-bind (rc _stdout stderr)
                   (run-cli-captured '("repl" "eval"
                                       "(error \"abort duplicate\")"
                                       "--debug" "--abort"))
                 (declare (ignore _stdout))
                 (assert-eql 1 rc)
                 (assert-contains stderr "Unknown eval option: --abort"))
               (dolist (case '((("repl" "eval" "(error \"needs frame pair\")"
                                  "--debug" "--frame" "1")
                                 "--frame and --frame-eval")
                                (("repl" "eval" "(error \"needs frame pair\")"
                                  "--debug" "--frame-eval" "(+ 1 2)")
                                 "--frame and --frame-eval")
                                (("repl" "eval" "(error \"arg needs restart\")"
                                  "--debug" "--arg" "42")
                                 "--arg requires --restart")
                                (("repl" "eval" "(error \"too many actions\")"
                                  "--debug" "--restart" "USE-VALUE" "--keep")
                                 "Choose only one debug continuation action")
                                (("repl" "eval" "(error \"too many actions\")"
                                  "--debug" "--restart" "USE-VALUE"
                                  "--frame" "1" "--frame-eval" "x")
                                 "Choose only one debug continuation action")))
                 (destructuring-bind (args expected) case
                   (multiple-value-bind (rc _stdout stderr)
                       (run-cli-captured args)
                     (declare (ignore _stdout))
                     (assert-eql 1 rc)
                     (assert-contains stderr expected))))
                (format t "  eval --debug OK~%")

                (format t "Test: eval --stdin reads and evaluates a region~%")
                (let ((*standard-input*
                        (make-string-input-stream
                         "(defpackage :rb-stdin-region (:use :cl))
(in-package :rb-stdin-region)
(defun value () 55)
(value)")))
                  (multiple-value-bind (rc stdout)
                      (run-cli-captured '("repl" "eval" "--stdin"))
                    (assert-eql 0 rc)
                    (assert-contains stdout "=> 55")
                    (assert-contains stdout "package: RB-STDIN-REGION")))
                (format t "  eval --stdin OK~%")

                (format t "Test: kept debug sessions are managed through call~%")
                (multiple-value-bind (rc stdout stderr)
                   (run-cli-captured
                    '("repl" "eval"
                      "(progn
                         (declaim (optimize (debug 3) (safety 3) (speed 0)))
                         (defun rb-cli-debug-keep-target (x)
                           (error \"x=~A\" x))
                         (rb-cli-debug-keep-target 7))"
                       "--debug" "--keep"))
                  (declare (ignore stdout))
                  (assert-eql 3 rc)
                  (assert-contains stderr "session:")
                  (assert-contains stderr "RB-CLI-DEBUG-KEEP-TARGET"))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl" "call"
                                       "list-debug-sessions"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "\"sessions\""))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl" "call"
                                       "debug-eval-in-frame"
                                       "--frame" "4"
                                       "--form" "(* x 2)"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "14"))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl" "call" "debug-abort"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "aborted"))
               (format t "  kept debug session OK~%")

                (format t "Test: source RPCs and redefinition drift use call~%")
                (multiple-value-bind (rc stdout)
                    (run-cli-captured '("repl" "call" "set-package"
                                        "--name" "COMMON-LISP-USER"))
                  (assert-eql 0 rc)
                  (assert-contains stdout "COMMON-LISP-USER"))
                (multiple-value-bind (rc stdout)
                    (run-cli-captured '("repl" "call" "macroexpand"
                                        "--form" "(when t :ok)"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "IF"))
               (let ((src (merge-pathnames "hello.lisp" proj)))
                 (with-open-file (s src :direction :output
                                        :if-exists :supersede)
                   (write-string "(defun hello () \"hi\")" s))
                 (multiple-value-bind (rc stdout)
                     (run-cli-captured (list "repl" "call"
                                             "compile-file"
                                             "--path" (namestring src)))
                   (assert-eql 0 rc)
                   (assert-contains stdout "\"result\""))
                 (multiple-value-bind (rc stdout)
                     (run-cli-captured (list "repl" "call"
                                             "load-file"
                                             "--path" (namestring src)))
                   (assert-eql 0 rc)
                   (assert-contains stdout "\"result\""))
                 (multiple-value-bind (rc stdout)
                     (run-cli-captured '("repl" "eval" "(hello)"))
                   (assert-eql 0 rc)
                   (assert-contains stdout "hi")))
               (run-cli-captured '("repl" "eval"
                                   "(defun diff-fn-x () 1)"))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl" "call"
                                       "list-redefinitions"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "DIFF-FN-X")
                 (assert-contains stdout "COMMON-LISP-USER")
                 (assert-not-contains stdout "CLPM.REPL.USER."))
               (run-cli-captured '("repl" "eval"
                                   "(defun package-leak-sentinel () 42)"))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl" "eval"
                                       "(format t \"~A\" (package-name *package*))"
                                       "--json"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "COMMON-LISP-USER")
                 (assert-not-contains stdout "CLPM.REPL.USER."))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl" "eval"
                                       "(error \"pkg ~S\" *package*)"
                                       "--json"))
                 (assert-eql 1 rc)
                 (assert-contains stdout "COMMON-LISP-USER")
                 (assert-not-contains stdout "CLPM.REPL.USER."))
               (dolist (case '(("apropos" ("--pattern" "PACKAGE-LEAK-SENTINEL"))
                               ("function-info" ("--symbol" "PACKAGE-LEAK-SENTINEL"))
                               ("function-info" ("--symbol" "PACKAGE-LEAK-SENTINEL"
                                                 "--package" "COMMON-LISP-USER"))
                               ("describe" ("--symbol" "PACKAGE-LEAK-SENTINEL"))))
                 (destructuring-bind (method args) case
                   (multiple-value-bind (rc stdout)
                       (run-cli-captured (append (list "repl" "call" method)
                                                 args))
                     (assert-eql 0 rc)
                     (assert-contains stdout "PACKAGE-LEAK-SENTINEL")
                     (assert-contains stdout "COMMON-LISP-USER")
                     (assert-not-contains stdout "CLPM.REPL.USER."))))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl" "call" "inspect"
                                       "--form" "(quote package-leak-sentinel)"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "PACKAGE-LEAK-SENTINEL")
                 (assert-contains stdout "COMMON-LISP-USER")
                 (assert-not-contains stdout "CLPM.REPL.USER."))
               (run-cli-captured '("repl" "call" "trace"
                                   "--symbols" "[\"PACKAGE-LEAK-SENTINEL\"]"))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl" "call" "list-traced"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "PACKAGE-LEAK-SENTINEL")
                 (assert-not-contains stdout "CLPM.REPL.USER."))
               (run-cli-captured '("repl" "call" "untrace"
                                   "--symbols" "[\"PACKAGE-LEAK-SENTINEL\"]"))
               (format t "  source RPCs OK~%")

               (format t "Test: legacy wrappers are gone~%")
               (multiple-value-bind (rc stdout stderr)
                   (run-cli-captured '("repl" "ping"))
                 (declare (ignore stdout))
                 (assert-true (not (zerop rc)) "expected nonzero rc")
                 (assert-contains stderr "Unknown subcommand"))
               (multiple-value-bind (rc stdout stderr)
                   (run-cli-captured '("repl" "debug"
                                       "(error \"x\")"))
                 (declare (ignore stdout))
                 (assert-true (not (zerop rc)) "expected nonzero rc")
                 (assert-contains stderr "Unknown subcommand"))
               (multiple-value-bind (rc _stdout stderr)
                   (run-cli-captured '("repl" "call" "query-response"
                                       "--value" "true"))
                 (declare (ignore _stdout))
                 (assert-eql 1 rc)
                 (assert-contains stderr
                                  "query-response is a continuation message"))
               (multiple-value-bind (rc _stdout stderr)
                   (run-cli-captured '("repl" "call" "shutdown"))
                 (declare (ignore _stdout))
                 (assert-eql 1 rc)
                 (assert-contains stderr
                                  "Use `clpm repl daemon --stop`"))
               (format t "  legacy wrappers rejected OK~%"))
          (ignore-errors (run-cli-captured '("repl" "daemon" "--stop")))
          (loop for i from 0 below 30
                while (sb-thread:thread-alive-p srv)
                do (sleep 0.1))
          (when (sb-thread:thread-alive-p srv)
            (ignore-errors (sb-thread:terminate-thread srv))))))))

(format t "~%REPL small CLI tests PASSED!~%")
(sb-ext:exit :code 0)
