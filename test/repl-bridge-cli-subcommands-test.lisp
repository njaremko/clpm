;;;; test/repl-bridge-cli-subcommands-test.lisp -- end-to-end test for the
;;;; small repl-bridge CLI algebra: daemon, eval, and call.

(require :asdf)

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

(defun assert-eql (expected actual)
  (unless (eql expected actual) (fail "expected ~S, got ~S" expected actual)))

(defun assert-true (x fmt &rest args)
  (unless x (apply #'fail fmt args)))

(defun assert-contains (haystack needle)
  (unless (and (stringp haystack) (search needle haystack))
    (fail "expected ~S in:~%~A" needle haystack)))

(defun run-cli-captured (args)
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((*standard-output* out) (*error-output* err))
      (let ((rc (clpm:run-cli args)))
        (values rc
                (get-output-stream-string out)
                (get-output-stream-string err))))))

(clpm.store:with-temp-dir (tmp)
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
                    (handler-case (run-cli-captured '("repl-bridge" "daemon"))
                      (error (c)
                        (format *error-output* "daemon thread died: ~A~%" c)
                        (force-output *error-output*))))
                  :name "test-cli-small-daemon"))
            (sock (namestring (merge-pathnames ".clpm/repl-bridge.sock" proj))))
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
                   (run-cli-captured '("repl-bridge" "call" "methods"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "\"eval\"")
                 (assert-contains stdout "\"methods\""))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "call" "help"
                                       "--method" "eval"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "\"params\"")
                 (assert-contains stdout "\"form\""))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured
                    '("repl-bridge" "call" "help"
                      "--params-json" "{\"method\":\"eval\"}"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "\"params\"")
                 (assert-contains stdout "\"form\""))
               (format t "  registry discovery OK~%")

               (format t "Test: call dispatches ordinary RPCs~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "call" "image-info"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "\"pid\"")
                 (assert-contains stdout "\"lisp\""))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "call" "current-package"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "COMMON-LISP-USER"))
               (run-cli-captured '("repl-bridge" "call" "set-package"
                                   "--name" "COMMON-LISP"))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "call" "current-package"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "COMMON-LISP"))
               (run-cli-captured '("repl-bridge" "call" "set-package"
                                   "--name" "COMMON-LISP-USER"))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "call" "list-workers"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "default"))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "call" "gc"
                                       "--full" "true"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "\"result\""))
               (format t "  ordinary RPC call OK~%")

               (format t "Test: eval remains human-readable and persistent~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "eval" "(+ 1 2)"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "=> 3")
                 (assert-true (not (search "\"result\"" stdout))
                              "default eval should not be JSON: ~A" stdout))
               (run-cli-captured '("repl-bridge" "eval"
                                   "(defparameter *cli-x* 41)"))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "eval" "*cli-x*"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "=> 41"))
               (format t "  eval OK~%")

               (format t "Test: eval --debug handles debugger continuations~%")
               (multiple-value-bind (rc stdout stderr)
                   (run-cli-captured '("repl-bridge" "eval"
                                       "(error \"boom\")" "--debug"))
                 (declare (ignore stdout))
                 (assert-eql 3 rc)
                 (assert-contains stderr "debugger entered"))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "eval"
                                       "(restart-case (/ 1 0) (use-value (v) v))"
                                       "--debug" "--restart" "USE-VALUE"
                                       "--arg" "42"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "=> 42"))
               (format t "  eval --debug OK~%")

               (format t "Test: kept debug sessions are managed through call~%")
               (multiple-value-bind (rc stdout stderr)
                   (run-cli-captured
                    '("repl-bridge" "eval"
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
                   (run-cli-captured '("repl-bridge" "call"
                                       "list-debug-sessions"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "\"sessions\""))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "call"
                                       "debug-eval-in-frame"
                                       "--frame" "4"
                                       "--form" "(* x 2)"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "14"))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "call" "debug-abort"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "aborted"))
               (format t "  kept debug session OK~%")

               (format t "Test: source RPCs and redefinition drift use call~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "call" "macroexpand"
                                       "--form" "(when t :ok)"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "IF"))
               (let ((src (merge-pathnames "hello.lisp" proj)))
                 (with-open-file (s src :direction :output
                                        :if-exists :supersede)
                   (write-string "(defun hello () \"hi\")" s))
                 (multiple-value-bind (rc stdout)
                     (run-cli-captured (list "repl-bridge" "call"
                                             "compile-file"
                                             "--path" (namestring src)))
                   (assert-eql 0 rc)
                   (assert-contains stdout "\"result\""))
                 (multiple-value-bind (rc stdout)
                     (run-cli-captured (list "repl-bridge" "call"
                                             "load-file"
                                             "--path" (namestring src)))
                   (assert-eql 0 rc)
                   (assert-contains stdout "\"result\""))
                 (multiple-value-bind (rc stdout)
                     (run-cli-captured '("repl-bridge" "eval" "(hello)"))
                   (assert-eql 0 rc)
                   (assert-contains stdout "hi")))
               (run-cli-captured '("repl-bridge" "eval"
                                   "(defun diff-fn-x () 1)"))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "call"
                                       "list-redefinitions"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "DIFF-FN-X"))
               (format t "  source RPCs OK~%")

               (format t "Test: legacy wrappers are gone~%")
               (multiple-value-bind (rc stdout stderr)
                   (run-cli-captured '("repl-bridge" "ping"))
                 (declare (ignore stdout))
                 (assert-true (not (zerop rc)) "expected nonzero rc")
                 (assert-contains stderr "Unknown subcommand"))
               (multiple-value-bind (rc stdout stderr)
                   (run-cli-captured '("repl-bridge" "debug"
                                       "(error \"x\")"))
                 (declare (ignore stdout))
                 (assert-true (not (zerop rc)) "expected nonzero rc")
                 (assert-contains stderr "Unknown subcommand"))
               (format t "  legacy wrappers rejected OK~%"))
          (ignore-errors (run-cli-captured '("repl-bridge" "daemon" "--stop")))
          (loop for i from 0 below 30
                while (sb-thread:thread-alive-p srv)
                do (sleep 0.1))
          (when (sb-thread:thread-alive-p srv)
            (ignore-errors (sb-thread:terminate-thread srv))))))))

(format t "~%REPL-bridge small CLI tests PASSED!~%")
(sb-ext:exit :code 0)
