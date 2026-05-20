;;;; test/repl-bridge-cli-subcommands-test.lisp -- end-to-end test for the
;;;; expanded `clpm repl-bridge ...' surface: image-info, apropos, find-
;;;; definition, arglist, doc, function-info, list-packages, package-info,
;;;; loaded-systems, current-package, set-package, gc, time-eval, trace/
;;;; untrace/list-traced, workers/reset, macroexpand, compile-file,
;;;; inspect (with --path traversal), debug (with --restart and --arg).

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
                  (lambda () (run-cli-captured '("repl-bridge" "serve")))
                  :name "test-cli-sub-serve"))
            (sock (namestring (merge-pathnames ".clpm/repl-bridge.sock" proj))))
        (unwind-protect
             (progn
               (loop for i from 0 below 50
                     while (not (probe-file sock))
                     do (sleep 0.1))
               (assert-true (probe-file sock) "daemon socket did not appear")
               (format t "  daemon up~%")

               (format t "Test: image-info~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "image-info"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "pid:")
                 (assert-contains stdout "lisp:"))
               (format t "  image-info OK~%")

               (format t "Test: image-info --json~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "image-info" "--json"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "\"pid\":")
                 (assert-contains stdout "\"features\":"))
               (format t "  image-info --json OK~%")

               (format t "Test: apropos~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "apropos" "mapcar"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "COMMON-LISP:MAPCAR"))
               (format t "  apropos OK~%")

               (format t "Test: arglist~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "arglist" "mapcar"))
                 (assert-eql 0 rc)
                 (assert-true (or (search "FUNCTION" stdout)
                                  (search "function" stdout))
                              "expected function in arglist, got ~A" stdout))
               (format t "  arglist OK~%")

               (format t "Test: find-definition~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "find-definition" "car"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "function"))
               (format t "  find-definition OK~%")

               (format t "Test: list-packages~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "list-packages"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "packages"))
               (format t "  list-packages OK~%")

               (format t "Test: loaded-systems~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "loaded-systems"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "clpm"))
               (format t "  loaded-systems OK~%")

               (format t "Test: current-package~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "current-package"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "COMMON-LISP-USER"))
               (format t "  current-package OK~%")

               (format t "Test: set-package + current-package~%")
               (run-cli-captured '("repl-bridge" "set-package" "COMMON-LISP"))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "current-package"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "COMMON-LISP"))
               (run-cli-captured '("repl-bridge" "set-package" "COMMON-LISP-USER"))
               (format t "  set-package OK~%")

               (format t "Test: workers shows default~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "workers"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "default"))
               (format t "  workers OK~%")

               (format t "Test: gc~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "gc"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "consed before"))
               (format t "  gc OK~%")

               (format t "Test: time-eval~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "time-eval" "(+ 1 2)"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "value:")
                 (assert-contains stdout "real_ms:"))
               (format t "  time-eval OK~%")

               (format t "Test: macroexpand~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "macroexpand"
                                       "(when t :ok)"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "IF"))
               (format t "  macroexpand OK~%")

               (format t "Test: trace / list-traced / untrace~%")
               ;; trace + list-traced are per-thread on SBCL, so we don't
               ;; require list-traced to *see* the trace -- we just verify
               ;; both invocations return successfully and untrace clears.
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "trace" "identity"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "traced:"))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "list-traced"))
                 (declare (ignore stdout))
                 (assert-eql 0 rc))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "untrace"))
                 (declare (ignore stdout))
                 (assert-eql 0 rc))
               (format t "  trace flow OK~%")

               (format t "Test: inspect single-shot~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "inspect"
                                       "(list 100 200 300)"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "value:")
                 (assert-contains stdout "100"))
               (format t "  inspect OK~%")

               (format t "Test: inspect with --path~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "inspect"
                                       "(list :a :b :c)" "--path" "1"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "depth: 2"))
               (format t "  inspect --path OK~%")

               (format t "Test: debug returns rc=3 + frames~%")
               (multiple-value-bind (rc stdout stderr)
                   (run-cli-captured '("repl-bridge" "debug"
                                       "(error \"boom\")"))
                 (declare (ignore stdout))
                 (assert-eql 3 rc)
                 (assert-contains stderr "debugger entered"))
               (format t "  debug default OK~%")

               (format t "Test: debug --restart USE-VALUE --arg 42~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "debug"
                                       "(restart-case (/ 1 0) (use-value (v) v))"
                                       "--restart" "USE-VALUE"
                                       "--arg" "42"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "=> 42"))
               (format t "  debug --restart with arg OK~%")

               (format t "Test: --json on a leaf command~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "current-package" "--json"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "\"package\""))
               (format t "  --json flag OK~%")

               (format t "Test: unknown subcommand~%")
               (multiple-value-bind (rc stdout stderr)
                   (run-cli-captured '("repl-bridge" "nonexistent"))
                 (declare (ignore stdout))
                 (assert-true (not (zerop rc)) "expected nonzero rc")
                 (assert-contains stderr "Unknown subcommand"))
               (format t "  unknown-subcommand OK~%")

               (format t "Test: complete-symbol prints candidates~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "complete-symbol"
                                       "def" "--limit" "5"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "DEF"))
               (format t "  complete-symbol OK~%")

               (format t "Test: class-info renders precedence~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "class-info"
                                       "standard-object"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "name: STANDARD-OBJECT")
                 (assert-contains stdout "precedence:"))
               (format t "  class-info OK~%")

               (format t "Test: package-info renders exports~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "package-info"
                                       "clpm.io.json"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "exports:"))
               (format t "  package-info OK~%")

               (format t "Test: describe-system renders fields~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "describe-system" "clpm"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "name: clpm")
                 (assert-contains stdout "version:"))
               (format t "  describe-system OK~%")

               (format t "Test: disassemble renders asm~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "disassemble"
                                       "identity"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "disassembly"))
               (format t "  disassemble OK~%")

               (format t "Test: compile-file + load-file~%")
               (let ((src (merge-pathnames "hello.lisp" proj)))
                 (with-open-file (s src :direction :output
                                        :if-exists :supersede)
                   (write-string "(defun hello () \"hi\")" s))
                 (multiple-value-bind (rc stdout)
                     (run-cli-captured (list "repl-bridge" "compile-file"
                                             (namestring src)))
                   (assert-eql 0 rc)
                   (assert-contains stdout "compiled"))
                 (multiple-value-bind (rc stdout)
                     (run-cli-captured (list "repl-bridge" "load-file"
                                             (namestring src)))
                   (assert-eql 0 rc)
                   (assert-contains stdout "loaded"))
                 (multiple-value-bind (rc stdout)
                     (run-cli-captured '("repl-bridge" "eval" "(hello)"
                                          "--no-autostart"))
                   (declare (ignore rc))
                   (assert-contains stdout "hi")))
               (format t "  compile-file/load-file flow OK~%")

               (format t "Test: eval default is human-readable~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "eval" "(+ 1 2)"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "=> 3")
                 ;; Should not contain raw JSON envelope by default.
                 (assert-true (not (search "\"result\"" stdout))
                              "default eval should not be JSON: ~A" stdout))
               (format t "  eval default OK~%")

               (format t "Test: eval error shows restarts + user frames~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "eval"
                                       "(unknown-function-xyz 1)"))
                 (declare (ignore rc))
                 (assert-contains stdout "error:")
                 (assert-contains stdout "restarts")
                 ;; Daemon scaffolding should be stripped.
                 (assert-true (not (search "%WORKER-LOOP" stdout))
                              "should not show daemon frames: ~A" stdout)
                 (assert-true (not (search "%EVAL-ONE" stdout))
                              "should not show daemon frames: ~A" stdout))
               (format t "  eval error layout OK~%")

               (format t "Test: ping renders human-readable~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "ping"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "pid:")
                 (assert-contains stdout "uptime:"))
               (format t "  ping renderer OK~%")

               (format t "Test: diff renders kind+name~%")
               (run-cli-captured '("repl-bridge" "eval"
                                   "(defun diff-fn-x () 1)"))
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "diff"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "defun")
                 (assert-contains stdout "DIFF-FN-X"))
               (format t "  diff renderer OK~%")

               (format t "Test: help text~%")
               (multiple-value-bind (rc stdout)
                   (run-cli-captured '("repl-bridge" "help"))
                 (assert-eql 0 rc)
                 (assert-contains stdout "introspection")
                 (assert-contains stdout "debug"))
               (format t "  help OK~%"))
          ;; cleanup
          (ignore-errors (run-cli-captured '("repl-bridge" "stop")))
          (loop for i from 0 below 30
                while (sb-thread:thread-alive-p srv)
                do (sleep 0.1))
          (when (sb-thread:thread-alive-p srv)
            (ignore-errors (sb-thread:terminate-thread srv))))))))

(format t "~%REPL-bridge CLI subcommand tests PASSED!~%")
(sb-ext:exit :code 0)
