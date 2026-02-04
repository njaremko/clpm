;;;; test/ux-errors-test.lisp - UX-focused actionable error tests

(require :asdf)
(require :sb-posix)

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

(defun run-cli-captured (args)
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((*standard-output* out)
          (*error-output* err))
      (let ((code (clpm:run-cli args)))
        (values code
                (get-output-stream-string out)
                (get-output-stream-string err))))))

(format t "Testing actionable \"No clpm.project found\"...~%")
(clpm.store:with-temp-dir (tmp)
  (uiop:with-current-directory (tmp)
    (multiple-value-bind (code _out err)
        (run-cli-captured '("resolve"))
      (declare (ignore _out))
      (assert-eql 1 code)
      (assert-true (search "No clpm.project found" err)
                   "Expected 'No clpm.project found' in stderr, got:~%~A"
                   err)
      (assert-true (search "Searched for clpm.project in:" err)
                   "Expected searched paths list in stderr, got:~%~A"
                   err)
      (let ((tmp-ns (namestring (uiop:ensure-directory-pathname tmp))))
        (assert-true (search tmp-ns err)
                     "Expected current dir (~A) listed in searched paths, got:~%~A"
                     tmp-ns err)))))
(format t "  No project error PASSED~%")

(format t "Testing actionable workspace root requires -p/--package...~%")
(clpm.store:with-temp-dir (tmp)
  (let* ((ws-root (merge-pathnames "ws/" tmp))
         (ws-path (merge-pathnames "clpm.workspace" ws-root)))
    (ensure-directories-exist ws-root)
    (clpm.io.sexp:write-canonical-sexp-to-file
     '(:workspace :format 1 :members ("app" "lib"))
     ws-path
     :pretty t)
    (uiop:with-current-directory (ws-root)
      (multiple-value-bind (code _out err)
          (run-cli-captured '("resolve"))
        (declare (ignore _out))
        (assert-eql 1 code)
        (assert-true (search "Workspace root detected" err)
                     "Expected workspace root guidance in stderr, got:~%~A"
                     err)
        (assert-true (search "Use -p/--package" err)
                     "Expected -p guidance in stderr, got:~%~A"
                     err)
        (assert-true (search "Workspace members:" err)
                     "Expected member listing in stderr, got:~%~A"
                     err)
        (assert-true (search "app" err) "Expected member 'app' listed, got:~%~A" err)
        (assert-true (search "lib" err) "Expected member 'lib' listed, got:~%~A" err)))))
(format t "  Workspace root error PASSED~%")

(format t "Testing missing tool errors include install hints...~%")
(let ((old-path (sb-posix:getenv "PATH")))
  (unwind-protect
       (progn
         (sb-posix:setenv "PATH" "" 1)
         (handler-case
             (progn
               (clpm.lisp:lisp-run-argv :sbcl)
               (fail "Expected clpm-missing-tool-error, but lisp-run-argv succeeded"))
           (clpm.errors:clpm-missing-tool-error (c)
             (let ((msg (with-output-to-string (s) (format s "~A" c))))
               (assert-true (search "Missing tool: sbcl" msg :test #'char-equal)
                            "Expected missing tool message for sbcl, got:~%~A"
                            msg)
               (assert-true (search "Install with:" msg :test #'char-equal)
                            "Expected install hints in error message, got:~%~A"
                            msg)))))
    (if old-path
        (sb-posix:setenv "PATH" old-path 1)
        (sb-posix:unsetenv "PATH"))))
(format t "  Missing tool error PASSED~%")

(format t "~%UX error tests PASSED!~%")
(sb-ext:exit :code 0)

