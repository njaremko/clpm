;;;; test/workspace-subcommand-test.lisp - `clpm workspace` subcommand tests

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

(format t "Testing `clpm workspace` subcommands...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (ws-root (merge-pathnames "ws/" tmp))
         (ws-path (merge-pathnames "clpm.workspace" ws-root))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist ws-root)
    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)

           ;; init
           (assert-eql 0 (clpm:run-cli (list "workspace" "init" "--dir" (namestring ws-root))))
           (assert-true (uiop:file-exists-p ws-path) "Expected clpm.workspace to exist")

           ;; add (normalize trailing slash)
           (assert-eql 0 (clpm:run-cli (list "workspace" "add" "app/" "--dir" (namestring ws-root))))
           (assert-eql 0 (clpm:run-cli (list "workspace" "add" "lib-a" "--dir" (namestring ws-root))))

           (let* ((ws (clpm.workspace:read-workspace-file ws-path))
                  (members (clpm.workspace:workspace-members ws)))
             (assert-true (equal members '("app" "lib-a"))
                          "Expected normalized members, got: ~S" members))

           ;; list
           (multiple-value-bind (code out err)
               (run-cli-captured (list "workspace" "list" "--dir" (namestring ws-root)))
             (declare (ignore err))
             (assert-eql 0 code)
             (assert-contains out "app")
             (assert-contains out "lib-a"))

           ;; error when no workspace exists
           (let ((other (merge-pathnames "other/" tmp)))
             (ensure-directories-exist other)
             (uiop:with-current-directory (other)
               (multiple-value-bind (code _out err)
                   (run-cli-captured '("workspace" "list"))
                 (declare (ignore _out))
                 (assert-true (not (zerop code)) "Expected workspace list to fail outside workspace")
                 (assert-contains err "No clpm.workspace found")))))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "~%Workspace subcommand tests PASSED!~%")
(sb-ext:exit :code 0)

