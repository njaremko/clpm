;;;; test/new-command-test.lisp - `clpm project new` scaffolding tests

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

(defun assert-true (x fmt &rest args)
  (unless x
    (apply #'fail fmt args)))

(defun assert-eql (expected actual)
  (unless (eql expected actual)
    (fail "Assertion failed: expected ~S, got ~S" expected actual)))

(defun assert-equal (expected actual)
  (unless (equal expected actual)
    (fail "Assertion failed: expected ~S, got ~S" expected actual)))

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

(defun assert-duplicate-option (args option)
  (multiple-value-bind (code stdout stderr)
      (run-cli-captured args)
    (declare (ignore stdout))
    (assert-eql 1 code)
    (assert-contains stderr (format nil "Duplicate option: ~A" option))))

(defparameter *expected-gitignore*
  ".DS_Store
.clpm/
*.fasl
*.fasl-tmp
")

(defun assert-gitignore (root)
  (assert-equal *expected-gitignore*
                (uiop:read-file-string (merge-pathnames ".gitignore" root))))

(format t "Testing `clpm project new` scaffolding...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((workspace (merge-pathnames "ws/" tmp))
         (bin-root (merge-pathnames "binproj/" workspace))
         (lib-root (merge-pathnames "libproj/" workspace)))
    (ensure-directories-exist workspace)

    ;; Singleton value options must reject duplicates before creating files.
    (assert-duplicate-option
     (list "project" "new" "dupdir" "--lib"
           "--dir" (namestring workspace)
           "--dir" (namestring (merge-pathnames "other-ws/" tmp)))
     "--dir")

    (let ((member-a (merge-pathnames "member-a/" tmp))
          (member-b (merge-pathnames "member-b/" tmp)))
      (dolist (member-root (list member-a member-b))
        (ensure-directories-exist member-root)
        (clpm.workspace:write-workspace-file
         (clpm.workspace:make-workspace :format 1 :members '())
         (merge-pathnames "clpm.workspace" member-root)))
      (assert-duplicate-option
       (list "project" "new" "dupmember" "--lib"
             "--member-of" (namestring member-a)
             "--member-of" (namestring member-b))
       "--member-of"))

    ;; --bin
    (uiop:with-current-directory (workspace)
      (assert-eql 0 (clpm:run-cli '("project" "new" "binproj" "--bin"))))
    (assert-true (uiop:directory-exists-p bin-root) "Expected bin project dir")
    (dolist (rel '(".gitignore" "clpm.project" "binproj.asd" "src/binproj.lisp" "test/binproj-test.lisp"))
      (assert-true (uiop:file-exists-p (merge-pathnames rel bin-root))
                   "Missing scaffold file: ~A" rel))
    (assert-gitignore bin-root)
    (let ((p (clpm.project:read-project-file (merge-pathnames "clpm.project" bin-root))))
      (assert-true (string= "binproj" (clpm.project:project-name p))
                   "Unexpected project name: ~S" (clpm.project:project-name p))
      (assert-true (equal '("binproj") (clpm.project:project-systems p))
                   "Unexpected project systems: ~S" (clpm.project:project-systems p))
      (let ((run (clpm.project:project-run p)))
        (assert-true run "Expected :run metadata for --bin project")
        (assert-true (string= "binproj" (getf run :system))
                     "Unexpected :run :system: ~S" (getf run :system))
        (assert-true (string= "binproj::main" (getf run :function))
                     "Unexpected :run :function: ~S" (getf run :function)))
      (let ((test (clpm.project:project-test p)))
        (assert-true test "Expected :test metadata for scaffolded project")
        (assert-true (equal '("binproj/test") (getf test :systems))
                     "Unexpected :test :systems: ~S" (getf test :systems)))
      (let ((pkg (clpm.project:project-package p)))
        (assert-true pkg "Expected :package metadata for --bin project")
        (assert-true (string= "dist/binproj" (getf pkg :output))
                     "Unexpected :package :output: ~S" (getf pkg :output))
        (assert-true (string= "binproj" (getf pkg :system))
                     "Unexpected :package :system: ~S" (getf pkg :system))
        (assert-true (string= "binproj::main" (getf pkg :function))
                     "Unexpected :package :function: ~S" (getf pkg :function))))

    ;; --lib with --dir
    (uiop:with-current-directory (workspace)
      (assert-eql 0 (clpm:run-cli (list "project" "new" "libproj" "--lib" "--dir" (namestring workspace)))))
    (assert-true (uiop:directory-exists-p lib-root) "Expected lib project dir")
    (dolist (rel '(".gitignore" "clpm.project" "libproj.asd" "src/libproj.lisp" "test/libproj-test.lisp"))
      (assert-true (uiop:file-exists-p (merge-pathnames rel lib-root))
                   "Missing scaffold file: ~A" rel))
    (assert-gitignore lib-root)
    (let ((p (clpm.project:read-project-file (merge-pathnames "clpm.project" lib-root))))
      (assert-true (string= "libproj" (clpm.project:project-name p))
                   "Unexpected project name: ~S" (clpm.project:project-name p))
      (assert-true (equal '("libproj") (clpm.project:project-systems p))
                   "Unexpected project systems: ~S" (clpm.project:project-systems p))
      (assert-true (null (clpm.project:project-run p))
                   "Did not expect :run metadata for --lib project")
      (let ((test (clpm.project:project-test p)))
        (assert-true test "Expected :test metadata for scaffolded project")
        (assert-true (equal '("libproj/test") (getf test :systems))
                     "Unexpected :test :systems: ~S" (getf test :systems)))
      (assert-true (null (clpm.project:project-package p))
                   "Did not expect :package metadata for --lib project"))))

(format t "  `clpm project new` scaffolding PASSED~%")
(format t "~%New command tests PASSED!~%")
(sb-ext:exit :code 0)
