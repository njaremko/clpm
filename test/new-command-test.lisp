;;;; test/new-command-test.lisp - `clpm new` scaffolding tests

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

(format t "Testing `clpm new` scaffolding...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((workspace (merge-pathnames "ws/" tmp))
         (bin-root (merge-pathnames "binproj/" workspace))
         (lib-root (merge-pathnames "libproj/" workspace)))
    (ensure-directories-exist workspace)

    ;; --bin
    (uiop:with-current-directory (workspace)
      (assert-eql 0 (clpm:run-cli '("new" "binproj" "--bin"))))
    (assert-true (uiop:directory-exists-p bin-root) "Expected bin project dir")
    (dolist (rel '("clpm.project" "binproj.asd" "src/binproj.lisp" "test/binproj-test.lisp"))
      (assert-true (uiop:file-exists-p (merge-pathnames rel bin-root))
                   "Missing scaffold file: ~A" rel))
    (let ((p (clpm.project:read-project-file (merge-pathnames "clpm.project" bin-root))))
      (assert-true (string= "binproj" (clpm.project:project-name p))
                   "Unexpected project name: ~S" (clpm.project:project-name p))
      (assert-true (equal '("binproj") (clpm.project:project-systems p))
                   "Unexpected project systems: ~S" (clpm.project:project-systems p)))

    ;; --lib with --dir
    (uiop:with-current-directory (workspace)
      (assert-eql 0 (clpm:run-cli (list "new" "libproj" "--lib" "--dir" (namestring workspace)))))
    (assert-true (uiop:directory-exists-p lib-root) "Expected lib project dir")
    (dolist (rel '("clpm.project" "libproj.asd" "src/libproj.lisp" "test/libproj-test.lisp"))
      (assert-true (uiop:file-exists-p (merge-pathnames rel lib-root))
                   "Missing scaffold file: ~A" rel))
    (let ((p (clpm.project:read-project-file (merge-pathnames "clpm.project" lib-root))))
      (assert-true (string= "libproj" (clpm.project:project-name p))
                   "Unexpected project name: ~S" (clpm.project:project-name p))
      (assert-true (equal '("libproj") (clpm.project:project-systems p))
                   "Unexpected project systems: ~S" (clpm.project:project-systems p)))))

(format t "  `clpm new` scaffolding PASSED~%")
(format t "~%New command tests PASSED!~%")
(sb-ext:exit :code 0)

