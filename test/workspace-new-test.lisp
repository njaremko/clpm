;;;; test/workspace-new-test.lisp - workspace scaffolding tests

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

(format t "Testing workspace scaffolding...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((base (merge-pathnames "wsbase/" tmp))
         (ws-root (merge-pathnames "myws/" base))
         (ws-path (merge-pathnames "clpm.workspace" ws-root)))
    (ensure-directories-exist base)

    ;; Create workspace
    (uiop:with-current-directory (base)
      (assert-eql 0 (clpm:run-cli (list "new" "myws" "--workspace" "--dir" (namestring base)))))
    (assert-true (uiop:directory-exists-p ws-root) "Expected workspace directory")
    (assert-true (uiop:file-exists-p ws-path) "Expected clpm.workspace file")
    (let ((ws (clpm.workspace:read-workspace-file ws-path)))
      (assert-equal '() (clpm.workspace:workspace-members ws)))

    ;; Add members out of order; verify deterministic (sorted) members list.
    (assert-eql 0 (clpm:run-cli (list "new" "zeta" "--lib" "--member-of" (namestring ws-root))))
    (assert-eql 0 (clpm:run-cli (list "new" "alpha" "--bin" "--member-of" (namestring ws-root))))

    (assert-true (uiop:directory-exists-p (merge-pathnames "zeta/" ws-root))
                 "Expected member directory zeta/")
    (assert-true (uiop:directory-exists-p (merge-pathnames "alpha/" ws-root))
                 "Expected member directory alpha/")

    (let ((ws (clpm.workspace:read-workspace-file ws-path)))
      (assert-equal '("alpha/" "zeta/") (clpm.workspace:workspace-members ws)))))

(format t "  Workspace scaffolding PASSED~%")
(format t "~%Workspace new tests PASSED!~%")
(sb-ext:exit :code 0)

