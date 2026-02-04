;;;; test/workspace-discovery-test.lisp - Workspace file format + discovery tests

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

(defun assert-eql (expected actual)
  (unless (eql expected actual)
    (fail "Assertion failed: expected ~S, got ~S" expected actual)))

(defun assert-string= (expected actual)
  (unless (string= expected actual)
    (fail "Assertion failed: expected ~S, got ~S" expected actual)))

(defun assert-true (x fmt &rest args)
  (unless x
    (apply #'fail fmt args)))

(defun canonical-dir-namestring (p)
  (namestring (uiop:ensure-directory-pathname (truename p))))

(defun canonical-file-namestring (p)
  (namestring (truename p)))

(defun write-empty-project (root name)
  (clpm.io.sexp:write-canonical-sexp-to-file
   `(:project
     :name ,name
     :version "0.1.0"
     :systems (,name)
     :depends ()
     :dev-depends ()
     :test-depends ()
     :registries ()
     :scripts ())
   (merge-pathnames "clpm.project" root)))

(format t "Testing workspace discovery...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((ws-root (merge-pathnames "ws/" tmp))
         (member-a (merge-pathnames "a/" ws-root))
         (member-b (merge-pathnames "b/" ws-root))
         (ws-file (merge-pathnames "clpm.workspace" ws-root)))
    (ensure-directories-exist member-a)
    (ensure-directories-exist member-b)

    (clpm.workspace:write-workspace-file
     (clpm.workspace:make-workspace :format 1 :members '("a" "b"))
     ws-file)

    (write-empty-project member-a "a")
    (write-empty-project member-b "b")

    ;; Read workspace file
    (let ((ws (clpm.workspace:read-workspace-file ws-file)))
      (assert-eql 1 (clpm.workspace:workspace-format ws))
      (assert-true (equal '("a" "b") (clpm.workspace:workspace-members ws))
                   "Expected members (\"a\" \"b\"), got ~S"
                   (clpm.workspace:workspace-members ws)))

    ;; find-workspace-root
    (multiple-value-bind (root found)
        (clpm.workspace:find-workspace-root ws-root)
      (assert-string= (canonical-dir-namestring ws-root)
                      (canonical-dir-namestring root))
      (assert-string= (canonical-file-namestring ws-file)
                      (canonical-file-namestring found)))

    (multiple-value-bind (root found)
        (clpm.workspace:find-workspace-root member-a)
      (assert-string= (canonical-dir-namestring ws-root)
                      (canonical-dir-namestring root))
      (assert-string= (canonical-file-namestring ws-file)
                      (canonical-file-namestring found)))

    ;; find-project-or-workspace-root prefers nearest marker
    (multiple-value-bind (root kind manifest lock workspace)
        (clpm.workspace:find-project-or-workspace-root ws-root)
      (declare (ignore manifest lock))
      (assert-string= (canonical-dir-namestring ws-root)
                      (canonical-dir-namestring root))
      (assert-eql :workspace kind)
      (assert-string= (canonical-file-namestring ws-file)
                      (canonical-file-namestring workspace)))

    (multiple-value-bind (root kind manifest lock workspace)
        (clpm.workspace:find-project-or-workspace-root member-a)
      (declare (ignore lock workspace))
      (assert-string= (canonical-dir-namestring member-a)
                      (canonical-dir-namestring root))
      (assert-eql :project kind)
      (assert-string= (canonical-file-namestring (merge-pathnames "clpm.project" member-a))
                      (canonical-file-namestring manifest)))))

(format t "~%Workspace discovery tests PASSED!~%")
(sb-ext:exit :code 0)

