;;;; test/tree-why-test.lisp - Dependency introspection command tests

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

(defun assert-equal (expected actual)
  (unless (equal expected actual)
    (fail "Assertion failed: expected ~S, got ~S" expected actual)))

(defun assert-eql (expected actual)
  (unless (eql expected actual)
    (fail "Assertion failed: expected ~S, got ~S" expected actual)))

(defun assert-true (x fmt &rest args)
  (unless x
    (apply #'fail fmt args)))

(defun split-lines (s)
  (let ((lines (uiop:split-string s :separator '(#\Newline))))
    (if (and lines (string= (car (last lines)) ""))
        (butlast lines)
        lines)))

(defun run-cli-captured (args)
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((*standard-output* out)
          (*error-output* err))
      (let ((code (clpm:run-cli args)))
        (values code
                (get-output-stream-string out)
                (get-output-stream-string err))))))

(defun make-locked-system (id deps &key (version "1.0.0"))
  (let* ((empty-sha256 "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
         (src (clpm.project:make-locked-source
               :kind :tarball
               :url (format nil "https://example.invalid/~A-~A.tgz" id version)
               :sha256 empty-sha256))
         (rel (clpm.project:make-locked-release
               :name id
               :version version
               :source src
               :artifact-sha256 empty-sha256
               :tree-sha256 empty-sha256)))
    (clpm.project:make-locked-system
     :id id
     :release rel
     :deps deps)))

(format t "Testing `clpm tree` and `clpm why`...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((project-root (merge-pathnames "proj/" tmp))
         (manifest-path (merge-pathnames "clpm.project" project-root))
         (lock-path (merge-pathnames "clpm.lock" project-root)))
    (ensure-directories-exist project-root)

    ;; Project roots: bar, foo, testlib.
    (let* ((project (clpm.project:make-project
                     :name "proj"
                     :version "0.1.0"
                     :systems '("proj")
                     :depends (list (clpm.project:make-dependency :system "foo" :constraint nil)
                                    (clpm.project:make-dependency :system "bar" :constraint nil))
                     :dev-depends '()
                     :test-depends (list (clpm.project:make-dependency :system "testlib" :constraint nil))
                     :registries '()
                     :sbcl-constraints '()
                     :build-options '()
                     :scripts '())))
      (clpm.project:write-project-file project manifest-path))

    ;; Lockfile graph:
    ;;   bar -> util -> leaf
    ;;   foo -> util
    ;;   testlib -> util
    (let* ((lock (clpm.project:make-lockfile
                  :format 1
                  :generated-at "2026-02-01T00:00:00Z"
                  :project-name "proj"
                  :clpm-version "0.1.0"
                  :registries '()
                  :resolved (list (make-locked-system "bar" (list "util"))
                                 (make-locked-system "foo" (list "util"))
                                 (make-locked-system "testlib" (list "util"))
                                 (make-locked-system "util" (list "leaf"))
                                 (make-locked-system "leaf" '())))))
      (clpm.project:write-lock-file lock lock-path))

    (uiop:with-current-directory (project-root)
      ;; tree (default depth)
      (multiple-value-bind (code stdout stderr)
          (run-cli-captured '("tree"))
        (declare (ignore stderr))
        (assert-eql 0 code)
        (assert-equal
         '("bar"
           "  util"
           "    leaf"
           "foo"
           "  util (*)"
           "testlib"
           "  util (*)")
         (split-lines stdout)))

      ;; tree --depth 1
      (multiple-value-bind (code stdout stderr)
          (run-cli-captured '("tree" "--depth" "1"))
        (declare (ignore stderr))
        (assert-eql 0 code)
        (assert-equal
         '("bar"
           "  util"
           "foo"
           "  util"
           "testlib"
           "  util")
         (split-lines stdout)))

      ;; tree --depth 0
      (multiple-value-bind (code stdout stderr)
          (run-cli-captured '("tree" "--depth" "0"))
        (declare (ignore stderr))
        (assert-eql 0 code)
        (assert-equal
         '("bar"
           "foo"
           "testlib")
         (split-lines stdout)))

      ;; why leaf
      (multiple-value-bind (code stdout stderr)
          (run-cli-captured '("why" "leaf"))
        (declare (ignore stderr))
        (assert-eql 0 code)
        (assert-equal
         '("Why: leaf"
           "  bar -> util -> leaf"
           "  foo -> util -> leaf"
           "  testlib -> util -> leaf")
         (split-lines stdout)))

      ;; why root node
      (multiple-value-bind (code stdout stderr)
          (run-cli-captured '("why" "foo"))
        (declare (ignore stderr))
        (assert-eql 0 code)
        (assert-equal
         '("Why: foo"
           "  foo")
         (split-lines stdout)))

      ;; why missing node
      (multiple-value-bind (code stdout stderr)
          (run-cli-captured '("why" "missing"))
        (declare (ignore stdout))
        (assert-eql 1 code)
        (assert-true (search "System not reachable" stderr :test #'char-equal)
                     "Expected error to mention unreachable system, got:~%~A"
                     stderr)))))

(format t "  Tree/why command tests PASSED~%")
(format t "~%Tree/why command tests PASSED!~%")
(sb-ext:exit :code 0)

