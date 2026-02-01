;;;; test/project-roundtrip-test.lisp - clpm.project round-trip tests

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

(format t "Testing clpm.project read/write round-trip...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((manifest (merge-pathnames "clpm.project" tmp))
         (manifest2 (merge-pathnames "clpm.project.2" tmp))
         (dep1 (clpm.project::make-dependency
                :system "foo"
                :constraint '(:semver "^1.0.0")
                :source '(:git :url "https://example.invalid/foo.git" :ref "main")
                :optional-p t
                :features '("feat-a" "feat-b")))
         (dep2 (clpm.project::make-dependency
                :system "bar"
                :constraint '(:exact "2.3.4")))
         (dev1 (clpm.project::make-dependency
                :system "dev-helper"
                :constraint '(:semver "^0.9.0")))
         (test1 (clpm.project::make-dependency
                 :system "test-lib"
                 :constraint '(:semver "^3.1.0")))
         (reg (clpm.project::make-registry-ref
               :kind :git
               :name "main"
               :url "https://example.invalid/registry.git"
               :trust "ed25519:test"))
         (project (clpm.project:make-project
                   :name "roundtrip"
                   :version "0.1.0"
                   :systems '("roundtrip")
                   :run '(:system "roundtrip" :function "roundtrip::main")
                   :test '(:systems ("roundtrip/test"))
                   :package '(:output "dist/roundtrip" :system "roundtrip" :function "roundtrip::main")
                   :depends (list dep1 dep2)
                   :dev-depends (list dev1)
                   :test-depends (list test1)
                   :registries (list reg)
                   :sbcl-constraints '(:min "2.0.0" :max nil)
                   :build-options '(:speed 3 :safety 1 :debug 1)
                   :scripts '(("hello" . "echo hello")
                              ("repl" . (:lisp "(format t \\\"hi\\\")"))))))
    (clpm.project:write-project-file project manifest)
    (let ((first (uiop:read-file-string manifest)))
      (let ((read-back (clpm.project:read-project-file manifest)))
        (clpm.project:write-project-file read-back manifest2))
      (let ((second (uiop:read-file-string manifest2)))
        (assert-true (string= first second)
                     "Round-trip mismatch for clpm.project")))))

(format t "  clpm.project round-trip PASSED~%")
(format t "~%Project round-trip tests PASSED!~%")
(sb-ext:exit :code 0)
