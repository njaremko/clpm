;;;; test/git-dep-test.lisp - Local (:git ...) dependency tests

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

(defun write-file (path contents)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (write-string contents s)))

(defun hex40-p (s)
  (and (stringp s)
       (= (length s) 40)
       (every (lambda (c) (find c "0123456789abcdef" :test #'char-equal)) s)))

(format t "Testing (:git ...) dependencies end-to-end...~%")

(let ((git (clpm.platform:find-git)))
  (unless git
    (fail "git not found in PATH"))

  (clpm.store:with-temp-dir (tmp)
    (let* ((workspace (merge-pathnames "ws/" tmp))
           (repo-root (merge-pathnames "dep-repo/" workspace))
           (app-root (merge-pathnames "app/" workspace)))
      (ensure-directories-exist repo-root)
      (ensure-directories-exist app-root)

      ;; Create a minimal dependency repo with an ASDF system named "dep".
      (write-file (merge-pathnames "dep.asd" repo-root)
                  (format nil "~S~%"
                          '(asdf:defsystem "dep"
                             :version "0.1.0"
                             :serial t
                             :components ((:file "dep")))))
      (write-file (merge-pathnames "dep.lisp" repo-root)
                  (format nil "~A~%"
                          "(defpackage #:dep (:use #:cl) (:export #:hello))\n(in-package #:dep)\n(defun hello () :ok)\n"))

      ;; Init git repo + commit.
      (multiple-value-bind (output error-output exit-code)
          (clpm.platform:run-program (list git "init")
                                     :directory repo-root
                                     :error-output :string)
        (declare (ignore output))
        (assert-eql 0 exit-code)
        (assert-true (or (null error-output) t) "git init failed"))
      (dolist (cfg '(("user.email" "test@example.com")
                     ("user.name" "CLPM Test")))
        (multiple-value-bind (output error-output exit-code)
            (clpm.platform:run-program (list git "config" (first cfg) (second cfg))
                                       :directory repo-root
                                       :error-output :string)
          (declare (ignore output))
          (assert-eql 0 exit-code)
          (assert-true (or (null error-output) t) "git config failed")))
      (multiple-value-bind (output error-output exit-code)
          (clpm.platform:run-program (list git "add" ".")
                                     :directory repo-root
                                     :error-output :string)
        (declare (ignore output))
        (assert-eql 0 exit-code)
        (assert-true (or (null error-output) t) "git add failed"))
      (multiple-value-bind (output error-output exit-code)
          (clpm.platform:run-program (list git "commit" "-m" "init")
                                     :directory repo-root
                                     :error-output :string)
        (declare (ignore output))
        (assert-eql 0 exit-code)
        (assert-true (or (null error-output) t) "git commit failed"))

      (multiple-value-bind (head-out error-output exit-code)
          (clpm.platform:run-program (list git "rev-parse" "HEAD")
                                     :directory repo-root
                                     :error-output :string)
        (declare (ignore error-output))
        (assert-eql 0 exit-code)
        (let* ((head (string-trim '(#\Space #\Newline #\Return) head-out))
               (url (format nil "file://~A" (namestring repo-root))))
          (assert-true (hex40-p head) "Invalid HEAD sha: ~S" head)

          ;; App project depends on dep via git ref (HEAD).
          (write-file (merge-pathnames "clpm.project" app-root)
                      (format nil "~S~%"
                              `(:project
                                :name "app"
                                :version "0.1.0"
                                :systems ("app")
                                :depends ((:dep :system "dep"
                                          :constraint (:git :url ,url :ref "HEAD")))
                                :registries ())))

          ;; Install from app root.
          (uiop:with-current-directory (app-root)
            (assert-eql 0 (clpm:run-cli '("install"))))

          ;; Ensure lockfile records a resolved commit.
          (let* ((lock-path (merge-pathnames "clpm.lock" app-root))
                 (lock (clpm.project:read-lock-file lock-path))
                 (locked (first (clpm.project:lockfile-resolved lock)))
                 (release (clpm.project:locked-system-release locked))
                 (source (clpm.project:locked-release-source release))
                 (locked-commit (clpm.project:locked-source-commit source)))
            (assert-true (eq (clpm.project:locked-source-kind source) :git)
                         "Expected :git source, got ~S"
                         (clpm.project:locked-source-kind source))
            (assert-true (hex40-p locked-commit)
                         "Expected 40-hex commit in clpm.lock, got ~S"
                         locked-commit)
            (assert-true (string= locked-commit head)
                         "clpm.lock commit mismatch: expected ~A got ~A"
                         head locked-commit)))))))

(format t "  (:git ...) end-to-end PASSED~%")
(format t "~%Git dependency tests PASSED!~%")
(sb-ext:exit :code 0)

