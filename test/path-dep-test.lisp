;;;; test/path-dep-test.lisp - Local (:path ...) dependency tests

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

(format t "Testing (:path ...) dependencies end-to-end...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((workspace (merge-pathnames "ws/" tmp))
         (dep-root (merge-pathnames "dep/" workspace))
         (app-root (merge-pathnames "app/" workspace)))
    (ensure-directories-exist dep-root)
    (ensure-directories-exist app-root)

    ;; Local dependency project (ASDF system named "dep").
    (write-file (merge-pathnames "dep.asd" dep-root)
                (format nil "~S~%"
                        '(asdf:defsystem "dep"
                           :version "0.1.0"
                           :serial t
                           :components ((:file "dep")))))
    (write-file (merge-pathnames "dep.lisp" dep-root)
                (format nil "~A~%"
                        "(defpackage #:dep (:use #:cl) (:export #:hello))\n(in-package #:dep)\n(defun hello () :ok)\n"))

    ;; App project that depends on dep via relative (:path "../dep").
    (write-file (merge-pathnames "clpm.project" app-root)
                (format nil "~S~%"
                        `(:project
                          :name "app"
                          :version "0.1.0"
                          :systems ("app")
                          :depends ((:dep :system "dep"
                                    :constraint (:path "../dep")))
                          :registries ())))

    ;; Install from app root.
    (uiop:with-current-directory (app-root)
      (assert-eql 0 (clpm:run-cli '("install"))))

    ;; Verify activation config exists.
    (let ((config-path (merge-pathnames ".clpm/asdf-config.lisp" app-root)))
      (assert-true (uiop:file-exists-p config-path)
                   "Missing activation config: ~A" (namestring config-path))
      ;; Validate SBCL can load the dependency system non-interactively
      ;; (this approximates what `clpm repl --load-system dep` would do).
      (multiple-value-bind (output error-output exit-code)
          (clpm.platform:run-program
           (list "sbcl" "--noinform" "--non-interactive" "--disable-debugger"
                 "--load" (namestring config-path)
                 "--eval" "(asdf:load-system \"dep\")")
           :output :string
           :error-output :string
           :timeout 60)
        (declare (ignore output))
        (assert-true (zerop exit-code)
                     "SBCL failed to load dep via activation config:~%~A"
                     error-output)))))

(format t "  (:path ...) end-to-end PASSED~%")
(format t "~%Path dependency tests PASSED!~%")
(sb-ext:exit :code 0)

