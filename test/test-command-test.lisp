;;;; test/test-command-test.lisp - `clpm test` command tests

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

(format t "Testing `clpm test`...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (project-root (merge-pathnames "app/" tmp))
         (src-dir (merge-pathnames "src/" project-root))
         (test-dir (merge-pathnames "test/" project-root))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist project-root)
    (ensure-directories-exist src-dir)
    (ensure-directories-exist test-dir)

    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)

           ;; ASDF systems: app + pass-test + fail-test.
           (write-file
            (merge-pathnames "app.asd" project-root)
            (with-output-to-string (s)
              (format s "(asdf:defsystem ~S~%  :version ~S~%  :serial t~%  :pathname ~S~%  :components ((:file ~S)))~%~%"
                      "app" "0.1.0" "src" "app")
              (format s "(asdf:defsystem ~S~%  :depends-on (~S)~%  :serial t~%  :pathname ~S~%  :components ((:file ~S))~%  :perform (asdf:test-op (op c)~%             (declare (ignore op c))~%             (uiop:symbol-call :app-pass-test :run)))~%~%"
                      "app/pass-test" "app" "test" "pass-test")
              (format s "(asdf:defsystem ~S~%  :depends-on (~S)~%  :serial t~%  :pathname ~S~%  :components ((:file ~S))~%  :perform (asdf:test-op (op c)~%             (declare (ignore op c))~%             (uiop:symbol-call :app-fail-test :run)))~%"
                      "app/fail-test" "app" "test" "fail-test")))

           (write-file
            (merge-pathnames "src/app.lisp" project-root)
            (with-output-to-string (s)
              (format s "(defpackage #:app (:use #:cl) (:export #:ping))~%")
              (format s "(in-package #:app)~%")
              (format s "(defun ping () :ok)~%")))

           (write-file
            (merge-pathnames "test/pass-test.lisp" project-root)
            (with-output-to-string (s)
              (format s "(defpackage #:app-pass-test (:use #:cl) (:export #:run))~%")
              (format s "(in-package #:app-pass-test)~%")
              (format s "(defun run () (assert (eql :ok (app:ping))) :ok)~%")))

           (write-file
            (merge-pathnames "test/fail-test.lisp" project-root)
            (with-output-to-string (s)
              (format s "(defpackage #:app-fail-test (:use #:cl) (:export #:run))~%")
              (format s "(in-package #:app-fail-test)~%")
              (format s "(defun run () (error \"boom\"))~%")))

           ;; Project manifest: only pass-test.
           (clpm.io.sexp:write-canonical-sexp-to-file
            '(:project
              :name "app"
              :version "0.1.0"
              :systems ("app")
              :run (:system "app" :function "app::ping")
              :test (:systems ("app/pass-test"))
              :depends ()
              :dev-depends ()
              :test-depends ()
              :registries ()
              :scripts ())
            (merge-pathnames "clpm.project" project-root))

           (uiop:with-current-directory (project-root)
             (assert-eql 0 (clpm:run-cli '("test"))))

           ;; Now include a failing test system as well.
           (clpm.io.sexp:write-canonical-sexp-to-file
            '(:project
              :name "app"
              :version "0.1.0"
              :systems ("app")
              :run (:system "app" :function "app::ping")
              :test (:systems ("app/pass-test" "app/fail-test"))
              :depends ()
              :dev-depends ()
              :test-depends ()
              :registries ()
              :scripts ())
            (merge-pathnames "clpm.project" project-root))

           (uiop:with-current-directory (project-root)
             (assert-true (not (zerop (clpm:run-cli '("test"))))
                          "Expected `clpm test` to fail when a test system fails"))))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME"))))

(format t "~%Test command tests PASSED!~%")
(sb-ext:exit :code 0)
