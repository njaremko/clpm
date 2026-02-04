;;;; test/example-workspace-workflow-test.lisp - End-to-end workspace member workflow

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

(defun write-text (path contents)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (write-string contents s)))

(format t "Testing workspace member workflow...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (base (merge-pathnames "wsbase/" tmp))
         (ws-root (merge-pathnames "myws/" base))
         (lib-name "hello-lib")
         (app-name "hello-app")
         (lib-root (merge-pathnames (format nil "~A/" lib-name) ws-root))
         (app-root (merge-pathnames (format nil "~A/" app-name) ws-root))
         (app-asd (merge-pathnames (format nil "~A/~A.asd" app-name app-name) ws-root))
         (app-src (merge-pathnames (format nil "~A/src/~A.lisp" app-name app-name) ws-root))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist base)
    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)

           ;; Create workspace + members.
           (uiop:with-current-directory (base)
             (assert-eql 0 (clpm:run-cli (list "new" "myws" "--workspace" "--dir" (namestring base)))))
           (assert-true (uiop:directory-exists-p ws-root) "Expected workspace root: ~A" (namestring ws-root))
           (assert-eql 0 (clpm:run-cli (list "new" lib-name "--lib" "--member-of" (namestring ws-root))))
           (assert-eql 0 (clpm:run-cli (list "new" app-name "--bin" "--member-of" (namestring ws-root))))
           (assert-true (uiop:directory-exists-p lib-root) "Expected lib root: ~A" (namestring lib-root))
           (assert-true (uiop:directory-exists-p app-root) "Expected app root: ~A" (namestring app-root))

           ;; Make the app ASDF system depend on the lib, and call it from main.
           (write-text
            app-asd
            (with-output-to-string (s)
              (format s ";;;; ~A.asd~%~%" app-name)
              (format s "(asdf:defsystem ~S~%  :version ~S~%  :depends-on (~S)~%  :serial t~%  :pathname ~S~%  :components ((:file ~S))~%  :in-order-to ((asdf:test-op (asdf:test-op ~S))))~%~%"
                      app-name "0.1.0" lib-name "src" app-name (format nil "~A/test" app-name))
              (format s "(asdf:defsystem ~S~%  :depends-on (~S)~%  :serial t~%  :pathname ~S~%  :components ((:file ~S))~%  :perform (asdf:test-op (op c)~%             (declare (ignore op c))~%             (uiop:symbol-call :~A-test :run)))~%"
                      (format nil "~A/test" app-name)
                      app-name
                      "test"
                      (format nil "~A-test" app-name)
                      app-name)))
           (write-text
            app-src
            (with-output-to-string (s)
              (format s ";;;; ~A.lisp~%~%" app-name)
              (format s "(defpackage #:~A~%  (:use #:cl)~%  (:import-from #:~A #:hello)~%  (:export #:main))~%~%"
                      app-name lib-name)
              (format s "(in-package #:~A)~%~%" app-name)
              (format s "(defun main (&optional (args nil))~%")
              (format s "  (declare (ignore args))~%")
              (format s "  (hello)~%")
              (format s "  0)~%")))

           ;; Add the lib as a path dependency (exercises -p targeting).
           (uiop:with-current-directory (ws-root)
             (assert-eql 0 (clpm:run-cli (list "-p" app-name "add"
                                               "--path" (format nil "../~A" lib-name)
                                               "--install" lib-name))))

           ;; Run tests and run the binary, both targeted from workspace root.
           (uiop:with-current-directory (ws-root)
             (assert-eql 0 (clpm:run-cli (list "-p" app-name "test")))
             (assert-eql 0 (clpm:run-cli (list "-p" app-name "run"))))

           ;; Ensure artifacts stay in the member directory (not the workspace root).
           (assert-true (uiop:file-exists-p (merge-pathnames "clpm.lock" app-root))
                        "Expected app clpm.lock")
           (assert-true (not (uiop:file-exists-p (merge-pathnames "clpm.lock" ws-root)))
                        "Did not expect workspace-root clpm.lock"))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "  Workspace member workflow PASSED~%")
(format t "~%Example workspace workflow tests PASSED!~%")
(sb-ext:exit :code 0)

