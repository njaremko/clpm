;;;; test/workspace-commands-test.lisp - Workspace member targeting (-p/--package) tests

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

(defun write-text (path contents)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (write-string contents s)))

(format t "Testing workspace member targeting...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (ws-root (merge-pathnames "ws/" tmp))
         (app-root (merge-pathnames "app/" ws-root))
         (dep-root (merge-pathnames "dep/" ws-root))
         (ws-file (merge-pathnames "clpm.workspace" ws-root))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist ws-root)

    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)

           (uiop:with-current-directory (ws-root)
             (assert-eql 0 (clpm:run-cli '("new" "dep" "--lib")))
             (assert-eql 0 (clpm:run-cli '("new" "app" "--bin"))))

           ;; Create workspace file listing members.
           (clpm.workspace:write-workspace-file
            (clpm.workspace:make-workspace :format 1 :members '("app" "dep"))
            ws-file)

           ;; Make app ASDF system depend on dep, and call dep from app:main.
           (write-text
            (merge-pathnames "app.asd" app-root)
            (with-output-to-string (s)
              (format s ";;;; app.asd~%~%")
              (format s "(asdf:defsystem ~S~%  :version ~S~%  :depends-on (~S)~%  :serial t~%  :pathname ~S~%  :components ((:file ~S))~%  :in-order-to ((asdf:test-op (asdf:test-op ~S))))~%~%"
                      "app" "0.1.0" "dep" "src" "app" "app/test")
              (format s "(asdf:defsystem ~S~%  :depends-on (~S)~%  :serial t~%  :pathname ~S~%  :components ((:file ~S))~%  :perform (asdf:test-op (op c)~%             (declare (ignore op c))~%             (uiop:symbol-call :app-test :run)))~%"
                      "app/test" "app" "test" "app-test")))

           (write-text
            (merge-pathnames "src/app.lisp" app-root)
            (with-output-to-string (s)
              (format s ";;;; app.lisp~%~%")
              (format s "(defpackage #:app (:use #:cl) (:import-from #:dep #:hello) (:export #:main))~%")
              (format s "(in-package #:app)~%~%")
              (format s "(defun main (&optional (args nil))~%  (declare (ignore args))~%  (hello)~%  0)~%")))

           ;; From workspace root, require -p for project commands.
           (uiop:with-current-directory (ws-root)
             (multiple-value-bind (code stdout stderr)
                 (run-cli-captured '("resolve"))
               (declare (ignore stdout))
               (assert-true (not (zerop code)) "Expected resolve to fail at workspace root without -p")
               (assert-contains stderr "Use -p/--package")
               (assert-contains stderr "Workspace members")))

           ;; Add dep as a path dependency to app from workspace root.
           (uiop:with-current-directory (ws-root)
             (assert-eql 0 (clpm:run-cli '("-p" "app" "add" "--path" "../dep" "--install" "dep"))))

           ;; Ensure project-local artifacts are in the member directory.
           (assert-true (uiop:file-exists-p (merge-pathnames "clpm.lock" app-root))
                        "Expected app/clpm.lock to exist")
           (assert-true (uiop:file-exists-p (merge-pathnames ".clpm/asdf-config.lisp" app-root))
                        "Expected app/.clpm/asdf-config.lisp to exist")
           (assert-true (not (uiop:file-exists-p (merge-pathnames "clpm.lock" ws-root)))
                        "Did not expect workspace-root clpm.lock to exist")

           ;; Run tests for app from workspace root.
           (uiop:with-current-directory (ws-root)
             (assert-eql 0 (clpm:run-cli '("-p" "app" "test")))))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "~%Workspace member targeting tests PASSED!~%")
(sb-ext:exit :code 0)

