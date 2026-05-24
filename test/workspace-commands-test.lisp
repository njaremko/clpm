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

(defun assert-equal (expected actual)
  (unless (equal expected actual)
    (fail "Assertion failed: expected ~S, got ~S" expected actual)))

(defun find-dep (deps system-id)
  (find system-id deps
        :key #'clpm.project:dependency-system
        :test #'string=))

(defun find-dep-form (deps system-id)
  (find system-id deps
        :key (lambda (dep) (getf (cdr dep) :system))
        :test #'string=))

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

           (multiple-value-bind (code stdout stderr)
               (run-cli-captured (list "project" "workspace" "list"
                                       "--dir" (namestring ws-root)
                                       "--dir" (namestring app-root)))
             (declare (ignore stdout))
             (assert-eql 1 code)
             (assert-contains stderr "Duplicate option: --dir"))

           (uiop:with-current-directory (ws-root)
             (assert-eql 0 (clpm:run-cli '("project" "new" "dep" "--lib")))
             (assert-eql 0 (clpm:run-cli '("project" "new" "app" "--bin"))))

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
                 (run-cli-captured '("deps" "sync" "--to" "lock"))
               (declare (ignore stdout))
               (assert-true (not (zerop code)) "Expected resolve to fail at workspace root without -p")
               (assert-contains stderr "Use -p/--package")
               (assert-contains stderr "Workspace members")))

           ;; Add dep as a workspace-member path dependency to app from workspace root.
           (uiop:with-current-directory (ws-root)
             (assert-eql 0 (clpm:run-cli '("-p" "app" "deps" "add" "--workspace" "dep"))))

           (let* ((manifest (clpm.io.sexp:read-safe-sexp-from-file
                             (merge-pathnames "clpm.project" app-root)))
                  (dep (find-dep-form (getf (cdr manifest) :depends) "dep")))
             (assert-true dep "Expected dep in app depends")
             (assert-equal '(:path "../dep")
                           (getf (cdr dep) :constraint)))

           (uiop:with-current-directory (ws-root)
             (assert-eql 0 (clpm:run-cli '("-p" "app" "deps" "sync"))))

           ;; Ensure project-local artifacts are in the member directory.
           (assert-true (uiop:file-exists-p (merge-pathnames "clpm.lock" app-root))
                        "Expected app/clpm.lock to exist")
           (assert-true (uiop:file-exists-p (merge-pathnames ".clpm/asdf-config.lisp" app-root))
                        "Expected app/.clpm/asdf-config.lisp to exist")
           (assert-true (not (uiop:file-exists-p (merge-pathnames "clpm.lock" ws-root)))
                        "Did not expect workspace-root clpm.lock to exist")

           ;; Member selection is a scoped CLI option, not a deps-local alias.
           (uiop:with-current-directory (ws-root)
             (multiple-value-bind (code _stdout stderr)
                 (run-cli-captured '("deps" "tree" "--package" "app"))
               (declare (ignore _stdout))
               (assert-eql 1 code)
               (assert-contains stderr "Unknown option: --package"))
             (multiple-value-bind (code _stdout stderr)
                 (run-cli-captured '("deps" "why" "dep" "--package" "app"))
               (declare (ignore _stdout))
               (assert-eql 1 code)
               (assert-contains stderr "Unknown option: --package"))
             (multiple-value-bind (code stdout stderr)
                 (run-cli-captured '("-p" "app" "deps" "tree"))
               (declare (ignore stderr))
               (assert-eql 0 code)
               (assert-contains stdout "dep"))
             (multiple-value-bind (code stdout stderr)
                 (run-cli-captured '("-p" "app" "deps" "why" "dep"))
               (declare (ignore stderr))
               (assert-eql 0 code)
               (assert-contains stdout "Why: dep"))
             (multiple-value-bind (code stdout stderr)
                 (run-cli-captured '("-p" "app" "deps" "why" "--workspace" "dep"))
               (declare (ignore stderr))
               (assert-eql 0 code)
               (assert-contains stdout "Why: dep")))

           ;; Workspace member shorthand also resolves dependency targets.
           (uiop:with-current-directory (ws-root)
             (assert-eql 0 (clpm:run-cli '("-p" "app" "deps" "update" "--workspace" "dep"))))

           ;; Run tests for app from workspace root.
           (uiop:with-current-directory (ws-root)
             (assert-eql 0 (clpm:run-cli '("-p" "app" "run" "test"))))

           (uiop:with-current-directory (ws-root)
             (assert-eql 0 (clpm:run-cli '("-p" "app" "deps" "remove" "--workspace" "dep"))))
           (let* ((project (clpm.project:read-project-file
                            (merge-pathnames "clpm.project" app-root)))
                  (dep (find-dep (clpm.project:project-depends project) "dep")))
             (assert-true (null dep) "Did not expect dep after workspace remove")))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "~%Workspace member targeting tests PASSED!~%")
(sb-ext:exit :code 0)
