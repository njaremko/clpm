;;;; test/workspace-dependencies-test.lisp - Workspace-level dependency declarations

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

(defun assert-equal (expected actual)
  (unless (equal expected actual)
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

(defun find-dep-form (deps system-id)
  (find system-id deps
        :key (lambda (dep) (getf (cdr dep) :system))
        :test #'string=))

(defun locked-source-path (lock system-id)
  (dolist (locked (clpm.project:lockfile-resolved lock))
    (when (string= system-id (clpm.project:locked-system-id locked))
      (let* ((release (clpm.project:locked-system-release locked))
             (source (clpm.project:locked-release-source release)))
        (return (clpm.project:locked-source-path source))))))

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

(format t "Testing workspace-level dependencies...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (ws-root (merge-pathnames "ws/" tmp))
         (app-root (merge-pathnames "app/" ws-root))
         (util-root (merge-pathnames "util/" ws-root))
         (hegel-root (merge-pathnames "hegel-cl/" ws-root))
         (ws-file (merge-pathnames "clpm.workspace" ws-root))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist ws-root)
    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)
           (uiop:with-current-directory (ws-root)
             (assert-eql 0 (clpm:run-cli '("project" "new" "hegel-cl" "--lib")))
             (assert-eql 0 (clpm:run-cli '("project" "new" "util" "--lib")))
             (assert-eql 0 (clpm:run-cli '("project" "new" "app" "--bin"))))

           (clpm.workspace:write-workspace-file
            (clpm.workspace:make-workspace :format 1
                                           :members '("app" "hegel-cl" "util"))
            ws-file)

           (write-text
            (merge-pathnames "util.asd" util-root)
            (with-output-to-string (s)
              (format s ";;;; util.asd~%~%")
              (format s "(asdf:defsystem ~S~%  :version ~S~%  :depends-on (~S)~%  :serial t~%  :pathname ~S~%  :components ((:file ~S)))~%"
                      "util" "0.1.0" "hegel-cl" "src" "util")))
           (write-text
            (merge-pathnames "src/util.lisp" util-root)
            "(defpackage #:util
  (:use #:cl)
  (:import-from #:hegel-cl #:hello)
  (:export #:hello-through-workspace))

(in-package #:util)

(defun hello-through-workspace ()
  (hello))
")
           (write-text
            (merge-pathnames "app.asd" app-root)
            (with-output-to-string (s)
              (format s ";;;; app.asd~%~%")
              (format s "(asdf:defsystem ~S~%  :version ~S~%  :depends-on (~S)~%  :serial t~%  :pathname ~S~%  :components ((:file ~S)))~%"
                      "app" "0.1.0" "util" "src" "app")))
           (write-text
            (merge-pathnames "src/app.lisp" app-root)
            "(defpackage #:app
  (:use #:cl)
  (:import-from #:util #:hello-through-workspace)
  (:export #:main))

(in-package #:app)

(defun main (&optional (args nil))
  (declare (ignore args))
  (hello-through-workspace)
  0)
")

           (uiop:with-current-directory (ws-root)
             (assert-eql 1 (clpm:run-cli '("project" "workspace" "deps" "add"
                                           "--workspace" "hegel-cl"
                                           "hegel-cl@^0.1.0")))
             (assert-eql 0 (clpm:run-cli '("project" "workspace" "deps" "add"
                                           "--workspace" "hegel-cl")))
             (multiple-value-bind (code stdout stderr)
                 (run-cli-captured '("project" "workspace" "deps" "list"))
               (declare (ignore stderr))
               (assert-eql 0 code)
               (assert-contains stdout "hegel-cl")
               (assert-contains stdout "(:PATH \"hegel-cl\")"))
             (assert-eql 0 (clpm:run-cli '("-p" "util" "deps" "add"
                                           "hegel-cl@workspace")))
             (assert-eql 0 (clpm:run-cli '("-p" "app" "deps" "add"
                                           "--workspace" "util"))))

           (let* ((manifest (clpm.io.sexp:read-safe-sexp-from-file
                             (merge-pathnames "clpm.project" util-root)))
                  (dep (find-dep-form (getf (cdr manifest) :depends) "hegel-cl")))
             (assert-true dep "Expected util to depend on hegel-cl")
             (assert-equal "workspace" (getf (cdr dep) :constraint)))

           (uiop:with-current-directory (ws-root)
             (assert-eql 0 (clpm:run-cli '("-p" "app" "deps" "sync")))
             (assert-eql 0 (clpm:run-cli '("-p" "app" "run"))))

           (let* ((lock (clpm.project:read-lock-file
                         (merge-pathnames "clpm.lock" app-root)))
                  (hegel-path (locked-source-path lock "hegel-cl")))
             (assert-equal (namestring (uiop:ensure-directory-pathname
                                        (truename hegel-root)))
                           hegel-path))

           (uiop:with-current-directory (ws-root)
             (assert-eql 0 (clpm:run-cli '("project" "workspace" "deps" "remove"
                                           "hegel-cl"))))
           (let ((ws (clpm.workspace:read-workspace-file ws-file)))
             (assert-true (null (clpm.workspace:workspace-depends ws))
                          "Expected workspace dependency removal")))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "~%Workspace dependency tests PASSED!~%")
(sb-ext:exit :code 0)
