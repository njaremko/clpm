;;;; test/manual/quicklisp-live-workflow.lisp - live Quicklisp smoke test (manual)
;;;;
;;;; This test talks to the real Quicklisp dist. It's intentionally NOT part of
;;;; test/all-tests.sh (which runs only test/*-test.lisp at maxdepth 1).
;;;;
;;;; Run manually:
;;;;   CLPM_LIVE_QUICKLISP=1 sbcl --script test/manual/quicklisp-live-workflow.lisp

(require :asdf)
(require :sb-posix)

;; Add repo root directory to ASDF load path.
(let* ((this-file (or *load-truename* *load-pathname*))
       (manual-dir (uiop:pathname-directory-pathname this-file))
       (test-dir (uiop:pathname-parent-directory-pathname manual-dir))
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

(defun write-text (path text)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (write-string text s)))

(let ((live (sb-posix:getenv "CLPM_LIVE_QUICKLISP")))
  (unless (and live (string= live "1"))
    (format t "Skipping live Quicklisp test (set CLPM_LIVE_QUICKLISP=1).~%")
    (sb-ext:exit :code 0)))

(format t "Testing live Quicklisp dist workflow end-to-end...~%")

(let* ((tmp (merge-pathnames (format nil "clpm-live-~D-~D-~D/"
                                     (get-universal-time)
                                     (sb-posix:getpid)
                                     (random (expt 2 32)))
                             (uiop:temporary-directory)))
       (clpm-home (merge-pathnames "clpm-home/" tmp))
       (ws (merge-pathnames "ws/" tmp))
       (old-home (sb-posix:getenv "CLPM_HOME")))
  (ensure-directories-exist clpm-home)
  (ensure-directories-exist ws)
  (unwind-protect
       (progn
         (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)
         (assert-eql 0 (clpm:run-cli (list "registry" "add"
                                           "--quicklisp"
                                           "--name" "quicklisp"
                                           "--url" "https://beta.quicklisp.org/dist/quicklisp.txt")))
         (assert-eql 0 (clpm:run-cli (list "registry" "update" "quicklisp")))

         ;; Create a new project.
         (assert-eql 0 (clpm:run-cli (list "new" "ql-live-app" "--bin" "--dir" (namestring ws))))

         (let* ((app-root (merge-pathnames "ql-live-app/" ws))
                (src-path (merge-pathnames "src/ql-live-app.lisp" app-root))
                (test-path (merge-pathnames "test/ql-live-app-test.lisp" app-root))
                (dist-bin (merge-pathnames "dist/ql-live-app" app-root)))
           (assert-true (uiop:directory-exists-p app-root)
                        "Missing project root: ~A" (namestring app-root))
           (uiop:with-current-directory (app-root)
             ;; Add dependencies from live Quicklisp and install.
             (assert-eql 0 (clpm:run-cli (list "add" "alexandria" "--install")))
             (assert-eql 0 (clpm:run-cli (list "add" "--test" "fiveam" "--install")))

             ;; Ensure project code actually uses Alexandria and FiveAM.
             (write-text
              src-path
              (with-output-to-string (s)
                (format s ";;;; ql-live-app.lisp~%~%")
                (format s "(defpackage #:ql-live-app~%  (:use #:cl)~%  (:export #:main))~%~%")
                (format s "(in-package #:ql-live-app)~%~%")
                (format s "(defun main (&optional (args nil))~%")
                (format s "  (declare (ignore args))~%")
                (format s "  (format t \"~~S~~%\" (alexandria:flatten '((:ok))))~%")
                (format s "  0)~%")))
             (write-text
              test-path
              (with-output-to-string (s)
                (format s ";;;; ql-live-app-test.lisp~%~%")
                (format s "(defpackage #:ql-live-app-test~%  (:use #:cl #:fiveam)~%  (:shadow #:run)~%  (:export #:run))~%~%")
                (format s "(in-package #:ql-live-app-test)~%~%")
                (format s "(defun run ()~%")
                (format s "  (format t \"Running ql-live-app tests...~~%\")~%")
                (format s "  (assert (eql 0 (ql-live-app:main)))~%")
                (format s "  (format t \"ql-live-app tests passed.~~%\")~%")
                (format s "  :ok)~%")))

             ;; Run tests and build a binary.
             (assert-eql 0 (clpm:run-cli (list "test")))
             (assert-eql 0 (clpm:run-cli (list "package")))
             (assert-true (uiop:file-exists-p dist-bin)
                          "Missing packaged binary: ~A" (namestring dist-bin))
             (multiple-value-bind (out err rc)
                 (clpm.platform:run-program (list (namestring dist-bin))
                                            :directory app-root
                                            :output :string
                                            :error-output :string
                                            :timeout 120)
               (declare (ignore err))
               (assert-eql 0 rc)
               (assert-true (search "(:OK)" out :test #'char-equal)
                            "Expected output from dependency, got:~%~A"
                            out))))
         (format t "Live Quicklisp workflow PASSED~%"))
    (if old-home
        (sb-posix:setenv "CLPM_HOME" old-home 1)
        (sb-posix:unsetenv "CLPM_HOME"))))

(format t "~%Live Quicklisp workflow tests PASSED!~%")
(sb-ext:exit :code 0)
