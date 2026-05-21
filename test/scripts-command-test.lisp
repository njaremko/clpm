;;;; test/scripts-command-test.lisp - `clpm run scripts` tests

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

(defun assert-equal (expected actual)
  (unless (equal expected actual)
    (fail "Assertion failed: expected ~S, got ~S" expected actual)))

(defun assert-contains (haystack needle)
  (assert-true (and (stringp haystack)
                    (search needle haystack :test #'char-equal))
               "Expected output to contain ~S, got:~%~A"
               needle
               haystack))

(defun assert-not-contains (haystack needle)
  (assert-true (not (and (stringp haystack)
                         (search needle haystack :test #'char-equal)))
               "Expected output not to contain ~S, got:~%~A"
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

(defun write-file (path contents)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (write-string contents s)))

(format t "Testing `clpm run scripts`...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (project-root (merge-pathnames "app/" tmp))
         (src-dir (merge-pathnames "src/" project-root))
         (shell-args-path (merge-pathnames "shell-script-args.txt" project-root))
         (lisp-args-path (merge-pathnames "lisp-script-args.sexp" project-root))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist src-dir)

    (format t "Testing `clpm run script` argv validation...~%")
    (uiop:with-current-directory (tmp)
      (multiple-value-bind (code stdout stderr)
          (run-cli-captured '("run" "script"))
        (declare (ignore stdout))
        (assert-eql 1 code)
        (assert-contains stderr "Usage: clpm run script <name> [-- <args...>]")
        (assert-not-contains stderr "No clpm.project found")))
    (format t "  `clpm run script` argv validation PASSED~%")

    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)

           ;; Minimal ASDF system exposing a lisp script entrypoint.
           (write-file
            (merge-pathnames "scriptapp.asd" project-root)
            (format nil "~S~%"
                    '(asdf:defsystem "scriptapp"
                       :version "0.1.0"
                       :serial t
                       :pathname "src"
                       :components ((:file "scriptapp")))))
           (write-file
            (merge-pathnames "src/scriptapp.lisp" project-root)
            (with-output-to-string (s)
              (format s "(defpackage #:scriptapp (:use #:cl) (:export #:script-main))~%")
              (format s "(in-package #:scriptapp)~%")
              (format s "(defun script-main (args)~%")
              (format s "  (with-open-file (s \"lisp-script-args.sexp\" :direction :output :if-exists :supersede :external-format :utf-8)~%")
              (format s "    (prin1 args s)~%")
              (format s "    (terpri s))~%")
              (format s "  19)~%")))

           ;; Project manifest with scripts.
           (clpm.io.sexp:write-canonical-sexp-to-file
            `(:project
              :name "app"
              :version "0.1.0"
              :systems ("scriptapp")
              :run nil
              :test nil
              :package nil
              :depends ()
              :dev-depends ()
              :test-depends ()
              :registries ()
              :scripts ((:script :name "zzz" :type :shell :command ("sh" "-c" "exit 0"))
                        (:script :name "args" :type :shell :command ("sh" "-c" "printf '%s\\n' \"$@\" > shell-script-args.txt" "sh"))
                        (:script :name "aaa" :type :lisp :system "scriptapp" :function "scriptapp::script-main")) )
            (merge-pathnames "clpm.project" project-root))

           ;; list: names sorted
           (uiop:with-current-directory (project-root)
             (let* ((out (with-output-to-string (s)
                           (let ((*standard-output* s))
                             (assert-eql 0 (clpm:run-cli '("run" "scripts"))))))
                    (lines (remove "" (uiop:split-string out :separator '(#\Newline))
                                   :test #'string=)))
               (assert-equal '("aaa" "args" "zzz") lines)))

           ;; run shell script args with forwarded args
           (when (uiop:file-exists-p shell-args-path)
             (delete-file shell-args-path))
           (uiop:with-current-directory (project-root)
             (assert-eql 0 (clpm:run-cli '("run" "script" "args" "--" "one" "two"))))
           (assert-true (uiop:file-exists-p shell-args-path)
                        "Missing shell args file: ~A" (namestring shell-args-path))
           (assert-equal (format nil "one~%two~%")
                         (uiop:read-file-string shell-args-path))

           ;; run lisp script args with forwarded args, assert exit code and args file
           (when (uiop:file-exists-p lisp-args-path)
             (delete-file lisp-args-path))
           (uiop:with-current-directory (project-root)
             (assert-eql 19 (clpm:run-cli '("run" "script" "aaa" "--" "hi" "there"))))
           (assert-true (uiop:file-exists-p lisp-args-path)
                        "Missing lisp args file: ~A" (namestring lisp-args-path))
           (let ((args (clpm.io.sexp:read-safe-sexp-from-file lisp-args-path)))
             (assert-equal '("hi" "there") args)))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "~%Scripts command tests PASSED!~%")
(sb-ext:exit :code 0)
