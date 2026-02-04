;;;; test/lisp-selection-test.lisp - `--lisp` selection helpers tests

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

(defun assert-string= (expected actual)
  (unless (string= expected actual)
    (fail "Assertion failed: expected ~S, got ~S" expected actual)))

(defun write-exe (path lines)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8)
    (dolist (l lines)
      (write-string l s)
      (terpri s)))
  (uiop:run-program (list "chmod" "+x" (namestring path)) :ignore-error-status t))

(format t "Testing lisp detection and argv generation...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((bin (merge-pathnames "bin/" tmp))
         (sbcl (merge-pathnames "sbcl" bin))
         (ccl (merge-pathnames "ccl" bin))
         (ecl (merge-pathnames "ecl" bin))
         (old-path (sb-posix:getenv "PATH")))
    (ensure-directories-exist bin)
    (write-exe sbcl '("#!/bin/sh" "echo \"SBCL 9.9.9\""))
    (write-exe ccl '("#!/bin/sh" "echo \"CCL 1.2.3\""))
    (write-exe ecl '("#!/bin/sh" "echo \"ECL 7.7.7\""))

    (unwind-protect
         (progn
           (sb-posix:setenv "PATH"
                            (concatenate 'string (namestring bin) ":" (or old-path ""))
                            1)
           (let ((found (clpm.lisp:find-lisp :sbcl)))
             (assert-true (and (stringp found)
                               (uiop:file-exists-p found)
                               (string= "sbcl" (file-namestring found)))
                          "Expected find-lisp to resolve stub sbcl, got ~S" found))
           (assert-string= "SBCL 9.9.9" (clpm.lisp:lisp-version :sbcl))

           (let* ((argv (clpm.lisp:lisp-run-argv :sbcl
                                                 :load-files (list "/tmp/x.lisp")
                                                 :eval-forms (list "(+ 1 2)")
                                                 :noinform t
                                                 :noninteractive t))
                  (joined (format nil "~{~A~^ ~}" argv)))
             (assert-true (search "--load" joined) "Expected --load in argv: ~S" argv)
             (assert-true (search "--eval" joined) "Expected --eval in argv: ~S" argv)
             (assert-true (search "--non-interactive" joined) "Expected --non-interactive in argv: ~S" argv)))
      (if old-path
          (sb-posix:setenv "PATH" old-path 1)
          (sb-posix:unsetenv "PATH")))))

(format t "~%Lisp selection tests PASSED!~%")
(sb-ext:exit :code 0)
