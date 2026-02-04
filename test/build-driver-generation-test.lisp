;;;; test/build-driver-generation-test.lisp - Build driver generation tests

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
    (asdf:load-system :clpm :force t)
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

(defun assert-string-contains (haystack needle)
  (assert-true (and (stringp haystack)
                    (search needle haystack))
               "Expected to find ~S in:~%~A" needle haystack))

(defun read-file-string (path)
  (uiop:read-file-string path))

(defun write-exe (path lines)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output :if-exists :supersede :external-format :utf-8)
    (dolist (l lines)
      (write-string l s)
      (terpri s)))
  (uiop:run-program (list "chmod" "+x" (namestring path)) :ignore-error-status t))

(format t "Testing build driver generation and argv wiring...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((bin (merge-pathnames "bin/" tmp))
         (sbcl (merge-pathnames "sbcl" bin))
         (ccl (merge-pathnames "ccl" bin))
         (ecl (merge-pathnames "ecl" bin))
         (old-path (sb-posix:getenv "PATH")))
    (ensure-directories-exist bin)
    ;; Stub implementations for argv construction (no real Lisp required).
    (write-exe sbcl '("#!/bin/sh" "echo \"SBCL 9.9.9\""))
    (write-exe ccl '("#!/bin/sh" "echo \"CCL 1.2.3\""))
    (write-exe ecl '("#!/bin/sh" "echo \"ECL 7.7.7\""))

    (unwind-protect
         (progn
           (sb-posix:setenv "PATH"
                            (concatenate 'string (namestring bin) ":" (or old-path ""))
                            1)

           ;; 1) Driver script contains per-impl quit forms.
           (let* ((src (merge-pathnames "src/" tmp))
                  (out (merge-pathnames "out/" tmp))
                  (spec (clpm.build::make-build-spec
                         :source-dir src
                         :systems '("dummy")
                         :output-dir out
                         :compile-options nil
                         :dependency-dirs nil
                         :tree-sha256 "deadbeef"))
                  (sbcl-driver (merge-pathnames "sbcl-driver.lisp" tmp))
                  (ccl-driver (merge-pathnames "ccl-driver.lisp" tmp))
                  (ecl-driver (merge-pathnames "ecl-driver.lisp" tmp)))
             (ensure-directories-exist src)
             (ensure-directories-exist out)

             (clpm.build::generate-build-driver-script spec sbcl-driver :lisp-kind :sbcl)
             (clpm.build::generate-build-driver-script spec ccl-driver :lisp-kind :ccl)
             (clpm.build::generate-build-driver-script spec ecl-driver :lisp-kind :ecl)

             (let ((sbcl-text (read-file-string sbcl-driver))
                   (ccl-text (read-file-string ccl-driver))
                   (ecl-text (read-file-string ecl-driver)))
               (assert-string-contains sbcl-text "(sb-ext:exit :code 0)")
               (assert-string-contains sbcl-text "(sb-ext:exit :code 1)")
               (assert-string-contains ccl-text "(ccl:quit 0)")
               (assert-string-contains ccl-text "(ccl:quit 1)")
               (assert-string-contains ecl-text "(ext:quit 0)")
               (assert-string-contains ecl-text "(ext:quit 1)")))

           ;; 2) Driver invocation argv uses clpm.lisp argv builder semantics.
           (let* ((driver (merge-pathnames "driver.lisp" tmp))
                  (sbcl-argv (clpm.build::build-driver-argv :sbcl driver))
                  (ccl-argv (clpm.build::build-driver-argv :ccl driver))
                  (ecl-argv (clpm.build::build-driver-argv :ecl driver))
                  (sbcl-joined (format nil "~{~A~^ ~}" sbcl-argv))
                  (ccl-joined (format nil "~{~A~^ ~}" ccl-argv))
                  (ecl-joined (format nil "~{~A~^ ~}" ecl-argv)))
             (assert-string-contains sbcl-joined "--load")
             (assert-string-contains ccl-joined "--load")
             (assert-string-contains ecl-joined "-load"))

           ;; 3) Orchestrator passes lisp selection through to build-release.
           (let* ((orig (symbol-function 'clpm.build:build-release))
                  (seen-kind nil)
                  (seen-version nil)
                  (resolution
                    (clpm.solver::make-resolution
                     :systems (list (list :system "a" :name "a" :version "0.1.0" :release-ref "a@0.1.0"))
                     :graph (list (cons "a" nil))))
                  (src-path (merge-pathnames "a/" tmp))
                  (source-paths (list (cons "a" src-path)))
                  (lock
                    (let* ((src (clpm.project:make-locked-source
                                 :kind :path
                                 :path "/dev/null/a"
                                 :sha256 "deadbeef"))
                           (rel (clpm.project:make-locked-release
                                 :name "a"
                                 :version "0.1.0"
                                 :source src
                                 :artifact-sha256 nil
                                 :tree-sha256 "deadbeef")))
                      (clpm.project:make-lockfile
                       :format 1
                       :generated-at (clpm.project:rfc3339-timestamp)
                       :project-name "proj"
                       :clpm-version "0.1.0"
                       :registries nil
                       :resolved (list (clpm.project:make-locked-system
                                        :id "a"
                                        :release rel
                                        :deps nil))))))
             (unwind-protect
                  (progn
                    (setf (symbol-function 'clpm.build:build-release)
                          (lambda (_source-path _systems _tree-sha256 _dep-source-dirs
                                   &key compile-options lisp-kind lisp-version)
                            (declare (ignore _source-path _systems _tree-sha256 _dep-source-dirs compile-options))
                            (setf seen-kind lisp-kind
                                  seen-version lisp-version)
                            "fake-build-id"))
                    (clpm.build:build-all resolution lock source-paths
                                          :jobs 1
                                          :compile-options nil
                                          :lisp-kind :ecl
                                          :lisp-version "ECL 7.7.7")
                    (assert-true (eq seen-kind :ecl) "Expected lisp-kind :ecl, got ~S" seen-kind)
                    (assert-true (string= seen-version "ECL 7.7.7")
                                 "Expected lisp-version ~S, got ~S" "ECL 7.7.7" seen-version))
               (setf (symbol-function 'clpm.build:build-release) orig))))
      (if old-path
          (sb-posix:setenv "PATH" old-path 1)
          (sb-posix:unsetenv "PATH")))))

(format t "~%Build driver generation tests PASSED!~%")
(sb-ext:exit :code 0)
