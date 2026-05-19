;;;; config-merge-test.lisp - merge-project-config must honor :defaults (:lisp ...).

(require :asdf)
(require :sb-posix)

(let* ((this-file (or *load-truename* *load-pathname*))
       (test-dir (uiop:pathname-directory-pathname this-file))
       (repo-root (uiop:pathname-parent-directory-pathname test-dir)))
  (push repo-root asdf:*central-registry*))

(format t "Loading CLPM...~%")
(handler-case
    (asdf:load-system :clpm :verbose nil)
  (error (c)
    (format *error-output* "Failed to load CLPM: ~A~%" c)
    (sb-ext:exit :code 1)))
(format t "CLPM loaded.~%")

(defun fail (fmt &rest args)
  (apply #'format *error-output* (concatenate 'string "FAIL: " fmt "~%") args)
  (sb-ext:exit :code 1))

(defun assert-equal (expected actual ctx)
  (unless (equal expected actual)
    (fail "~A: expected ~S, got ~S" ctx expected actual)))

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)

           ;; 1) No project lisp, no global default -> nil (falls back to sbcl in caller).
           (format t "Empty config -> nil lisp default... ")
           (let ((proj (clpm.project:make-project :name "p" :version "0.1.0")))
             (multiple-value-bind (regs build lisp)
                 (clpm.config:merge-project-config proj)
               (declare (ignore regs build))
               (assert-equal nil lisp "empty"))
             (format t "ok~%"))

           ;; 2) Global :defaults (:lisp "ccl") and no project lisp -> ccl.
           (format t "Global :defaults (:lisp ...) flows through... ")
           (let ((cfg (clpm.config::make-config :format 1
                                               :defaults '(:lisp "ccl"))))
             (clpm.config:write-config cfg))
           (let ((proj (clpm.project:make-project :name "p" :version "0.1.0")))
             (multiple-value-bind (regs build lisp)
                 (clpm.config:merge-project-config proj)
               (declare (ignore regs build))
               (assert-equal "ccl" lisp "global default")))
           (format t "ok~%")

           ;; 3) Project :lisp wins over global default.
           (format t "Project :lisp overrides global default... ")
           (let ((proj (clpm.project:make-project :name "p" :version "0.1.0"
                                                  :lisp "sbcl")))
             (multiple-value-bind (regs build lisp)
                 (clpm.config:merge-project-config proj)
               (declare (ignore regs build))
               (assert-equal "sbcl" lisp "project override")))
           (format t "ok~%")

           ;; 4) :build options plist-merge with project winning per-key.
           (format t "Build options merge per-key... ")
           (let ((cfg (clpm.config::make-config
                       :format 1
                       :defaults '(:build (:debug nil :speed 3 :safety 1)
                                   :lisp "ccl"))))
             (clpm.config:write-config cfg))
           (let ((proj (clpm.project:make-project
                        :name "p" :version "0.1.0"
                        :build-options '(:safety 2))))
             (multiple-value-bind (regs build lisp)
                 (clpm.config:merge-project-config proj)
               (declare (ignore regs lisp))
               ;; :safety from project, :debug + :speed from global default.
               (assert-equal 2 (getf build :safety) "merged safety")
               (assert-equal 3 (getf build :speed) "merged speed")
               (assert-equal nil (getf build :debug) "merged debug")))
           (format t "ok~%"))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "~%Config merge tests PASSED!~%")
(sb-ext:exit :code 0)
