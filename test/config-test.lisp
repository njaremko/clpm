;;;; test/config-test.lisp - Global config loading/merge tests

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

(format t "Testing global config merge...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (project-root (merge-pathnames "proj/" tmp))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist project-root)
    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)
           ;; Write global config.
           (ensure-directories-exist (merge-pathnames "config.sxp" (clpm.platform:config-dir)))
           (clpm.io.sexp:write-canonical-sexp-to-file
            '(:config
              :format 1
              :registries ((:git :url "https://example.invalid/global.git"
                                 :name "global"
                                 :trust nil))
              :defaults (:build (:jobs 2 :compile (:speed 2))))
            (merge-pathnames "config.sxp" (clpm.platform:config-dir)))
           ;; Write project manifest.
           (clpm.io.sexp:write-canonical-sexp-to-file
            '(:project
              :name "proj"
              :version "0.1.0"
              :systems ("proj")
              :depends ()
              :registries ((:git :url "https://example.invalid/project.git"
                                 :name "project"
                                 :trust nil))
              :build (:jobs 4))
            (merge-pathnames "clpm.project" project-root))

           (let* ((project (clpm.project:read-project-file
                            (merge-pathnames "clpm.project" project-root)))
                  (cfg (clpm.config:read-config)))
             (multiple-value-bind (refs build)
                 (clpm.config:merge-project-config project :config cfg)
               (assert-true (equal (mapcar #'clpm.project:registry-ref-name refs)
                                   '("global" "project"))
                            "Expected merged registries to be global then project")
               (assert-true (= (getf build :jobs) 4)
                            "Expected project build settings to override globals")
               (assert-true (equal (getf build :compile) '(:speed 2))
                            "Expected global build settings to be preserved"))))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "  Config merge PASSED~%")
(format t "~%Config tests PASSED!~%")
(sb-ext:exit :code 0)
