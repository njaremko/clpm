;;;; build/driver.lisp - Build driver (runs in child SBCL process)

(in-package #:clpm.build)

;;; Build specification

(defstruct build-spec
  "Specification for a build task."
  (source-dir nil :type (or null pathname))
  (systems nil :type list)           ; systems to build
  (output-dir nil :type (or null pathname))
  (compile-options nil :type list)   ; (:speed 3 :safety 1 :debug 0)
  (dependency-dirs nil :type list)   ; list of dependency source dirs
  (tree-sha256 nil :type (or null string)))

;;; Generate build driver script

(defun generate-build-driver-script (spec output-path)
  "Generate a Lisp file that will build according to SPEC."
  (with-open-file (s output-path :direction :output
                                 :if-exists :supersede)
    (format s ";;;; Auto-generated build driver for CLPM~%")
    (format s ";;;; DO NOT EDIT~%~%")
    ;; Configure ASDF
    (format s "(require \"asdf\")~%~%")
    ;; Set up source registry
    (format s "(asdf:initialize-source-registry~%")
    (format s " '(:source-registry~%")
    (format s "   (:tree ~S)~%" (namestring (build-spec-source-dir spec)))
    (dolist (dep-dir (build-spec-dependency-dirs spec))
      (format s "   (:tree ~S)~%" (namestring dep-dir)))
    (format s "   :ignore-inherited-configuration))~%~%")
    ;; Set up output translations
    (format s "(asdf:initialize-output-translations~%")
    (format s " '(:output-translations~%")
    (format s "   (t (~S **/ *.*.*))~%" (namestring (build-spec-output-dir spec)))
    (format s "   :ignore-inherited-configuration))~%~%")
    ;; Set compile policy
    (let ((opts (build-spec-compile-options spec)))
      (format s "(declaim (optimize")
      (format s " (speed ~D)" (or (getf opts :speed) 1))
      (format s " (safety ~D)" (or (getf opts :safety) 1))
      (format s " (debug ~D)" (or (getf opts :debug) 1))
      (format s "))~%~%"))
    ;; Build systems
    (format s "(handler-case~%")
    (format s "    (progn~%")
    (dolist (system (build-spec-systems spec))
      (format s "      (format t \"~%Building system: ~A~~%%\" ~S)~%" system system)
      (format s "      (asdf:load-system ~S)~%" system))
    (format s "      (format t \"~%Build completed successfully.~~%\")~%")
    (format s "      (sb-ext:exit :code 0))~%")
    (format s "  (error (c)~%")
    (format s "    (format *error-output* \"~%Build failed: ~~A~~%%\" c)~%")
    (format s "    (sb-ext:exit :code 1)))~%")))

;;; ASDF configuration for activation

(defun generate-asdf-config (project-root dep-source-dirs build-cache-dirs output-path)
  "Generate ASDF configuration file for project activation."
  (with-open-file (s output-path :direction :output
                                 :if-exists :supersede)
    (format s ";;;; ASDF configuration for CLPM project~%")
    (format s ";;;; Auto-generated - do not edit~%~%")
    (format s "(require \"asdf\")~%~%")
    ;; Source registry
    (format s "(asdf:initialize-source-registry~%")
    (format s " '(:source-registry~%")
    (format s "   (:tree ~S)~%" (namestring project-root))
    (dolist (dir dep-source-dirs)
      (format s "   (:tree ~S)~%" (namestring dir)))
    (format s "   :ignore-inherited-configuration))~%~%")
    ;; Output translations for using cached builds
    (when build-cache-dirs
      (format s "(asdf:initialize-output-translations~%")
      (format s " '(:output-translations~%")
      (dolist (entry build-cache-dirs)
        (let ((source-dir (car entry))
              (cache-dir (cdr entry)))
          (format s "   ((~S **/ *.*.*) (~S **/ *.*.*))"
                  (namestring source-dir) (namestring cache-dir))))
      (format s "   :ignore-inherited-configuration))~%"))))

;;; Activate project

(defun activate-project (project-root lockfile &key compile-options)
  "Activate a project for use.
Creates .clpm/ directory with ASDF configuration."
  (let* ((clpm-dir (merge-pathnames ".clpm/" project-root))
         (config-path (merge-pathnames "asdf-config.lisp" clpm-dir))
         (env-path (merge-pathnames "env.sexp" clpm-dir)))
    (ensure-directories-exist clpm-dir)
    ;; Collect dependency source dirs and build cache dirs
    (let ((dep-source-dirs '())
          (build-cache-dirs '()))
      (dolist (locked (clpm.project:lockfile-resolved lockfile))
        (let* ((release (clpm.project:locked-system-release locked))
               (tree-sha256 (clpm.project:locked-release-tree-sha256 release))
               (source-path (when tree-sha256
                              (clpm.store:get-source-path tree-sha256))))
          (when source-path
            (pushnew source-path dep-source-dirs :test #'equal)
            ;; Check for cached build
            (let* ((build-id (clpm.store:compute-build-id
                              tree-sha256 compile-options))
                   (build-path (clpm.store:get-build-path build-id)))
              (when build-path
                (push (cons source-path
                            (merge-pathnames "fasl/" build-path))
                      build-cache-dirs))))))
      ;; Generate config
      (generate-asdf-config project-root dep-source-dirs build-cache-dirs config-path)
      ;; Write environment info
      (with-open-file (s env-path :direction :output
                                  :if-exists :supersede)
        (clpm.io.sexp:write-canonical-sexp
         `(:env
           :project-root ,(namestring project-root)
           :generated-at ,(clpm.project:rfc3339-timestamp)
           :sbcl-version ,(clpm.platform:sbcl-version)
           :dep-count ,(length dep-source-dirs))
         s)))
    config-path))

;;; Build a single release

(defun build-release (source-path systems tree-sha256 dep-source-dirs
                      &key compile-options)
  "Build a release from SOURCE-PATH.
SYSTEMS is list of system names to build.
TREE-SHA256 is the source tree hash.
DEP-SOURCE-DIRS is list of dependency source directories.
Returns build-id on success, signals error on failure."
  (let* ((build-id (clpm.store:compute-build-id tree-sha256 compile-options))
         (build-cache (clpm.store:get-build-path build-id)))
    ;; Check if already built
    (when build-cache
      (return-from build-release build-id))
    ;; Build in fresh SBCL process
    (clpm.store:with-temp-dir (tmp)
      (let* ((output-dir (merge-pathnames "fasl/" tmp))
             (driver-path (merge-pathnames "build-driver.lisp" tmp))
             (log-path (merge-pathnames (format nil "build/~A.log" build-id)
                                        (clpm.platform:log-dir)))
             (spec (make-build-spec
                    :source-dir source-path
                    :systems systems
                    :output-dir output-dir
                    :compile-options compile-options
                    :dependency-dirs dep-source-dirs
                    :tree-sha256 tree-sha256)))
        (ensure-directories-exist output-dir)
        (ensure-directories-exist log-path)
        ;; Generate driver script
        (generate-build-driver-script spec driver-path)
        ;; Run build
        (multiple-value-bind (output error-output exit-code)
            (clpm.platform:run-program
             (list "sbcl" "--noinform" "--non-interactive"
                   "--disable-debugger"
                   "--load" (namestring driver-path))
             :timeout 600000)  ; 10 minute timeout
          ;; Write log
          (with-open-file (s log-path :direction :output
                                      :if-exists :supersede)
            (format s "Build ID: ~A~%" build-id)
            (format s "Systems: ~{~A~^, ~}~%" systems)
            (format s "Exit code: ~D~%~%" exit-code)
            (format s "=== STDOUT ===~%~A~%~%" output)
            (format s "=== STDERR ===~%~A~%" error-output))
          (unless (zerop exit-code)
            (error 'clpm.errors:clpm-build-error
                   :message "Build process failed"
                   :system (first systems)
                   :log-file log-path
                   :exit-code exit-code)))
        ;; Store build outputs
        (clpm.store:store-build build-id source-path output-dir
                                (list :systems systems
                                      :compile-options compile-options))))
    build-id))
