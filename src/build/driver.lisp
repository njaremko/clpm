;;;; build/driver.lisp - Build driver (runs in child Lisp process)

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

(defun generate-build-driver-script (spec output-path &key (lisp-kind :sbcl))
  "Generate a Lisp file that will build according to SPEC."
  (let* ((lisp-kind (clpm.lisp:parse-lisp-kind lisp-kind))
         (quit-ok (ecase lisp-kind
                    (:sbcl "(sb-ext:exit :code 0)")
                    (:ccl "(ccl:quit 0)")
                    (:ecl "(ext:quit 0)")))
         (quit-fail (ecase lisp-kind
                      (:sbcl "(sb-ext:exit :code 1)")
                      (:ccl "(ccl:quit 1)")
                      (:ecl "(ext:quit 1)"))))
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
      (format s "   (t (~S :**/ :*.*.*))~%"
              (namestring (build-spec-output-dir spec)))
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
        (format s "      (format t \"~%Building system: ~A~~%%\" ~S)~%"
                system system)
        (format s "      (asdf:load-system ~S)~%" system))
      (format s "      (format t \"~%Build completed successfully.~~%\")~%")
      (format s "      ~A)~%" quit-ok)
      (format s "  (error (c)~%")
      (format s "    (format *error-output* \"~%Build failed: ~~A~~%%\" c)~%")
      (format s "    ~A)))~%" quit-fail))))

(defun build-driver-argv (lisp-kind driver-path)
  "Return argv for invoking the generated build driver under LISP-KIND."
  (clpm.lisp:lisp-run-argv (clpm.lisp:parse-lisp-kind lisp-kind)
                           :load-files (list (namestring driver-path))
                           :noinform t
                           :noninteractive t
                           :disable-debugger t))

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
          (format s "   ((~S :**/ :*.*.*) (~S :**/ :*.*.*))"
                  (namestring source-dir) (namestring cache-dir))))
      (format s "   :ignore-inherited-configuration))~%"))))

;;; Activate project

(defun %projects-index-path ()
  "Return the path to the global projects index."
  (merge-pathnames "projects.sxp" (clpm.platform:data-dir)))

(defun %read-project-index-roots (path)
  "Read PATH as a projects index and return its :roots list (or NIL)."
  (when (uiop:file-exists-p path)
    (handler-case
        (let* ((form (clpm.io.sexp:read-safe-sexp-from-file path))
               (plist (cdr form)))
          (when (and (consp form) (eq (car form) :projects)
                     (eql (getf plist :format) 1))
            (let ((roots (getf plist :roots)))
              (when (listp roots)
                (remove-duplicates
                 (remove-if-not #'stringp roots)
                 :test #'string=)))))
      (error ()
        nil))))

(defun %write-project-index-roots (path roots)
  "Write ROOTS to PATH in canonical format."
  (ensure-directories-exist path)
  (clpm.io.sexp:write-canonical-sexp-to-file
   `(:projects
     :format 1
     :roots ,(sort (copy-list roots) #'string<))
   path))

(defun %upsert-project-index-root (project-root)
  "Upsert PROJECT-ROOT into the global projects index."
  (let* ((project-root
           (uiop:ensure-directory-pathname
            (truename (uiop:ensure-directory-pathname project-root))))
         (root (namestring project-root))
         (index-path (%projects-index-path))
         (existing (%read-project-index-roots index-path))
         (roots (remove-duplicates (cons root existing) :test #'string=)))
    (%write-project-index-roots index-path roots)))

(defun activate-project (project-root lockfile &key compile-options (lisp-kind :sbcl) lisp-version)
  "Activate a project for use.
Creates .clpm/ directory with ASDF configuration."
  (let* ((clpm-dir (merge-pathnames ".clpm/" project-root))
         (config-path (merge-pathnames "asdf-config.lisp" clpm-dir))
         (env-path (merge-pathnames "env.sexp" clpm-dir)))
    (ensure-directories-exist clpm-dir)
    (%upsert-project-index-root project-root)
    ;; Collect dependency source dirs and build cache dirs
    (let ((dep-source-dirs '())
          (build-cache-dirs '())
          (tree-sha256s '())
          (artifact-sha256s '())
          (build-ids '()))
      (dolist (locked (clpm.project:lockfile-resolved lockfile))
        (let* ((release (clpm.project:locked-system-release locked))
               (tree-sha256 (clpm.project:locked-release-tree-sha256 release))
               (artifact-sha256 (clpm.project:locked-release-artifact-sha256 release))
               (source-path (when tree-sha256
                              (clpm.store:get-source-path tree-sha256))))
          (when (stringp tree-sha256)
            (pushnew tree-sha256 tree-sha256s :test #'string=)
            (pushnew (clpm.store:compute-build-id tree-sha256 compile-options
                                                  :lisp-kind lisp-kind
                                                  :lisp-version lisp-version)
                     build-ids
                     :test #'string=))
          (when (stringp artifact-sha256)
            (pushnew artifact-sha256 artifact-sha256s :test #'string=))
          (when source-path
            (pushnew source-path dep-source-dirs :test #'equal)
            ;; Check for cached build
            (let* ((build-id (clpm.store:compute-build-id
                              tree-sha256 compile-options
                              :lisp-kind lisp-kind
                              :lisp-version lisp-version))
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
           :format 1
           :project-root ,(namestring project-root)
           :generated-at ,(clpm.project:rfc3339-timestamp)
           :lisp-kind ,(clpm.lisp:parse-lisp-kind lisp-kind)
           :lisp-version ,(or lisp-version
                              (case (clpm.lisp:parse-lisp-kind lisp-kind)
                                (:sbcl (clpm.platform:sbcl-version))
                                (t (clpm.lisp:lisp-version (clpm.lisp:parse-lisp-kind lisp-kind)))))
           :dep-count ,(length dep-source-dirs)
           :source-tree-sha256s ,(sort (copy-list tree-sha256s) #'string<)
           :artifact-sha256s ,(sort (copy-list artifact-sha256s) #'string<)
           :build-ids ,(sort (copy-list build-ids) #'string<))
         s)))
    config-path))

;;; Build a single release

(defun build-release (source-path systems tree-sha256 dep-source-dirs
                      &key compile-options (lisp-kind :sbcl) lisp-version)
  "Build a release from SOURCE-PATH.
SYSTEMS is list of system names to build.
TREE-SHA256 is the source tree hash.
DEP-SOURCE-DIRS is list of dependency source directories.
Returns build-id on success, signals error on failure."
  (let* ((lisp-kind (clpm.lisp:parse-lisp-kind lisp-kind))
         (build-id (clpm.store:compute-build-id tree-sha256 compile-options
                                                :lisp-kind lisp-kind
                                                :lisp-version lisp-version))
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
        (generate-build-driver-script spec driver-path :lisp-kind lisp-kind)
        ;; Run build
        (multiple-value-bind (output error-output exit-code)
            (clpm.platform:run-program
             (build-driver-argv lisp-kind driver-path)
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
