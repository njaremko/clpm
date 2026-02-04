;;;; commands.lisp - CLI command implementations

(in-package #:clpm.commands)

;;; Global options (set from main)

(defvar *verbose* nil "Enable verbose output")
(defvar *offline* nil "Offline mode - fail if artifacts missing")
(defvar *insecure* nil "Skip signature verification")
(defvar *jobs* 1 "Number of parallel jobs")
(defvar *lisp* nil "Selected Lisp implementation kind (:sbcl/:ccl/:ecl), from --lisp.")
(defvar *target-package* nil "Workspace member to target (from -p/--package).")

;;; Helper functions

(defun log-info (format-string &rest args)
  "Print info message."
  (format t "~&~?~%" format-string args))

(defun log-verbose (format-string &rest args)
  "Print verbose message if verbose mode enabled."
  (when *verbose*
    (format t "~&  ~?~%" format-string args)))

(defun log-error (format-string &rest args)
  "Print error message."
  (format *error-output* "~&error: ~?~%" format-string args))

(defun %searched-parent-directories (&optional (start (uiop:getcwd)))
  "Return a list of directory pathnames searched when looking for clpm.project."
  (let ((dirs '())
        (dir (uiop:ensure-directory-pathname start)))
    (loop
      (push dir dirs)
      (let ((parent (uiop:pathname-parent-directory-pathname dir)))
        (when (or (null parent) (equal parent dir))
          (return (nreverse dirs)))
        (setf dir parent)))))

(defun log-no-project-found (&optional (start (uiop:getcwd)))
  "Print an actionable 'no clpm.project found' error, including searched paths."
  (log-error "No clpm.project found")
  (format *error-output* "Searched for clpm.project in:~%")
  (dolist (d (%searched-parent-directories start))
    (format *error-output* "  ~A~%" (namestring d))))

;;; Registry loading (global + project)

(defun load-merged-registries ()
  "Load merged registries (global config plus project registries when in a project).

When not in a project, only the global config registries are used."
  (clpm.platform:ensure-directories)
  (multiple-value-bind (_project-root manifest-path _lock-path)
      (clpm.project:find-project-root)
    (declare (ignore _project-root _lock-path))
    (let ((refs
            (if manifest-path
                (let ((project (clpm.project:read-project-file manifest-path)))
                  (nth-value 0 (clpm.config:merge-project-config project)))
                (clpm.config:config-registries (clpm.config:read-config)))))
      (loop for ref in refs
            collect
            (clpm.registry:clone-registry
             (clpm.project:registry-ref-name ref)
             (clpm.project:registry-ref-url ref)
             :trust-key (clpm.project:registry-ref-trust ref)
             :quicklisp-systems-sha256 (clpm.project:registry-ref-quicklisp-systems-sha256 ref)
             :quicklisp-releases-sha256 (clpm.project:registry-ref-quicklisp-releases-sha256 ref)
             :kind (clpm.project:registry-ref-kind ref))))))

;;; init command

(defun cmd-init (&key name)
  "Initialize a new CLPM project in current directory."
  (let* ((cwd (uiop:getcwd))
         (manifest-path (merge-pathnames "clpm.project" cwd))
         (project-name (or name
                           (file-namestring
                            (uiop:pathname-directory-pathname cwd)))))
    ;; Check if already exists
    (when (uiop:file-exists-p manifest-path)
      (log-error "clpm.project already exists")
      (return-from cmd-init 1))
    ;; Detect .asd files
    (let ((asd-files (directory (merge-pathnames "*.asd" cwd)))
          (systems '()))
      (dolist (asd asd-files)
        (push (pathname-name asd) systems))
      ;; Create manifest
      (let ((project (clpm.project:make-project
                      :name project-name
                      :version "0.1.0"
                      :systems (or (nreverse systems) (list project-name)))))
        (clpm.project:write-project-file project manifest-path))
      (log-info "Initialized CLPM project: ~A" project-name)
      (log-info "Created clpm.project")
      (when systems
        (log-info "Detected systems: ~{~A~^, ~}" systems)))
    0))

;;; new command

(defun cmd-new (&rest args)
  "Create a new project scaffold."
  (labels ((write-text (path text)
             (ensure-directories-exist path)
             (with-open-file (s path :direction :output
                                     :if-exists :supersede
                                     :external-format :utf-8)
               (write-string text s)))
           (usage-error (fmt &rest fmt-args)
             (apply #'log-error fmt fmt-args)
             (log-error "Usage:")
             (log-error "  clpm new <name> --workspace [--dir <path>]")
             (log-error "  clpm new <name> --bin|--lib [--dir <path>]")
             (log-error "  clpm new <name> --bin|--lib --member-of <workspace-dir>")
             (return-from cmd-new 1))
           (ensure-dir-arg (path)
             (uiop:ensure-directory-pathname
              (uiop:ensure-pathname (clpm.platform:expand-path path)
                                    :defaults (uiop:getcwd)
                                    :want-existing nil))))
    (let ((name (first args))
          (kind nil)
          (dir nil)
          (workspace-p nil)
          (member-of nil))
      (unless (and name (plusp (length name)) (not (char= (char name 0) #\-)))
        (usage-error "Missing project name"))
      ;; Parse flags
      (let ((i 1))
        (loop while (< i (length args)) do
          (let ((arg (nth i args)))
            (cond
              ((string= arg "--workspace")
               (setf workspace-p t))
              ((string= arg "--bin")
               (when kind
                 (usage-error "Only one of --bin or --lib may be specified"))
               (setf kind :bin))
              ((string= arg "--lib")
               (when kind
                 (usage-error "Only one of --bin or --lib may be specified"))
               (setf kind :lib))
              ((string= arg "--dir")
               (incf i)
               (when (>= i (length args))
                 (usage-error "Missing value for --dir"))
               (setf dir (nth i args)))
              ((string= arg "--member-of")
               (incf i)
               (when (>= i (length args))
                 (usage-error "Missing value for --member-of"))
               (setf member-of (nth i args)))
              (t
               (usage-error "Unknown option: ~A" arg))))
          (incf i)))

      (when (and workspace-p kind)
        (usage-error "--workspace may not be combined with --bin/--lib"))
      (when (and workspace-p member-of)
        (usage-error "--workspace may not be combined with --member-of"))

      (labels ((compute-base ()
                 (if dir
                     (ensure-dir-arg dir)
                     (uiop:getcwd)))
               (make-project-scaffold (project-root)
                 (let* ((src-dir (merge-pathnames "src/" project-root))
                        (test-dir (merge-pathnames "test/" project-root))
                        (asd-path (merge-pathnames (format nil "~A.asd" name) project-root))
                        (src-path (merge-pathnames (format nil "src/~A.lisp" name) project-root))
                        (test-path (merge-pathnames (format nil "test/~A-test.lisp" name) project-root))
                        (manifest-path (merge-pathnames "clpm.project" project-root)))
                   (when (uiop:directory-exists-p project-root)
                     (log-error "Destination already exists: ~A" (namestring project-root))
                     (return-from cmd-new 1))
                   (ensure-directories-exist src-dir)
                   (ensure-directories-exist test-dir)

                   ;; clpm.project
                   (let ((project (clpm.project:make-project
                                   :name name
                                   :version "0.1.0"
                                   :systems (list name)
                                   :run (when (eq kind :bin)
                                          (list :system name
                                                :function (format nil "~A::main" name)))
                                   :test (list :systems (list (format nil "~A/test" name)))
                                   :package (when (eq kind :bin)
                                              (list :output (format nil "dist/~A" name)
                                                    :system name
                                                    :function (format nil "~A::main" name)))
                                   :scripts nil)))
                     (clpm.project:write-project-file project manifest-path))

                   ;; ASDF system + test system.
                   (write-text
                    asd-path
                    (with-output-to-string (s)
                      (format s ";;;; ~A.asd~%~%" name)
                      (format s "(asdf:defsystem ~S~%  :version ~S~%  :serial t~%  :pathname ~S~%  :components ((:file ~S))~%  :in-order-to ((asdf:test-op (asdf:test-op ~S))))~%~%"
                              name "0.1.0" "src" name (format nil "~A/test" name))
                      (format s "(asdf:defsystem ~S~%  :depends-on (~S)~%  :serial t~%  :pathname ~S~%  :components ((:file ~S))~%  :perform (asdf:test-op (op c)~%             (declare (ignore op c))~%             (uiop:symbol-call :~A-test :run)))~%"
                              (format nil "~A/test" name)
                              name
                              "test"
                              (format nil "~A-test" name)
                              name)))

                   ;; src/<name>.lisp
                   (write-text
                    src-path
                    (with-output-to-string (s)
                      (format s ";;;; ~A.lisp~%~%" name)
                      (format s "(defpackage #:~A~%  (:use #:cl)~%  (:export #:~A))~%~%"
                              name (if (eq kind :bin) "main" "hello"))
                      (format s "(in-package #:~A)~%~%" name)
                      (ecase kind
                        (:bin
                         (format s "(defun main (&optional (args nil))~%  (declare (ignore args))~%  (format t \"Hello from ~A!~%\")~%  0)~%"
                                 name))
                        (:lib
                         (format s "(defun hello ()~%  :ok)~%")))))

                   ;; test/<name>-test.lisp
                   (write-text
                    test-path
                    (with-output-to-string (s)
                      (format s ";;;; ~A-test.lisp~%~%" name)
                      (format s "(defpackage #:~A-test~%  (:use #:cl)~%  (:export #:run))~%~%"
                              name)
                      (format s "(in-package #:~A-test)~%~%" name)
                      (format s "(defun run ()~%  (format t \"Running tests for ~A...~%\")~%  (assert ~A)~%  (format t \"All tests passed.~%\"))~%"
                              name
                              (if (eq kind :bin)
                                  (format nil "(eql 0 (uiop:symbol-call :~A :main))" name)
                                  (format nil "(eql :ok (uiop:symbol-call :~A :hello))" name))))))))
        (cond
          (workspace-p
           (let* ((base (compute-base))
                  (ws-root (merge-pathnames (format nil "~A/" name) base))
                  (ws-path (merge-pathnames "clpm.workspace" ws-root))
                  (readme-path (merge-pathnames "README.md" ws-root)))
             (when (uiop:directory-exists-p ws-root)
               (log-error "Destination already exists: ~A" (namestring ws-root))
               (return-from cmd-new 1))
             (ensure-directories-exist ws-path)
             (clpm.workspace:write-workspace-file
              (clpm.workspace:make-workspace :format 1 :members '())
              ws-path)
             (write-text
              readme-path
              (with-output-to-string (s)
                (format s "# ~A~%~%" name)
                (format s "This is a CLPM workspace.~%~%")
                (format s "## Common commands~%~%")
                (format s "Add a binary member:~%~%")
                (format s "  clpm new app --bin --member-of .~%~%")
                (format s "Target a member:~%~%")
                (format s "  clpm -p app install~%")
                (format s "  clpm -p app test~%")))
             (log-info "Created workspace: ~A" (namestring ws-root))
             0))
          (t
           (unless kind
             (usage-error "Missing project kind: specify --bin or --lib"))
           (when (and member-of dir)
             (usage-error "--dir may not be combined with --member-of"))
           (if member-of
               (let* ((ws-root (ensure-dir-arg member-of))
                      (ws-path (merge-pathnames "clpm.workspace" ws-root)))
                 (unless (uiop:file-exists-p ws-path)
                   (log-error "Workspace not found: ~A" (namestring ws-root))
                   (log-error "Expected to find: ~A" (namestring ws-path))
                   (return-from cmd-new 1))
                 (let* ((project-root (merge-pathnames (format nil "~A/" name) ws-root))
                        (member-rel
                          (string-right-trim
                           '(#\/ #\\)
                           (namestring
                            (uiop:ensure-directory-pathname
                             (uiop:enough-pathname project-root ws-root))))))
                   (make-project-scaffold project-root)
                   (let* ((ws (clpm.workspace:read-workspace-file ws-path))
                          (members (remove-duplicates
                                    (append (clpm.workspace:workspace-members ws)
                                            (list member-rel))
                                    :test #'string=)))
                     (setf (clpm.workspace:workspace-members ws)
                           (sort members #'string<))
                     (clpm.workspace:write-workspace-file ws ws-path))
                   (log-info "Added workspace member: ~A" member-rel)
                   (log-info "Created ~A project: ~A"
                             (string-downcase (symbol-name kind))
                             (namestring project-root))
                   0))
               (let* ((base (compute-base))
                      (project-root (merge-pathnames (format nil "~A/" name) base)))
                 (make-project-scaffold project-root)
                 (log-info "Created ~A project: ~A"
                           (string-downcase (symbol-name kind))
                           (namestring project-root))
                 0))))))))

;;; add/remove commands

(defun parse-dep-spec (spec)
  "Parse a dependency spec like:
  <system>
  <system>@^<semver>
  <system>@=<exact>
Returns (values system-id constraint-form-or-nil)."
  (let ((at (position #\@ spec)))
    (if (null at)
        (values spec nil)
        (let* ((system (subseq spec 0 at))
               (rest (subseq spec (1+ at))))
          (cond
            ((and (plusp (length rest)) (char= (char rest 0) #\^))
             (values system (list :semver rest)))
            ((and (plusp (length rest)) (char= (char rest 0) #\=))
             (values system (list :exact (subseq rest 1))))
            (t
             (values system :invalid)))))))

(defun highest-system-version (registries system-id)
  "Return the highest version string available for SYSTEM-ID across REGISTRIES."
  (let* ((index (clpm.registry:build-registry-index registries))
         (entries (clpm.registry:index-lookup-system index system-id))
         (best nil))
    (dolist (entry entries)
      (let ((release-ref (cdr entry)))
        (when (stringp release-ref)
          (let ((at (position #\@ release-ref)))
            (when at
              (let ((ver (subseq release-ref (1+ at))))
                (when (or (null best)
                          (clpm.solver.version:version> ver best))
                  (setf best ver))))))))
    best))

(defun sorted-deps (deps)
  (sort (copy-list deps) #'string< :key #'clpm.project:dependency-system))

(defun %sha256-hex-of-string (s)
  (clpm.crypto.sha256:bytes-to-hex
   (clpm.crypto.sha256:sha256 s)))

(defun %canonical-sexp-sha256 (sexp)
  (%sha256-hex-of-string
   (clpm.io.sexp:write-canonical-sexp-to-string sexp)))

(defun %registry-input-sexp (refs)
  (let ((items
          (loop for ref in refs
                collect
                `(:registry
                  :name ,(clpm.project:registry-ref-name ref)
                  :kind ,(clpm.project:registry-ref-kind ref)
                  :url ,(clpm.project:registry-ref-url ref)
                  :trust ,(clpm.project:registry-ref-trust ref)
                  :systems-sha256 ,(clpm.project:registry-ref-quicklisp-systems-sha256 ref)
                  :releases-sha256 ,(clpm.project:registry-ref-quicklisp-releases-sha256 ref)))))
    (setf items
          (sort items
                (lambda (a b)
                  (string< (or (getf (cdr a) :name) "")
                           (or (getf (cdr b) :name) "")))))
    `(:registries :items ,items)))

(defun find-effective-project-root (&optional (start (uiop:getcwd)))
  "Find the effective project root for commands that operate on a project.

If invoked from a workspace root (or a directory within a workspace with no
nearest clpm.project), requires *target-package* to be set and resolves the
workspace member directory.

Returns (values project-root manifest-path lock-path workspace-root workspace-path)."
  (multiple-value-bind (root kind manifest lock workspace-path)
      (clpm.workspace:find-project-or-workspace-root start)
    (cond
      ((null kind)
       (values nil nil nil nil nil))
      ((eq kind :project)
       (values root manifest lock nil nil))
      ((eq kind :workspace)
       (let* ((ws (clpm.workspace:read-workspace-file workspace-path))
              (members (sort (copy-list (or (clpm.workspace:workspace-members ws) '()))
                             #'string<))
              (member *target-package*))
         (unless member
           (log-error "Workspace root detected. Use -p/--package to select a member.")
           (if (null members)
               (log-error "Workspace has no members")
               (progn
                 (log-error "Workspace members:")
                 (dolist (m members)
                   (log-error "  ~A" m))))
           (return-from find-effective-project-root
             (values nil nil nil root workspace-path)))
         (unless (member member members :test #'string=)
           (log-error "Workspace member ~A not found. Members:" member)
           (dolist (m members)
             (log-error "  ~A" m))
           (return-from find-effective-project-root
             (values nil nil nil root workspace-path)))
         (let* ((member-dir
                  (uiop:ensure-directory-pathname
                   (uiop:ensure-pathname (merge-pathnames member root)
                                         :defaults root
                                         :want-existing nil)))
                (member-manifest (merge-pathnames "clpm.project" member-dir)))
           (unless (uiop:file-exists-p member-manifest)
             (log-error "Workspace member ~A does not contain clpm.project: ~A"
                        member (namestring member-dir))
             (return-from find-effective-project-root
               (values nil nil nil root workspace-path)))
           (multiple-value-bind (proj-root proj-manifest proj-lock)
               (clpm.project:find-project-root member-dir)
             (declare (ignore proj-root))
             (values member-dir proj-manifest proj-lock root workspace-path)))))
      (t
       (values nil nil nil nil nil)))))

(defun cmd-add (&rest args)
  "Add a dependency to clpm.project and update clpm.lock."
  (multiple-value-bind (project-root manifest-path lock-path workspace-root _workspace-path)
      (find-effective-project-root)
    (declare (ignore lock-path _workspace-path))
    (unless manifest-path
      (when (null workspace-root)
        (log-no-project-found))
      (return-from cmd-add 1))

    (let ((spec nil)
          (dev-p nil)
          (test-p nil)
          (install-p nil)
          (any-p nil)
          (caret-p nil)
          (registry-name nil)
          (path nil)
          (git-url nil)
          (git-ref nil))
      ;; Parse args
      (let ((i 0))
        (loop while (< i (length args)) do
          (let ((arg (nth i args)))
            (cond
              ((string= arg "--dev")
               (setf dev-p t))
              ((string= arg "--test")
               (setf test-p t))
              ((string= arg "--install")
               (setf install-p t))
              ((string= arg "--any")
               (setf any-p t))
              ((string= arg "--caret")
               (setf caret-p t))
              ((string= arg "--registry")
               (incf i)
               (when (>= i (length args))
                 (log-error "Missing value for --registry")
                 (return-from cmd-add 1))
               (when registry-name
                 (log-error "Duplicate option: --registry")
                 (return-from cmd-add 1))
               (setf registry-name (nth i args)))
              ((string= arg "--path")
               (incf i)
               (when (>= i (length args))
                 (log-error "Missing value for --path")
                 (return-from cmd-add 1))
               (when path
                 (log-error "Duplicate option: --path")
                 (return-from cmd-add 1))
               (setf path (nth i args)))
              ((string= arg "--git")
               (incf i)
               (when (>= i (length args))
                 (log-error "Missing value for --git")
                 (return-from cmd-add 1))
               (when git-url
                 (log-error "Duplicate option: --git")
                 (return-from cmd-add 1))
               (setf git-url (nth i args)))
              ((string= arg "--ref")
               (incf i)
               (when (>= i (length args))
                 (log-error "Missing value for --ref")
                 (return-from cmd-add 1))
               (when git-ref
                 (log-error "Duplicate option: --ref")
                 (return-from cmd-add 1))
               (setf git-ref (nth i args)))
              ((and (plusp (length arg)) (char= (char arg 0) #\-))
               (log-error "Unknown option: ~A" arg)
               (return-from cmd-add 1))
              ((null spec)
               (setf spec arg))
              (t
               (log-error "Unexpected argument: ~A" arg)
               (return-from cmd-add 1))))
          (incf i)))

      (unless spec
        (log-error "Usage: clpm add [--dev|--test] [--any|--caret] [--registry <name>] [--path <dir> | --git <url> --ref <ref>] <system>[@^<semver>|@=<exact>]")
        (return-from cmd-add 1))

      (when (and any-p caret-p)
        (log-error "Only one of --any or --caret may be specified")
        (return-from cmd-add 1))

      (when (and dev-p test-p)
        (log-error "Only one of --dev or --test may be specified")
        (return-from cmd-add 1))

      (when (and path git-url)
        (log-error "Only one of --path or --git may be specified")
        (return-from cmd-add 1))

      (when (and registry-name (or path git-url))
        (log-error "Do not combine --registry with --path/--git")
        (return-from cmd-add 1))

      (when (and git-url (null git-ref))
        (log-error "--git requires --ref")
        (return-from cmd-add 1))

      (when (and git-ref (null git-url))
        (log-error "--ref requires --git")
        (return-from cmd-add 1))

      (let* ((project (clpm.project:read-project-file manifest-path))
             (registries (load-project-registries project))
             (section (cond
                        (dev-p :dev-depends)
                        (test-p :test-depends)
                        (t :depends)))
             (system-id nil)
             (constraint-form nil)
             (dep-source nil))
        (multiple-value-bind (sys parsed-constraint)
            (parse-dep-spec spec)
          (setf system-id sys)
          (when (eq parsed-constraint :invalid)
            (log-error "Invalid dependency spec: ~A" spec)
            (return-from cmd-add 1))
          (when (and parsed-constraint (or path git-url))
            (log-error "Do not combine @<constraint> with --path/--git")
            (return-from cmd-add 1))
          (when (and parsed-constraint (or any-p caret-p))
            (log-error "Do not combine @<constraint> with --any/--caret")
            (return-from cmd-add 1))

          ;; Registry disambiguation for non-pinned sources.
          (when (and (null path) (null git-url))
            (let* ((index (clpm.registry:build-registry-index registries))
                   (entries (clpm.registry:index-lookup-system index system-id))
                   (provider-names
                     (sort (remove-duplicates
                            (mapcar (lambda (e)
                                      (clpm.registry:registry-name (car e)))
                                    (or entries '()))
                            :test #'string=)
                           #'string<)))
              (when (null provider-names)
                (log-error "System not found in configured registries: ~A" system-id)
                (return-from cmd-add 1))
              (when (and (null registry-name) (> (length provider-names) 1))
                (log-error "System ~A is provided by multiple registries; use --registry <name>:" system-id)
                (dolist (n provider-names)
                  (log-error "  ~A" n))
                (return-from cmd-add 1))
              (when registry-name
                (unless (member registry-name provider-names :test #'string=)
                  (log-error "Registry ~A does not provide ~A. Providers:" registry-name system-id)
                  (dolist (n provider-names)
                    (log-error "  ~A" n))
                  (return-from cmd-add 1))
                (setf dep-source (list :registry registry-name)))))

          (cond
            (path
             (setf constraint-form (list :path path)))
            (git-url
             (setf constraint-form (list :git :url git-url :ref git-ref)))
            (parsed-constraint
             (setf constraint-form parsed-constraint))
            (caret-p
             (let* ((regs (if registry-name
                              (let ((r (find registry-name registries
                                             :key #'clpm.registry:registry-name
                                             :test #'string=)))
                                (if r (list r) registries))
                              registries))
                    (v-max (highest-system-version regs system-id)))
               (unless v-max
                 (log-error "No versions found for ~A in configured registries" system-id)
                 (return-from cmd-add 1))
               (setf constraint-form (list :semver (format nil "^~A" v-max)))))
            (t
             (setf constraint-form nil))))

        (labels ((deps-slot ()
                   (ecase section
                     (:depends (clpm.project:project-depends project))
                     (:dev-depends (clpm.project:project-dev-depends project))
                     (:test-depends (clpm.project:project-test-depends project))))
                 (set-deps-slot (new)
                   (ecase section
                     (:depends (setf (clpm.project:project-depends project) new))
                     (:dev-depends (setf (clpm.project:project-dev-depends project) new))
                     (:test-depends (setf (clpm.project:project-test-depends project) new)))))
          (let* ((deps (deps-slot))
                 (existing (find system-id deps
                                 :key #'clpm.project:dependency-system
                                 :test #'string=)))
            (if existing
                (unless (equal (clpm.project:dependency-constraint existing) constraint-form)
                  (setf (clpm.project:dependency-constraint existing) constraint-form))
                (push (clpm.project:make-dependency
                       :system system-id
                       :constraint constraint-form
                       :source dep-source)
                      deps))
            (when (and existing dep-source)
              (setf (clpm.project:dependency-source existing) dep-source))
            (set-deps-slot (sorted-deps deps))
            (clpm.project:write-project-file project manifest-path)))

        (log-info "Added ~A to ~A" system-id
                  (ecase section
                    (:depends "depends")
                    (:dev-depends "dev-depends")
                    (:test-depends "test-depends")))

        (uiop:with-current-directory (project-root)
          (if install-p
              (cmd-install)
              (cmd-resolve)))))))

(defun cmd-remove (&rest args)
  "Remove a dependency from clpm.project and update clpm.lock."
  (multiple-value-bind (project-root manifest-path lock-path workspace-root _workspace-path)
      (find-effective-project-root)
    (declare (ignore lock-path _workspace-path))
    (unless manifest-path
      (when (null workspace-root)
        (log-no-project-found))
      (return-from cmd-remove 1))

    (let ((system-id nil)
          (dev-p nil)
          (test-p nil)
          (install-p nil))
      (dolist (arg args)
        (cond
          ((string= arg "--dev") (setf dev-p t))
          ((string= arg "--test") (setf test-p t))
          ((string= arg "--install") (setf install-p t))
          ((and (plusp (length arg)) (char= (char arg 0) #\-))
           (log-error "Unknown option: ~A" arg)
           (return-from cmd-remove 1))
          ((null system-id) (setf system-id arg))
          (t
           (log-error "Unexpected argument: ~A" arg)
           (return-from cmd-remove 1))))
      (unless system-id
        (log-error "Usage: clpm remove <system> [--dev|--test] [--install]")
        (return-from cmd-remove 1))

      (when (and dev-p test-p)
        (log-error "Only one of --dev or --test may be specified")
        (return-from cmd-remove 1))

      (let ((section (cond
                       (dev-p :dev-depends)
                       (test-p :test-depends)
                       (t :depends))))
        (let ((project (clpm.project:read-project-file manifest-path)))
          (labels ((deps-slot ()
                     (ecase section
                       (:depends (clpm.project:project-depends project))
                       (:dev-depends (clpm.project:project-dev-depends project))
                       (:test-depends (clpm.project:project-test-depends project))))
                   (set-deps-slot (new)
                     (ecase section
                       (:depends (setf (clpm.project:project-depends project) new))
                       (:dev-depends (setf (clpm.project:project-dev-depends project) new))
                       (:test-depends (setf (clpm.project:project-test-depends project) new)))))
            (let* ((deps (deps-slot))
                   (new-deps (remove system-id deps
                                     :key #'clpm.project:dependency-system
                                     :test #'string=)))
              (when (eql (length new-deps) (length deps))
                (log-error "Dependency not found: ~A" system-id)
                (return-from cmd-remove 1))
              (set-deps-slot (sorted-deps new-deps))
              (clpm.project:write-project-file project manifest-path))))

        (log-info "Removed ~A from ~A" system-id
                  (ecase section
                    (:depends "depends")
                    (:dev-depends "dev-depends")
                    (:test-depends "test-depends")))

        (uiop:with-current-directory (project-root)
          (if install-p
              (cmd-install)
              (cmd-resolve)))))))

;;; search command

(defun cmd-search (&rest args)
  "Search configured registries for systems matching a query string."
  (labels ((usage-error (fmt &rest fmt-args)
             (apply #'log-error fmt fmt-args)
             (log-error "Usage: clpm search <query> [--limit N] [--json]")
             (return-from cmd-search 1))
           (parse-release-ref (release-ref)
             (let ((at-pos (and (stringp release-ref)
                                (position #\@ release-ref))))
               (if at-pos
                   (values (subseq release-ref 0 at-pos)
                           (subseq release-ref (1+ at-pos)))
                   (values release-ref nil)))))
    (let ((query nil)
          (limit nil)
          (jsonp nil))
      ;; Parse args
      (let ((i 0))
        (loop while (< i (length args)) do
          (let ((arg (nth i args)))
            (cond
              ((string= arg "--limit")
               (incf i)
               (when (>= i (length args))
                 (usage-error "Missing value for --limit"))
               (let* ((raw (nth i args))
                      (n (ignore-errors (parse-integer raw :junk-allowed nil))))
                 (unless (and (integerp n) (plusp n))
                   (usage-error "Invalid value for --limit: ~A" raw))
                 (setf limit n)))
              ((string= arg "--json")
               (setf jsonp t))
              ((and (stringp arg) (plusp (length arg)) (char= (char arg 0) #\-))
               (usage-error "Unknown option: ~A" arg))
              ((null query)
               (setf query arg))
              (t
               (usage-error "Unexpected argument: ~A" arg))))
          (incf i)))

      (unless (and (stringp query) (plusp (length query)))
        (usage-error "Missing search query"))

      (let ((registries (load-merged-registries)))
        (when (null registries)
          (log-error "No registries configured (run: clpm registry add ...)")
          (return-from cmd-search 1))

        (let* ((q (string-downcase query))
               (results '()))
          (dolist (reg registries)
            (let* ((reg-name (clpm.registry:registry-name reg))
                   (snap (clpm.registry:registry-snapshot reg))
                   (best (make-hash-table :test 'equal)))
              (when snap
                (dolist (entry (clpm.registry:snapshot-provides snap))
                  (let ((system-id (car entry))
                        (release-ref (cdr entry)))
                    (when (and (stringp system-id)
                               (search q system-id :test #'char-equal))
                      (multiple-value-bind (_pkg ver)
                          (parse-release-ref release-ref)
                        (declare (ignore _pkg))
                        (let ((existing (gethash system-id best)))
                          (cond
                            ((null existing)
                             (setf (gethash system-id best) (cons release-ref ver)))
                            (t
                             (let ((existing-ref (car existing))
                                   (existing-ver (cdr existing)))
                               (when (or (and ver existing-ver
                                              (clpm.solver.version:version> ver existing-ver))
                                         (and ver existing-ver
                                              (clpm.solver.version:version= ver existing-ver)
                                              (string< release-ref existing-ref))
                                         (and ver (null existing-ver))
                                         (and (null ver) (null existing-ver)
                                              (string< release-ref existing-ref)))
                                 (setf (gethash system-id best) (cons release-ref ver))))))))))))
              (maphash (lambda (system-id info)
                         (push (list system-id reg-name (car info)) results))
                       best)))

          (setf results
                (sort results
                      (lambda (a b)
                        (destructuring-bind (asys areg arel) a
                          (destructuring-bind (bsys breg brel) b
                            (cond
                              ((string< asys bsys) t)
                              ((string> asys bsys) nil)
                              ((string< areg breg) t)
                              ((string> areg breg) nil)
                              (t (string< arel brel))))))))

          (when limit
            (setf results (subseq results 0 (min limit (length results)))))

          (if jsonp
              (progn
                (clpm.io.json:write-json
                 (list :array
                       (mapcar (lambda (r)
                                 (destructuring-bind (sys reg-name rel) r
                                   (list :object
                                         (list (cons "system" sys)
                                               (cons "registry" reg-name)
                                               (cons "release" rel)))))
                               results))
                 *standard-output*)
                (terpri)
                0)
              (progn
                (dolist (r results)
                  (destructuring-bind (sys reg-name rel) r
                    (format t "~A~C~A~C~A~%" sys #\Tab reg-name #\Tab rel)))
                0)))))))

;;; info command

(defun cmd-info (&rest args)
  "Show information about a system across configured registries."
  (labels ((usage-error (fmt &rest fmt-args)
             (apply #'log-error fmt fmt-args)
             (log-error "Usage: clpm info <system> [--json] [--all]")
             (return-from cmd-info 1))
           (release-ref (pkg ver)
             (format nil "~A@~A" pkg ver))
           (source->fields (source)
             (when (and (consp source) (keywordp (car source)))
               (list (car source)
                     (getf (cdr source) :url)
                     (or (getf (cdr source) :sha256)
                         (getf (cdr source) :sha1))
                     (getf (cdr source) :commit)))))
    (let ((system-id nil)
          (jsonp nil)
          (allp nil))
      (dolist (arg args)
        (cond
          ((string= arg "--json") (setf jsonp t))
          ((string= arg "--all") (setf allp t))
          ((and (stringp arg) (plusp (length arg)) (char= (char arg 0) #\-))
           (usage-error "Unknown option: ~A" arg))
          ((null system-id) (setf system-id arg))
          (t (usage-error "Unexpected argument: ~A" arg))))
      (unless (and (stringp system-id) (plusp (length system-id)))
        (usage-error "Missing system id"))

      (let ((registries (load-merged-registries)))
        (when (null registries)
          (log-error "No registries configured (run: clpm registry add ...)")
          (return-from cmd-info 1))

        (let ((candidates '()))
          (dolist (reg registries)
            (let ((reg-name (clpm.registry:registry-name reg)))
              (dolist (pair (clpm.registry:find-system-candidates reg system-id))
                (push (list :registry reg
                            :registry-name reg-name
                            :package (car pair)
                            :version (cdr pair)
                            :release (release-ref (car pair) (cdr pair)))
                      candidates))))
          (when (null candidates)
            (log-error "System not found: ~A" system-id)
            (return-from cmd-info 1))

          (setf candidates
                (sort candidates
                      (lambda (a b)
                        (let ((aver (getf a :version))
                              (bver (getf b :version)))
                          (cond
                            ((and aver bver (clpm.solver.version:version> aver bver)) t)
                            ((and aver bver (clpm.solver.version:version< aver bver)) nil)
                            ((and aver (null bver)) t)
                            ((and (null aver) bver) nil)
                            ((string< (getf a :registry-name) (getf b :registry-name)) t)
                            ((string> (getf a :registry-name) (getf b :registry-name)) nil)
                            ((string< (getf a :package) (getf b :package)) t)
                            ((string> (getf a :package) (getf b :package)) nil)
                            (t (string< (getf a :release) (getf b :release))))))))

          (let* ((selected (first candidates))
                 (sel-reg (getf selected :registry))
                 (sel-reg-name (getf selected :registry-name))
                 (sel-pkg (getf selected :package))
                 (sel-ver (getf selected :version))
                 (sel-rel (getf selected :release))
                 (sel-meta (clpm.registry:get-release-metadata sel-reg sel-pkg sel-ver)))
            (if jsonp
                (let* ((selected-entries
                         (list (cons "registry" sel-reg-name)
                               (cons "package" sel-pkg)
                               (cons "version" sel-ver)
                               (cons "release" sel-rel)))
                       (candidates-json
                         (mapcar (lambda (c)
                                   (let ((entries (list (cons "registry" (getf c :registry-name))
                                                        (cons "package" (getf c :package))
                                                        (cons "version" (getf c :version))
                                                        (cons "release" (getf c :release)))))
                                     (when allp
                                       (let* ((reg (getf c :registry))
                                              (pkg (getf c :package))
                                              (ver (getf c :version))
                                              (meta (clpm.registry:get-release-metadata reg pkg ver)))
                                         (when (and meta (clpm.registry:release-metadata-license meta))
                                           (push (cons "license" (clpm.registry:release-metadata-license meta))
                                                 entries))))
                                     (list :object (nreverse entries))))
                                 candidates)))
                  (when sel-meta
                    (let ((fields (source->fields (clpm.registry:release-metadata-source sel-meta))))
                      (when fields
                        (destructuring-bind (kind url hash commit) fields
                          (push (cons "source"
                                      (list :object
                                            (list (cons "kind" (string-downcase (symbol-name kind)))
                                                  (cons "url" (or url ""))
                                                  (cons "hash" (or hash ""))
                                                  (cons "commit" (or commit "")))))
                                selected-entries))))
                    (when (clpm.registry:release-metadata-license sel-meta)
                      (push (cons "license" (clpm.registry:release-metadata-license sel-meta))
                            selected-entries)))
                  (clpm.io.json:write-json
                   (list :object
                         (list (cons "system" system-id)
                               (cons "selected" (list :object (nreverse selected-entries)))
                               (cons "candidates" (list :array candidates-json))))
                   *standard-output*)
                  (terpri)
                  0)
                (progn
                  (format t "System: ~A~%" system-id)
                  (format t "Selected:~%  ~A~C~A~%" sel-reg-name #\Tab sel-rel)
                  (when sel-meta
                    (let ((fields (source->fields (clpm.registry:release-metadata-source sel-meta))))
                      (when fields
                        (destructuring-bind (kind url hash commit) fields
                          (format t "Source:~%  ~A~C~A"
                                  (string-downcase (symbol-name kind)) #\Tab (or url ""))
                          (cond
                            ((and hash (plusp (length hash)))
                             (format t "~Chash:~A~%" #\Tab hash))
                            ((and commit (plusp (length commit)))
                             (format t "~Ccommit:~A~%" #\Tab commit))
                            (t (terpri))))))
                    (when (clpm.registry:release-metadata-license sel-meta)
                      (format t "Metadata:~%  license~C~A~%"
                              #\Tab (clpm.registry:release-metadata-license sel-meta))))
                  (format t "Candidates:~%")
                  (dolist (c candidates)
                    (format t "  ~A~C~A~%"
                            (getf c :registry-name) #\Tab (getf c :release)))
                  0))))))))

;;; dependency graph introspection (tree/why)

(defun %project-root-system-ids (project)
  "Return all root dependency system IDs for PROJECT in deterministic order.

Roots include :depends, :dev-depends, and :test-depends."
  (let ((roots '()))
    (dolist (dep (append (clpm.project:project-depends project)
                         (clpm.project:project-dev-depends project)
                         (clpm.project:project-test-depends project)))
      (let ((sys (and dep (clpm.project:dependency-system dep))))
        (when (and (stringp sys) (plusp (length sys)))
          (pushnew sys roots :test #'string=))))
    (sort roots #'string<)))

(defun %lockfile-graph (lockfile)
  "Build a hash table mapping system-id -> sorted dependency system-id list."
  (let ((graph (make-hash-table :test 'equal)))
    (dolist (locked (clpm.project:lockfile-resolved lockfile))
      (let* ((id (clpm.project:locked-system-id locked))
             (deps (copy-list (or (clpm.project:locked-system-deps locked) '()))))
        (when (and (stringp id) (plusp (length id)))
          (setf (gethash id graph) (sort deps #'string<)))))
    graph))

(defun %print-tree-line (depth system-id &optional (suffix ""))
  (let ((indent (make-string (* 2 depth) :initial-element #\Space)))
    (format t "~A~A~A~%" indent system-id suffix)))

(defun %print-dependency-tree (roots graph &key depth-limit)
  "Print a deterministic dependency tree rooted at ROOTS.

GRAPH is a hash table mapping system-id -> sorted list of dependency system-ids."
  (let ((expanded (make-hash-table :test 'equal)))
    (labels ((walk (system-id depth path)
               (when (member system-id path :test #'string=)
                 (%print-tree-line depth system-id " (cycle)")
                 (return-from walk nil))
               (multiple-value-bind (deps presentp) (gethash system-id graph)
                 (unless presentp
                   (%print-tree-line depth system-id " (missing from lockfile)")
                   (return-from walk nil))
                 (when (gethash system-id expanded)
                   (%print-tree-line depth system-id " (*)")
                   (return-from walk nil))
                 (%print-tree-line depth system-id)
                 (when (and (integerp depth-limit)
                            (>= depth depth-limit))
                   (return-from walk nil))
                 (setf (gethash system-id expanded) t)
                 (dolist (dep deps)
                   (walk dep (1+ depth) (cons system-id path))))))
      (dolist (root roots)
        (walk root 0 '())))))

(defun cmd-tree (&rest args)
  "Print a dependency tree from the current project's lockfile."
  (labels ((usage-error (fmt &rest fmt-args)
             (apply #'log-error fmt fmt-args)
             (log-error "Usage: clpm tree [--package <member>] [--depth N]")
             (return-from cmd-tree 1)))
    (let ((package nil)
          (depth-limit nil))
      ;; Parse args
      (let ((i 0))
        (loop while (< i (length args)) do
          (let ((arg (nth i args)))
            (cond
              ((string= arg "--package")
               (incf i)
               (when (>= i (length args))
                 (usage-error "Missing value for --package"))
               (setf package (nth i args)))
              ((string= arg "--depth")
               (incf i)
               (when (>= i (length args))
                 (usage-error "Missing value for --depth"))
               (let* ((raw (nth i args))
                      (n (ignore-errors (parse-integer raw :junk-allowed nil))))
                 (unless (and (integerp n) (<= 0 n))
                   (usage-error "Invalid value for --depth: ~A" raw))
                 (setf depth-limit n)))
              ((and (stringp arg) (plusp (length arg)) (char= (char arg 0) #\-))
               (usage-error "Unknown option: ~A" arg))
              (t
               (usage-error "Unexpected argument: ~A" arg))))
          (incf i)))

      (let ((*target-package* (or package *target-package*)))
        (multiple-value-bind (_project-root manifest-path lock-path workspace-root _workspace-path)
            (find-effective-project-root)
          (declare (ignore _project-root _workspace-path))
          (unless manifest-path
            (when (null workspace-root)
              (log-no-project-found))
            (return-from cmd-tree 1))
          (unless lock-path
            (log-error "No clpm.lock found - run 'clpm resolve' first")
            (return-from cmd-tree 1))
          (let* ((project (clpm.project:read-project-file manifest-path))
                 (lockfile (clpm.project:read-lock-file lock-path))
                 (roots (%project-root-system-ids project)))
            (when (null roots)
              (log-info "No dependencies")
              (return-from cmd-tree 0))
            (%print-dependency-tree roots (%lockfile-graph lockfile)
                                    :depth-limit depth-limit)
            0))))))

(defun %distinct-shortest-paths (roots graph target &key (limit 10))
  "Return up to LIMIT distinct shortest paths (as lists of system-ids) from ROOTS to TARGET."
  (let ((dist (make-hash-table :test 'equal))
        (parents (make-hash-table :test 'equal)))
    (flet ((add-parent (node parent)
             (let ((ps (gethash node parents)))
               (unless (member parent ps :test #'string=)
                 (setf (gethash node parents) (cons parent ps)))))
           (parents-of (node)
             (sort (copy-list (or (gethash node parents) '())) #'string<)))
      ;; BFS: build dist + parent sets for shortest paths from ROOTS.
      (let ((queue (make-array 0 :adjustable t :fill-pointer 0))
            (head 0))
        (flet ((enqueue (x) (vector-push-extend x queue)))
          (dolist (root roots)
            (unless (gethash root dist)
              (setf (gethash root dist) 0)
              (enqueue root)))
          (loop while (< head (length queue)) do
            (let* ((u (aref queue head))
                   (u-dist (gethash u dist)))
              (incf head)
              (multiple-value-bind (deps _presentp) (gethash u graph)
                (declare (ignore _presentp))
                (dolist (v deps)
                  (let* ((alt (1+ u-dist))
                         (v-dist (gethash v dist)))
                    (cond
                      ((null v-dist)
                       (setf (gethash v dist) alt)
                       (add-parent v u)
                       (enqueue v))
                      ((= v-dist alt)
                       (add-parent v u))
                      (t nil)))))))))

      (unless (gethash target dist)
        (return-from %distinct-shortest-paths nil))

      ;; Enumerate paths (shortest only), in stable order, up to LIMIT.
      (labels ((paths-to (node remaining)
                 (when (<= remaining 0)
                   (return-from paths-to (values '() 0)))
                 (let ((d (gethash node dist)))
                   (when (null d)
                     (return-from paths-to (values '() remaining)))
                   (when (zerop d)
                     (return-from paths-to
                       (values (list (list node)) (1- remaining))))
                   (let ((acc '())
                         (left remaining))
                     (dolist (p (parents-of node))
                       (when (<= left 0)
                         (return))
                       (multiple-value-bind (ppaths new-left)
                           (paths-to p left)
                         (setf left new-left)
                         (dolist (pp ppaths)
                           (push (append pp (list node)) acc))))
                     (values (nreverse acc) left)))))
        (multiple-value-bind (paths _left)
            (paths-to target limit)
          (declare (ignore _left))
          paths)))))

(defun cmd-why (&rest args)
  "Explain why a system appears in the resolved dependency graph."
  (labels ((usage-error (fmt &rest fmt-args)
             (apply #'log-error fmt fmt-args)
             (log-error "Usage: clpm why <system-id> [--package <member>]")
             (return-from cmd-why 1)))
    (let ((package nil)
          (target nil))
      ;; Parse args
      (let ((i 0))
        (loop while (< i (length args)) do
          (let ((arg (nth i args)))
            (cond
              ((string= arg "--package")
               (incf i)
               (when (>= i (length args))
                 (usage-error "Missing value for --package"))
               (setf package (nth i args)))
              ((and (stringp arg) (plusp (length arg)) (char= (char arg 0) #\-))
               (usage-error "Unknown option: ~A" arg))
              ((null target)
               (setf target arg))
              (t
               (usage-error "Unexpected argument: ~A" arg))))
          (incf i)))

      (unless (and (stringp target) (plusp (length target)))
        (usage-error "Missing <system-id>"))

      (let ((*target-package* (or package *target-package*)))
        (multiple-value-bind (_project-root manifest-path lock-path workspace-root _workspace-path)
            (find-effective-project-root)
          (declare (ignore _project-root _workspace-path))
          (unless manifest-path
            (when (null workspace-root)
              (log-no-project-found))
            (return-from cmd-why 1))
          (unless lock-path
            (log-error "No clpm.lock found - run 'clpm resolve' first")
            (return-from cmd-why 1))
          (let* ((project (clpm.project:read-project-file manifest-path))
                 (lockfile (clpm.project:read-lock-file lock-path))
                 (roots (%project-root-system-ids project)))
            (when (null roots)
              (log-error "No dependencies to explain")
              (return-from cmd-why 1))
            (let* ((graph (%lockfile-graph lockfile))
                   (paths (%distinct-shortest-paths roots graph target :limit 10)))
              (unless paths
                (log-error "System not reachable from project dependencies: ~A" target)
                (return-from cmd-why 1))
              (format t "Why: ~A~%" target)
              (dolist (path paths)
                (format t "  ~{~A~^ -> ~}~%" path))
              0)))))))

;;; resolve command

(defun cmd-resolve ()
  "Resolve dependencies and create/update lockfile."
  (multiple-value-bind (project-root manifest-path lock-path workspace-root _workspace-path)
      (find-effective-project-root)
    (declare (ignore _workspace-path))
    (unless manifest-path
      (when (null workspace-root)
        (log-no-project-found))
      (return-from cmd-resolve 1))
    (let* ((project (clpm.project:read-project-file manifest-path))
           (project-hash (%canonical-sexp-sha256 (clpm.project:serialize-project project)))
           (registries-hash
             (multiple-value-bind (refs _build-options)
                 (clpm.config:merge-project-config project)
               (declare (ignore _build-options))
               (%canonical-sexp-sha256 (%registry-input-sexp refs))))
           (lockfile (when lock-path
                       (ignore-errors (clpm.project:read-lock-file lock-path)))))

      (when (and lockfile
                 (stringp (clpm.project:lockfile-project-sha256 lockfile))
                 (stringp (clpm.project:lockfile-registries-sha256 lockfile))
                 (string= (clpm.project:lockfile-project-sha256 lockfile) project-hash)
                 (string= (clpm.project:lockfile-registries-sha256 lockfile) registries-hash))
        (log-info "clpm.lock is up to date (inputs unchanged); skipping resolve")
        (return-from cmd-resolve 0))

      (log-info "Resolving dependencies...")
      (let ((registries (load-project-registries project)))
        (handler-case
            (let* ((resolution (clpm.solver:solve project registries
                                                 :lockfile lockfile))
                   (new-lockfile (clpm.solver:resolution-to-lockfile
                                  resolution project registries)))
              (setf (clpm.project:lockfile-project-sha256 new-lockfile) project-hash
                    (clpm.project:lockfile-registries-sha256 new-lockfile) registries-hash)
              (let ((lock-out (merge-pathnames "clpm.lock" project-root)))
                (clpm.project:write-lock-file new-lockfile lock-out))
              (log-info "Wrote clpm.lock")
              (log-info "Resolved ~D systems"
                        (length (clpm.project:lockfile-resolved new-lockfile))))
          (clpm.errors:clpm-resolve-error (c)
            (log-error "Failed to resolve dependencies: ~A"
                       (clpm.errors:clpm-error-message c))
            (let ((chain (clpm.errors:clpm-resolve-error-conflict-chain c)))
              (when chain
                (format *error-output* "~&Conflict chain:~%")
                (dolist (line chain)
                  (format *error-output* "  ~A~%" line))))
            (return-from cmd-resolve 2)))))
    0))

;;; fetch command

(defun cmd-fetch ()
  "Fetch all dependencies from lockfile."
  (multiple-value-bind (project-root manifest-path lock-path workspace-root _workspace-path)
      (find-effective-project-root)
    (declare (ignore manifest-path _workspace-path))
    (unless lock-path
      (when (null project-root)
        (when (null workspace-root)
          (log-no-project-found)
          (return-from cmd-fetch 1))
        (return-from cmd-fetch 1))
      (log-error "No clpm.lock found - run 'clpm resolve' first")
      (return-from cmd-fetch 1))
    (log-info "Fetching dependencies...")
    (let ((lockfile (clpm.project:read-lock-file lock-path)))
      (let ((missing-tree-before
              (count-if (lambda (locked)
                          (null (clpm.project:locked-release-tree-sha256
                                 (clpm.project:locked-system-release locked))))
                        (clpm.project:lockfile-resolved lockfile))))
        (when *offline*
          (log-error "Offline mode - checking store only")
          ;; In offline mode, just verify all are present
          (dolist (locked (clpm.project:lockfile-resolved lockfile))
            (let* ((release (clpm.project:locked-system-release locked))
                   (tree-sha256 (clpm.project:locked-release-tree-sha256 release)))
              (unless (and tree-sha256
                           (clpm.store:source-exists-p tree-sha256))
                (log-error "Missing in store: ~A"
                           (clpm.project:locked-system-id locked))
                (return-from cmd-fetch 1))))
          (return-from cmd-fetch 0))
        ;; Fetch all
        (handler-case
            (let ((results (clpm.fetch:fetch-lockfile-deps
                            lockfile
                            :lockfile-path lock-path
                            :jobs *jobs*)))
              (let ((missing-tree-after
                      (count-if (lambda (locked)
                                  (null (clpm.project:locked-release-tree-sha256
                                         (clpm.project:locked-system-release locked))))
                                (clpm.project:lockfile-resolved lockfile))))
                (when (and (plusp missing-tree-before)
                           (< missing-tree-after missing-tree-before))
                  (log-info "Updated clpm.lock with source tree hashes")))
              (log-info "Fetched ~D dependencies" (length results)))
          (error (c)
            (log-error "~A" c)
            (return-from cmd-fetch 1))))
      0)))

;;; build command

(defun cmd-build (&key compile-options)
  "Build all dependencies."
  (multiple-value-bind (project-root manifest-path lock-path workspace-root _workspace-path)
      (find-effective-project-root)
    (declare (ignore project-root _workspace-path))
    (unless manifest-path
      (when (null workspace-root)
        (log-no-project-found))
      (return-from cmd-build 1))
    (unless lock-path
      (log-error "No clpm.lock found - run 'clpm resolve' first")
      (return-from cmd-build 1))
    (log-info "Building dependencies...")
    (let* ((project (clpm.project:read-project-file manifest-path))
           (kind (effective-lisp-kind project))
           (lisp-version (case kind
                           (:sbcl (clpm.platform:sbcl-version))
                           (t (clpm.lisp:lisp-version kind))))
           (effective-build (nth-value 1 (clpm.config:merge-project-config project)))
           (compile-options (or compile-options effective-build))
           (lockfile (clpm.project:read-lock-file lock-path))
           (registries (load-project-registries project)))
      ;; First check native deps
      (handler-case
          (clpm.build:check-native-deps lockfile)
        (clpm.errors:clpm-missing-native-dep-error (c)
          (log-error "~A" c)
          (return-from cmd-build 1)))
      ;; Get source paths
      (let ((source-paths (collect-source-paths lockfile)))
        ;; Re-resolve for graph info
        (let ((resolution (clpm.solver:solve project registries
                                             :lockfile lockfile)))
          ;; Build
          (handler-case
              (let ((build-results
                      (clpm.build:build-all resolution lockfile source-paths
                                            :jobs *jobs*
                                            :compile-options compile-options
                                            :lisp-kind kind
                                            :lisp-version lisp-version)))
                (log-info "Built ~D systems" (length build-results)))
            (clpm.errors:clpm-build-error (c)
              (log-error "~A" c)
              (return-from cmd-build 1))))))
    0))

;;; install command (resolve + fetch + build)

(defun cmd-install ()
  "Install all dependencies (resolve, fetch, build)."
  (multiple-value-bind (project-root manifest-path lock-path workspace-root _workspace-path)
      (find-effective-project-root)
    (declare (ignore lock-path _workspace-path))
    (unless manifest-path
      (when (null workspace-root)
        (log-no-project-found))
      (return-from cmd-install 1))
    (let* ((project (clpm.project:read-project-file manifest-path))
           (kind (effective-lisp-kind project))
           (lisp-version (case kind
                           (:sbcl (clpm.platform:sbcl-version))
                           (t (clpm.lisp:lisp-version kind))))
           (compile-options (nth-value 1 (clpm.config:merge-project-config project))))
      ;; Resolve to ensure clpm.lock matches current clpm.project.
      (let ((result (cmd-resolve)))
        (unless (zerop result)
          (return-from cmd-install result)))
      (setf lock-path (merge-pathnames "clpm.lock" project-root))
      ;; Fetch
      (let ((result (cmd-fetch)))
        (unless (zerop result)
          (return-from cmd-install result)))
      ;; Build
      (let ((result (cmd-build :compile-options compile-options)))
        (unless (zerop result)
          (return-from cmd-install result)))
      ;; Activate
      (log-info "Activating project...")
      (let ((lockfile (clpm.project:read-lock-file lock-path)))
        (clpm.build:activate-project project-root lockfile
                                     :lockfile-path lock-path
                                     :compile-options compile-options
                                     :lisp-kind kind
                                     :lisp-version lisp-version))
      (log-info "Project installed successfully")
      (log-info "Run 'clpm repl' to start a REPL with the project loaded"))
    0))

;;; update command

(defun cmd-update (&rest systems)
  "Update dependencies (re-resolve with latest versions)."
  (declare (ignore systems))  ; TODO: selective update
  (multiple-value-bind (project-root manifest-path _lock-path workspace-root _workspace-path)
      (find-effective-project-root)
    (declare (ignore _lock-path _workspace-path))
    (unless manifest-path
      (when (null workspace-root)
        (log-no-project-found))
      (return-from cmd-update 1))
    (log-info "Updating dependencies...")
    ;; Force fresh resolve by ignoring lockfile
    (let* ((project (clpm.project:read-project-file manifest-path))
           (registries (load-project-registries project)))
      ;; Update registries first
      (dolist (reg registries)
        (log-verbose "Updating registry: ~A" (clpm.registry:registry-name reg))
        (handler-case
            (clpm.registry:update-registry reg)
          (error (c)
            (log-error "Failed to update registry ~A: ~A"
                       (clpm.registry:registry-name reg) c))))
      ;; Resolve fresh (no lockfile)
      (handler-case
          (let ((resolution (clpm.solver:solve project registries)))
            (let ((new-lockfile (clpm.solver:resolution-to-lockfile
                                 resolution project registries)))
              (let ((lock-out (merge-pathnames "clpm.lock"
                                               project-root)))
                (clpm.project:write-lock-file new-lockfile lock-out)
                (log-info "Updated clpm.lock"))))
        (clpm.errors:clpm-resolve-error (c)
          (log-error "~A" c)
          (return-from cmd-update 2))))
    0))

;;; repl command

(defun cmd-repl (&key load-system)
  "Start a REPL with the project activated."
  (multiple-value-bind (project-root manifest-path lock-path workspace-root _workspace-path)
      (find-effective-project-root)
    (declare (ignore _workspace-path))
    (unless manifest-path
      (when (null workspace-root)
        (log-no-project-found))
      (return-from cmd-repl 1))
    (unless lock-path
      (log-error "No clpm.lock found - run 'clpm install' first")
      (return-from cmd-repl 1))
    (let* ((project (when manifest-path
                      (clpm.project:read-project-file manifest-path)))
           (kind (effective-lisp-kind project))
           (config-path (merge-pathnames ".clpm/asdf-config.lisp" project-root)))
      (unless (uiop:file-exists-p config-path)
        (log-error "Project not activated - run 'clpm install' first")
        (return-from cmd-repl 1))
      (let ((argv (clpm.lisp:lisp-run-argv kind
                                          :load-files (list (namestring config-path))
                                          :eval-forms (when load-system
                                                        (list (format nil "(asdf:load-system ~S)"
                                                                      load-system)))
                                          :noinform nil
                                          :noninteractive nil
                                          :disable-debugger nil)))
        (log-info "Starting ~A..." (string-downcase (symbol-name kind)))
      (multiple-value-bind (output error-output exit-code)
          (clpm.platform:run-program argv
                                     :input :interactive
                                     :output :interactive
                                     :error-output :interactive)
        (declare (ignore output error-output))
        exit-code)))))

;;; run/exec commands

(defun ensure-project-activated (project-root)
  "Ensure PROJECT-ROOT is installed and activated; returns (values config-path exit-code)."
  (labels ((lock-path ()
             (merge-pathnames "clpm.lock" project-root))
           (config-path ()
             (merge-pathnames ".clpm/asdf-config.lisp" project-root))
           (env-path ()
             (merge-pathnames ".clpm/env.sexp" project-root))
           (lockfile-sha256-hex (path)
             (clpm.crypto.sha256:bytes-to-hex
              (clpm.crypto.sha256:sha256-file path)))
           (read-env-lockfile-sha256 (path)
             (when (uiop:file-exists-p path)
               (handler-case
                   (let ((form (clpm.io.sexp:read-safe-sexp-from-file path)))
                     (when (and (consp form) (eq (car form) :env))
                       (let ((plist (cdr form)))
                         (getf plist :lockfile-sha256))))
                 (error ()
                   nil))))
           (installed-and-fresh-p ()
             (clpm.platform:ensure-directories)
             (let ((lp (lock-path))
                   (cp (config-path))
                   (ep (env-path)))
               (when (or (not (uiop:file-exists-p lp))
                         (not (uiop:file-exists-p cp))
                         (not (uiop:file-exists-p ep)))
                 (return-from installed-and-fresh-p nil))
               (let* ((env-lock (read-env-lockfile-sha256 ep))
                      (cur-lock (lockfile-sha256-hex lp)))
                 (unless (and (stringp env-lock) (stringp cur-lock)
                              (string= env-lock cur-lock))
                   (return-from installed-and-fresh-p nil))
                 ;; Ensure lockfile is fully fetched (tree hashes present) and
                 ;; store entries exist. This avoids confusing ASDF missing-system
                 ;; errors during `clpm run` when the activation is stale or incomplete.
                 (let ((lockfile (ignore-errors (clpm.project:read-lock-file lp))))
                   (unless lockfile
                     (return-from installed-and-fresh-p nil))
                   (dolist (locked (clpm.project:lockfile-resolved lockfile))
                     (let* ((release (clpm.project:locked-system-release locked))
                            (tree (clpm.project:locked-release-tree-sha256 release)))
                       (when (or (null tree)
                                 (not (clpm.store:source-exists-p tree)))
                         (return-from installed-and-fresh-p nil))))
                   t)))))
    (let ((cp (config-path)))
      (unless (installed-and-fresh-p)
        (log-info "Project not installed/activated (or out of date); running 'clpm install'...")
        (let ((rc (uiop:with-current-directory (project-root)
                    (cmd-install))))
          (unless (zerop rc)
            (return-from ensure-project-activated (values nil rc)))))
      (if (uiop:file-exists-p cp)
          (values cp 0)
          (values nil 1)))))

(defun parse-function-spec (spec)
  "Parse \"<package>::<fn>\" and return (values package-name function-name).
Returns NIL values on parse failure."
  (let ((pos (search "::" spec)))
    (when pos
      (let ((pkg (subseq spec 0 pos))
            (fn (subseq spec (+ pos 2))))
        (when (and (plusp (length pkg)) (plusp (length fn)))
          (values pkg fn))))))

(defun project-dependency-system-ids (project sections)
  "Return a sorted list of system IDs from PROJECT dependency SECTIONS.

SECTIONS is a list of keywords: :DEPENDS, :DEV-DEPENDS, :TEST-DEPENDS."
  (let ((systems nil))
    (dolist (section sections)
      (dolist (dep (ecase section
                     (:depends (clpm.project:project-depends project))
                     (:dev-depends (clpm.project:project-dev-depends project))
                     (:test-depends (clpm.project:project-test-depends project))))
        (let ((id (clpm.project:dependency-system dep)))
          (when (and id (stringp id))
            (push id systems)))))
    (sort (remove-duplicates systems :test #'string=) #'string<)))

(defun effective-lisp-kind (project)
  "Return the effective Lisp kind for PROJECT.

Precedence: CLI `--lisp` (*lisp*) > project :lisp > default :sbcl."
  (cond
    (*lisp*
     (clpm.lisp:parse-lisp-kind *lisp*))
    ((and project (clpm.project:project-lisp project))
     (clpm.lisp:parse-lisp-kind (clpm.project:project-lisp project)))
    (t :sbcl)))

(defun lisp-load-systems-eval-forms (systems)
  (mapcar (lambda (sys)
            (format nil "(asdf:load-system ~S)" sys))
          systems))

(defun run-lisp-with-config (kind project-root config-path eval-forms
                              &key (noinform t) (noninteractive t) (timeout 600000))
  "Run KIND with CONFIG-PATH loaded and EVAL-FORMS evaluated in order."
  (let* ((argv (clpm.lisp:lisp-run-argv kind
                                       :load-files (list (namestring config-path))
                                       :eval-forms eval-forms
                                       :noinform noinform
                                       :noninteractive noninteractive
                                       :disable-debugger t)))
    (multiple-value-bind (output error-output exit-code)
        (clpm.platform:run-program argv
                                   :directory project-root
                                   :output :interactive
                                   :error-output :interactive
                                   :timeout timeout)
      (declare (ignore output error-output))
      exit-code)))

(defun run-lisp-entrypoint (kind project-root config-path deps system fn-spec run-args)
  "Load DEPS and SYSTEM then call FN-SPEC with RUN-ARGS under KIND."
  (multiple-value-bind (pkg fn)
      (parse-function-spec fn-spec)
    (unless (and pkg fn)
      (log-error "Invalid :function: expected <package>::<fn>, got ~S" fn-spec)
      (return-from run-lisp-entrypoint 1))
    (let* ((pkg-name (string-upcase pkg))
           (fn-name (string-upcase fn))
           (args-var (intern "CLPM-RUN-ARGS" "CL-USER"))
           (result-var (intern "CLPM-RUN-RESULT" "CL-USER"))
           (pkg-var (intern "CLPM-RUN-PKG" "CL-USER"))
           (sym-var (intern "CLPM-RUN-SYM" "CL-USER"))
           (call-form
             `(let ((,args-var ',run-args))
                (let* ((,pkg-var (find-package ,pkg-name))
                       (,sym-var (and ,pkg-var (find-symbol ,fn-name ,pkg-var))))
                  (unless (and ,sym-var (fboundp ,sym-var))
                    (format *error-output* "~&Entry function not found: ~A::~A~%" ,pkg-name ,fn-name)
                    (uiop:quit 1))
                  (let ((,result-var (funcall ,sym-var ,args-var)))
                    (uiop:quit (if (integerp ,result-var) ,result-var 0))))))
           (call-form-str
             (with-standard-io-syntax
               (let ((*package* (find-package "CL-USER")))
                 (prin1-to-string call-form))))
           (eval-forms (append (lisp-load-systems-eval-forms deps)
                               (list (format nil "(asdf:load-system ~S)" system)
                                     call-form-str))))
      (run-lisp-with-config kind project-root config-path eval-forms))))

(defun sbcl-load-systems-argv (systems)
  "Return an argv fragment that loads each system in SYSTEMS via ASDF."
  (mapcan (lambda (sys)
            (list "--eval" (format nil "(asdf:load-system ~S)" sys)))
          systems))

(defun run-sbcl-entrypoint (project-root config-path deps system fn-spec run-args)
  "Run SYSTEM and call FN-SPEC under SBCL in the activated project environment.

Returns an integer exit code."
  (multiple-value-bind (pkg fn)
      (parse-function-spec fn-spec)
    (unless (and pkg fn)
      (log-error "Invalid :function: expected <package>::<fn>, got ~S" fn-spec)
      (return-from run-sbcl-entrypoint 1))

    (let* ((pkg-name (string-upcase pkg))
           (fn-name (string-upcase fn))
           (args-var (intern "CLPM-RUN-ARGS" "CL-USER"))
           (result-var (intern "CLPM-RUN-RESULT" "CL-USER"))
           (pkg-var (intern "CLPM-RUN-PKG" "CL-USER"))
           (sym-var (intern "CLPM-RUN-SYM" "CL-USER"))
           (call-form
             `(let ((,args-var ',run-args))
                (let* ((,pkg-var (find-package ,pkg-name))
                       (,sym-var (and ,pkg-var (find-symbol ,fn-name ,pkg-var))))
                  (unless (and ,sym-var (fboundp ,sym-var))
                    (format *error-output* "~&Entry function not found: ~A::~A~%" ,pkg-name ,fn-name)
                    (sb-ext:exit :code 1))
                  (let ((,result-var (funcall ,sym-var ,args-var)))
                    (sb-ext:exit :code (if (integerp ,result-var) ,result-var 0))))))
           (call-form-str
             (with-standard-io-syntax
               (let ((*package* (find-package "CL-USER")))
                 (prin1-to-string call-form))))
           (sbcl-args (append (list "sbcl" "--noinform" "--non-interactive" "--disable-debugger"
                                    "--load" (namestring config-path))
                              (sbcl-load-systems-argv deps)
                              (list "--eval" (format nil "(asdf:load-system ~S)" system)
                                    "--eval" call-form-str))))
      (multiple-value-bind (output error-output exit-code)
          (clpm.platform:run-program sbcl-args
                                     :directory project-root
                                     :output :interactive
                                     :error-output :interactive
                                     :timeout 600000)
        (declare (ignore output error-output))
        exit-code))))

(defun cmd-run (&rest args)
  "Run the project entrypoint defined in clpm.project :run."
  (multiple-value-bind (project-root manifest-path lock-path workspace-root _workspace-path)
      (find-effective-project-root)
    (declare (ignore lock-path _workspace-path))
    (unless manifest-path
      (when (null workspace-root)
        (log-no-project-found))
      (return-from cmd-run 1))
    (let* ((project (clpm.project:read-project-file manifest-path))
           (run (clpm.project:project-run project)))
      (unless run
        (log-error "No :run entry configured in clpm.project")
        (return-from cmd-run 1))
      (let ((system (getf run :system))
            (fn-spec (getf run :function)))
        (unless (and (stringp system) (stringp fn-spec))
          (log-error "Invalid :run entry: expected (:system <string> :function <string>)")
          (return-from cmd-run 1))
        (multiple-value-bind (config-path rc)
            (ensure-project-activated project-root)
          (unless (zerop rc)
            (return-from cmd-run rc))
          (let* ((run-args (if (and args (string= (first args) "--"))
                               (rest args)
                               args))
                 (deps (project-dependency-system-ids project '(:depends)))
                 (kind (effective-lisp-kind project)))
            (log-info "Running ~A (~A)..." system fn-spec)
            (run-lisp-entrypoint kind project-root config-path deps system fn-spec run-args)))))))

(defun sbcl-loads-config-p (cmd config-path)
  (let ((abs (namestring config-path))
        (rel ".clpm/asdf-config.lisp"))
    (loop for tail on cmd
          for a = (first tail)
          for b = (second tail)
          when (and b
                    (string= a "--load")
                    (or (string= b abs)
                        (string= b rel)))
            do (return t)
          finally (return nil))))

(defun cmd-exec (&rest args)
  "Run an external command in the project's activated environment.

Usage: clpm exec -- <cmd...>"
  (multiple-value-bind (project-root manifest-path _lock-path workspace-root _workspace-path)
      (find-effective-project-root)
    (declare (ignore _lock-path _workspace-path))
    (unless manifest-path
      (when (null workspace-root)
        (log-no-project-found))
      (return-from cmd-exec 1))
    (let ((cmd args))
      (when (and cmd (string= (first cmd) "--"))
        (setf cmd (rest cmd)))
      (unless cmd
        (log-error "Usage: clpm exec -- <cmd...>")
        (return-from cmd-exec 1))

      (multiple-value-bind (config-path rc)
          (ensure-project-activated project-root)
        (unless (zerop rc)
          (return-from cmd-exec rc))

        (let ((final-cmd
                (if (and cmd (string= (first cmd) "sbcl"))
                    (if (sbcl-loads-config-p cmd config-path)
                        cmd
                        (let* ((prog (first cmd))
                               (rest (rest cmd))
                               (insert-at (or (position-if
                                               (lambda (a)
                                                 (member a '("--eval" "--load" "--script")
                                                         :test #'string=))
                                               rest)
                                              (length rest))))
                          (append (list prog)
                                  (subseq rest 0 insert-at)
                                  (list "--load" (namestring config-path))
                                  (subseq rest insert-at))))
                    cmd)))
          (let* ((env (clpm.platform:which "env"))
                 (cmd-with-env
                   (if env
                       (cons env
                             (cons (format nil "CLPM_PROJECT_ROOT=~A"
                                           (namestring project-root))
                                   final-cmd))
                       final-cmd)))
            (multiple-value-bind (output error-output exit-code)
                (clpm.platform:run-program cmd-with-env
                                           :directory project-root
                                           :output :interactive
                                           :error-output :interactive)
              (declare (ignore output error-output))
              exit-code)))))))

;;; scripts command

(defun parse-script-form (form)
  "Parse a (:script ...) form from clpm.project.

Returns (values script-plist nil) on success.
Returns (values nil error-message) on failure."
  (unless (and (consp form) (eq (car form) :script))
    (return-from parse-script-form
      (values nil (format nil "Script must be a list starting with :script, got ~S" form))))
  (when (oddp (length (cdr form)))
    (return-from parse-script-form
      (values nil (format nil "Script form has an odd number of elements: ~S" form))))
  (let ((name nil)
        (type nil)
        (command nil)
        (system nil)
        (fn nil))
    (loop for (key val) on (cdr form) by #'cddr do
      (case key
        (:name (setf name val))
        (:type (setf type val))
        (:command (setf command val))
        (:system (setf system val))
        (:function (setf fn val))
        (t
         (return-from parse-script-form
           (values nil (format nil "Unknown key in script form: ~S" key))))))
    (unless (and (stringp name) (plusp (length name)))
      (return-from parse-script-form
        (values nil (format nil "Script :name must be a non-empty string, got ~S" name))))
    (unless (keywordp type)
      (return-from parse-script-form
        (values nil (format nil "Script :type must be a keyword, got ~S" type))))
    (case type
      (:shell
       (unless (and (listp command) command (every #'stringp command))
         (return-from parse-script-form
           (values nil (format nil "Shell script :command must be a non-empty list of strings, got ~S" command))))
       (values (list :name name :type :shell :command command) nil))
      (:lisp
       (unless (and (stringp system) (plusp (length system)))
         (return-from parse-script-form
           (values nil (format nil "Lisp script :system must be a non-empty string, got ~S" system))))
       (unless (and (stringp fn) (plusp (length fn)))
         (return-from parse-script-form
           (values nil (format nil "Lisp script :function must be a non-empty string, got ~S" fn))))
       (values (list :name name :type :lisp :system system :function fn) nil))
      (t
       (values nil (format nil "Unsupported script :type: ~S" type)))))
    )

(defun parse-project-scripts (scripts)
  "Validate and parse SCRIPTS from a clpm.project file.

Returns (values parsed-scripts exit-code)."
  (cond
    ((null scripts) (values '() 0))
    ((not (listp scripts))
     (log-error "Invalid :scripts: expected a list, got ~S" scripts)
     (values nil 1))
    (t
     (let ((parsed '())
           (seen (make-hash-table :test #'equal)))
       (dolist (form scripts)
         (multiple-value-bind (script err)
             (parse-script-form form)
           (when err
             (log-error "~A" err)
             (return-from parse-project-scripts (values nil 1)))
           (let ((name (getf script :name)))
             (when (gethash name seen)
               (log-error "Duplicate script name: ~S" name)
               (return-from parse-project-scripts (values nil 1)))
             (setf (gethash name seen) t))
           (push script parsed)))
       (values (nreverse parsed) 0)))))

(defun cmd-scripts (&rest args)
  "List and run project scripts defined in clpm.project."
  (multiple-value-bind (project-root manifest-path lock-path workspace-root _workspace-path)
      (find-effective-project-root)
    (declare (ignore lock-path _workspace-path))
    (unless manifest-path
      (when (null workspace-root)
        (log-no-project-found))
      (return-from cmd-scripts 1))
    (let* ((project (clpm.project:read-project-file manifest-path))
           (scripts (clpm.project:project-scripts project)))
      (multiple-value-bind (parsed rc)
          (parse-project-scripts scripts)
        (unless (zerop rc)
          (return-from cmd-scripts rc))
        (let ((sub (and (first args) (string-downcase (first args)))))
          (cond
            ((or (null sub) (string= sub "help") (string= sub "--help"))
             (log-info "Usage:")
             (log-info "  clpm scripts list")
             (log-info "  clpm scripts run <name> [-- <args...>]")
             0)
            ((string= sub "list")
             (dolist (name (sort (mapcar (lambda (s) (getf s :name)) parsed) #'string<))
               (format t "~A~%" name))
             0)
            ((string= sub "run")
             (let ((name (second args)))
               (unless (and (stringp name) (plusp (length name)))
                 (log-error "Usage: clpm scripts run <name> [-- <args...>]")
                 (return-from cmd-scripts 1))
               (let* ((rest (cddr args))
                      (forward (if (and rest (string= (first rest) "--"))
                                   (rest rest)
                                   rest))
                      (script (find name parsed :test #'string= :key (lambda (s) (getf s :name)))))
                 (unless script
                   (log-error "Unknown script: ~A" name)
                   (return-from cmd-scripts 1))
                 (multiple-value-bind (config-path act-rc)
                     (ensure-project-activated project-root)
                   (unless (zerop act-rc)
                     (return-from cmd-scripts act-rc))
                   (case (getf script :type)
                     (:shell
                      (let* ((cmd (append (getf script :command) forward))
                             (env (clpm.platform:which "env"))
                             (cmd-with-env
                               (if env
                                   (cons env
                                         (cons (format nil "CLPM_PROJECT_ROOT=~A" (namestring project-root))
                                               cmd))
                                   cmd)))
                        (multiple-value-bind (output error-output exit-code)
                            (clpm.platform:run-program cmd-with-env
                                                       :directory project-root
                                                       :output :interactive
                                                       :error-output :interactive)
                          (declare (ignore output error-output))
                          exit-code)))
                     (:lisp
                      (let* ((system (getf script :system))
                             (fn-spec (getf script :function))
                             (deps (project-dependency-system-ids project '(:depends)))
                             (kind (effective-lisp-kind project)))
                        (log-info "Running script ~A (~A)..." name fn-spec)
                        (run-lisp-entrypoint kind project-root config-path deps system fn-spec forward)))
                     (t
                      (log-error "Unsupported script type: ~S" (getf script :type))
                      1))))))
            (t
             (log-error "Usage: clpm scripts <list|run> [args]")
             1)))))))

;;; test command

(defun cmd-test (&rest args)
  "Run project tests.

Uses clpm.project :test metadata:
  :test (:systems (\"<test-system>\" ...))"
  (declare (ignore args))
  (multiple-value-bind (project-root manifest-path lock-path workspace-root _workspace-path)
      (find-effective-project-root)
    (declare (ignore lock-path _workspace-path))
    (unless manifest-path
      (when (null workspace-root)
        (log-no-project-found))
      (return-from cmd-test 1))
    (let* ((project (clpm.project:read-project-file manifest-path))
           (test (clpm.project:project-test project)))
      (unless test
        (log-error "No :test entry configured in clpm.project")
        (return-from cmd-test 1))
      (let ((systems (getf test :systems)))
        (unless (and (listp systems)
                     systems
                     (every #'stringp systems))
          (log-error "Invalid :test entry: expected (:systems (<strings...>))")
          (return-from cmd-test 1))

        (multiple-value-bind (config-path rc)
            (ensure-project-activated project-root)
          (unless (zerop rc)
            (return-from cmd-test rc))

	          (let* ((systems-var (intern "CLPM-TEST-SYSTEMS" "CL-USER"))
	                 (ok-var (intern "CLPM-TEST-OK" "CL-USER"))
	                 (sys-var (intern "CLPM-TEST-SYSTEM" "CL-USER"))
	                 (cond-var (intern "CLPM-TEST-CONDITION" "CL-USER"))
	                 (call-form
	                   `(let ((,systems-var ',systems)
	                          (,ok-var t))
	                      (dolist (,sys-var ,systems-var)
	                        (handler-case
	                            (progn
	                              (format t "~&Testing ~A...~%" ,sys-var)
	                              (asdf:test-system ,sys-var)
	                              (format t "~&OK: ~A~%" ,sys-var))
	                          (error (,cond-var)
	                            (format *error-output* "~&FAIL: ~A: ~A~%" ,sys-var ,cond-var)
	                            (setf ,ok-var nil))))
	                      (uiop:quit (if ,ok-var 0 1))))
	                 (call-form-str
	                   (with-standard-io-syntax
	                     (let ((*package* (find-package "CL-USER")))
	                       (prin1-to-string call-form))))
	                 (deps (project-dependency-system-ids project '(:depends :test-depends)))
                   (kind (effective-lisp-kind project))
                   (eval-forms (append (lisp-load-systems-eval-forms deps)
                                       (list call-form-str))))
              (run-lisp-with-config kind project-root config-path eval-forms)))))))

;;; package command

(defun cmd-package (&rest args)
  "Build a distributable executable according to clpm.project :package metadata.

Manifest schema:
  :package (:output \"dist/<name>\" :system \"<system>\" :function \"<package>::<fn>\")"
  (declare (ignore args))
  (labels ((chmod-755 (path)
             (let ((path (namestring (uiop:ensure-pathname path :want-existing nil))))
               (when (uiop:os-windows-p)
                 (return-from chmod-755 t))
               (handler-case
                   (let ((proc (sb-ext:run-program "chmod" (list "+x" path)
                                                   :search t
                                                   :output nil
                                                   :error nil
                                                   :wait t)))
                     (zerop (sb-ext:process-exit-code proc)))
                 (error ()
                   nil))))
           (write-sbcl-wrapper (wrapper-path bin-basename)
             (let ((wrapper-path (uiop:ensure-pathname wrapper-path :want-existing nil :want-file t)))
               (ensure-directories-exist wrapper-path)
               (with-open-file (s wrapper-path :direction :output
                                       :if-exists :supersede
                                       :external-format :utf-8)
                 (write-line "#!/bin/sh" s)
                 (write-line "set -e" s)
                 (format s "exec \"$(dirname \"$0\")/~A\" --end-runtime-options \"$@\"~%"
                         bin-basename))
               (unless (chmod-755 wrapper-path)
                 (log-error "Failed to mark wrapper executable: ~A" (namestring wrapper-path))
                 (return-from cmd-package 1)))))
  (multiple-value-bind (project-root manifest-path lock-path workspace-root _workspace-path)
      (find-effective-project-root)
    (declare (ignore _workspace-path))
    (unless manifest-path
      (when (null workspace-root)
        (log-no-project-found))
      (return-from cmd-package 1))
    (let* ((project (clpm.project:read-project-file manifest-path))
           (pkg (clpm.project:project-package project)))
      (let ((kind (effective-lisp-kind project)))
        (unless (eq kind :sbcl)
          (log-error "Packaging currently supports SBCL only; re-run with --lisp sbcl")
          (return-from cmd-package 1)))
      (unless pkg
        (log-error "No :package entry configured in clpm.project")
        (return-from cmd-package 1))
      (let ((output (getf pkg :output))
            (system (getf pkg :system))
            (fn-spec (getf pkg :function)))
        (unless (and (stringp output) (stringp system) (stringp fn-spec))
          (log-error "Invalid :package entry: expected (:output <string> :system <string> :function <string>)")
          (return-from cmd-package 1))

        ;; Ensure lockfile + activation config exist.
        (let ((config-path (merge-pathnames ".clpm/asdf-config.lisp" project-root)))
          (when (or (null lock-path)
                    (not (uiop:file-exists-p config-path)))
            (log-info "Ensuring project is installed before packaging...")
            (let ((rc (uiop:with-current-directory (project-root)
                        (cmd-install))))
              (unless (zerop rc)
                (return-from cmd-package rc)))
            (setf lock-path (merge-pathnames "clpm.lock" project-root)))
          (unless (and lock-path (uiop:file-exists-p lock-path))
            (log-error "Missing clpm.lock - run 'clpm install' first")
            (return-from cmd-package 1))
          (unless (uiop:file-exists-p config-path)
            (log-error "Missing activation config - run 'clpm install' first")
            (return-from cmd-package 1))

          (multiple-value-bind (pkg-name fn-name)
              (parse-function-spec fn-spec)
            (unless (and pkg-name fn-name)
              (log-error "Invalid :package :function: expected <package>::<fn>, got ~S" fn-spec)
              (return-from cmd-package 1))

	            (let* ((expanded-output (clpm.platform:expand-path output))
	                   (output-path (uiop:ensure-pathname expanded-output
	                                                     :defaults project-root
	                                                     :want-existing nil
	                                                     :want-file t))
                     (bin-path (uiop:ensure-pathname (format nil "~A.bin" (namestring output-path))
                                                     :want-existing nil
                                                     :want-file t))
	                   (lock-sha256
	                     (clpm.crypto.sha256:bytes-to-hex
	                      (clpm.crypto.sha256:sha256-file lock-path)))
	                   (meta-path (make-pathname :name (format nil "~A.meta"
	                                                          (pathname-name output-path))
                                             :type "sxp"
                                             :defaults output-path))
                   (pkg-key (intern (string-upcase pkg-name) :keyword))
                   (fn-key (intern (string-upcase fn-name) :keyword))
                   (main-sym (intern "CLPM-PACKAGE-MAIN" "CL-USER"))
                   (args-var (intern "CLPM-PACKAGE-ARGS" "CL-USER"))
                   (result-var (intern "CLPM-PACKAGE-RESULT" "CL-USER"))
	                   (defun-form
	                     `(defun ,main-sym ()
	                        (let* ((,args-var (uiop:command-line-arguments))
	                               (,result-var (uiop:symbol-call ,pkg-key ,fn-key ,args-var)))
	                          (sb-ext:exit :code (if (integerp ,result-var) ,result-var 0)))))
	                   (save-form
	                     `(sb-ext:save-lisp-and-die ,(namestring bin-path)
	                                                :toplevel ',main-sym
	                                                :executable t
	                                                :compression t))
	                   (defun-str
	                     (with-standard-io-syntax
                       (let ((*package* (find-package "CL-USER")))
                         (prin1-to-string defun-form))))
		                   (save-str
		                     (with-standard-io-syntax
		                       (let ((*package* (find-package "CL-USER")))
		                         (prin1-to-string save-form))))
			                   (deps (project-dependency-system-ids project '(:depends)))
			                   (sbcl-args (append (list "sbcl" "--noinform" "--non-interactive" "--disable-debugger"
		                                            "--load" (namestring config-path))
		                                      (sbcl-load-systems-argv deps)
		                                      (list "--eval" "(ignore-errors (require :sb-posix))"
		                                            "--eval" (format nil "(asdf:load-system ~S)" system)
		                                            "--eval" defun-str
		                                            "--eval" save-str))))
		              (ensure-directories-exist output-path)

              (log-info "Packaging ~A -> ~A" system (namestring output-path))
              (multiple-value-bind (out err rc)
                  (clpm.platform:run-program sbcl-args
                                             :directory project-root
                                             :output :interactive
                                             :error-output :interactive
                                             :timeout 600000)
	                (declare (ignore out err))
	                (unless (zerop rc)
	                  (log-error "Packaging failed (exit code ~D)" rc)
	                  (return-from cmd-package rc)))

	              ;; Write wrapper to ensure SBCL runtime options don't steal flags.
	              (write-sbcl-wrapper output-path (file-namestring bin-path))

	              (clpm.io.sexp:write-canonical-sexp-to-file
	               `(:package-meta
	                 :lock-sha256 ,lock-sha256
	                 :sbcl-version ,(clpm.platform:sbcl-version)
                 :platform ,(clpm.platform:platform-triple))
               meta-path)
              (log-info "Wrote package metadata: ~A" (namestring meta-path))
              0))))))))

;;; clean command

(defun cmd-clean (&rest args)
  "Clean project-local outputs."
  (let ((clean-dist nil)
        (rest args))
    (loop while rest do
      (let ((arg (pop rest)))
        (cond
          ((string= arg "--dist")
           (setf clean-dist t))
          (t
           (log-error "Unknown option: ~A" arg)
           (log-error "Usage: clpm clean [--dist]")
           (return-from cmd-clean 1)))))
    (multiple-value-bind (project-root manifest-path lock-path workspace-root _workspace-path)
        (find-effective-project-root)
      (declare (ignore manifest-path lock-path _workspace-path))
      (unless project-root
        (when (null workspace-root)
          (log-no-project-found))
        (return-from cmd-clean 1))
      (let ((clpm-dir (merge-pathnames ".clpm/" project-root))
            (dist-dir (merge-pathnames "dist/" project-root)))
        (when (uiop:directory-exists-p clpm-dir)
          (log-info "Removing ~A" (namestring clpm-dir))
          (uiop:delete-directory-tree clpm-dir :validate t))
        (when (and clean-dist (uiop:directory-exists-p dist-dir))
          (log-info "Removing ~A" (namestring dist-dir))
          (uiop:delete-directory-tree dist-dir :validate t))
        0))))

;;; gc command

(defun cmd-gc (&key dry-run)
  "Garbage collect unused store entries."
  (log-info "~:[Garbage collecting~;Checking~] store..."
            dry-run)
  (let ((deleted (clpm.store:gc-store :dry-run dry-run)))
    (if deleted
        (log-info "~:[Deleted~;Would delete~] ~D entries"
                  dry-run (length deleted))
        (log-info "Nothing to clean"))
    0))

;;; Helper functions

(defun load-project-registries (project)
  "Load registries specified in PROJECT."
  (clpm.platform:ensure-directories)
  (let ((registries '())
        (refs (nth-value 0 (clpm.config:merge-project-config project))))
    (dolist (ref refs)
      (let* ((name (clpm.project:registry-ref-name ref))
             (kind (clpm.project:registry-ref-kind ref))
             (url (clpm.project:registry-ref-url ref))
             (trust (clpm.project:registry-ref-trust ref))
             (ql-systems (clpm.project:registry-ref-quicklisp-systems-sha256 ref))
             (ql-releases (clpm.project:registry-ref-quicklisp-releases-sha256 ref)))
        (log-verbose "Loading registry: ~A" name)
        (push (clpm.registry:clone-registry name url
                                            :trust-key trust
                                            :quicklisp-systems-sha256 ql-systems
                                            :quicklisp-releases-sha256 ql-releases
                                            :kind kind)
              registries)))
    (nreverse registries)))

;;; workspace command

(defun %normalize-workspace-member-arg (raw)
  "Normalize and validate a workspace member path string."
  (unless (and (stringp raw) (plusp (length raw)))
    (log-error "Workspace member path must be a non-empty string, got ~S" raw)
    (return-from %normalize-workspace-member-arg nil))
  (let ((trimmed (string-right-trim '(#\/ #\\) raw)))
    (when (zerop (length trimmed))
      (log-error "Workspace member path must not be empty")
      (return-from %normalize-workspace-member-arg nil))
    (let ((pn (uiop:ensure-pathname trimmed
                                    :defaults (uiop:getcwd)
                                    :want-relative nil
                                    :want-absolute nil)))
      (when (uiop:absolute-pathname-p pn)
        (log-error "Workspace member path must be relative, got ~S" raw)
        (return-from %normalize-workspace-member-arg nil)))
    trimmed))

(defun cmd-workspace (&rest args)
  "Manage workspaces and workspace members."
  (labels ((usage-error (fmt &rest fmt-args)
             (apply #'log-error fmt fmt-args)
             (log-error "Usage:")
             (log-error "  clpm workspace init [--dir <path>]")
             (log-error "  clpm workspace add <member> [--dir <path>]")
             (log-error "  clpm workspace list [--dir <path>]")
             (return-from cmd-workspace 1))
           (parse-dir-arg (rest)
             (let ((dir nil)
                   (out '()))
               (loop while rest do
                 (let ((a (pop rest)))
                   (cond
                     ((string= a "--dir")
                      (setf dir (pop rest))
                      (unless (and (stringp dir) (plusp (length dir)))
                        (usage-error "Missing value for --dir")))
                     (t
                      (push a out)))))
               (values dir (nreverse out))))
           (workspace-root-and-path (dir)
             (if dir
                 (let* ((root (uiop:ensure-directory-pathname
                               (clpm.platform:expand-path dir)))
                        (path (merge-pathnames "clpm.workspace" root)))
                   (values root path))
                 (clpm.workspace:find-workspace-root (uiop:getcwd)))))
    (let* ((sub (and (first args) (string-downcase (first args))))
           (rest (rest args)))
      (cond
        ((or (null sub) (string= sub "help") (string= sub "--help"))
         (log-info "Usage:")
         (log-info "  clpm workspace init [--dir <path>]")
         (log-info "  clpm workspace add <member> [--dir <path>]")
         (log-info "  clpm workspace list [--dir <path>]")
         0)
        ((string= sub "init")
         (multiple-value-bind (dir extra)
             (parse-dir-arg rest)
           (when extra
             (usage-error "Unexpected arguments: ~{~A~^ ~}" extra))
           (let ((root (if dir
                           (uiop:ensure-directory-pathname
                            (clpm.platform:expand-path dir))
                           (uiop:getcwd))))
             (let ((path (merge-pathnames "clpm.workspace" root)))
               (when (uiop:file-exists-p path)
                 (usage-error "clpm.workspace already exists: ~A" (namestring path)))
               (clpm.workspace:write-workspace-file
                (clpm.workspace:make-workspace :format 1 :members '())
                path)
               (log-info "Initialized workspace: ~A" (namestring root))
               0))))
        ((string= sub "add")
         (multiple-value-bind (dir extra)
             (parse-dir-arg rest)
           (let ((member (first extra))
                 (extra (rest extra)))
             (when (or (null member) extra)
               (usage-error "Usage: clpm workspace add <member> [--dir <path>]"))
             (let ((norm (%normalize-workspace-member-arg member)))
               (unless norm
                 (return-from cmd-workspace 1))
               (multiple-value-bind (root ws-path)
                   (workspace-root-and-path dir)
                 (unless (and root ws-path (uiop:file-exists-p ws-path))
                   (usage-error "No clpm.workspace found (run: clpm workspace init)"))
                 (let* ((ws (clpm.workspace:read-workspace-file ws-path))
                        (members (sort (remove-duplicates
                                        (append (or (clpm.workspace:workspace-members ws) '())
                                                (list norm))
                                        :test #'string=)
                                       #'string<)))
                   (setf (clpm.workspace:workspace-members ws) members)
                   (clpm.workspace:write-workspace-file ws ws-path)
                   (log-info "Added member: ~A" norm)
                   0))))))
        ((string= sub "list")
         (multiple-value-bind (dir extra)
             (parse-dir-arg rest)
           (when extra
             (usage-error "Unexpected arguments: ~{~A~^ ~}" extra))
           (multiple-value-bind (root ws-path)
               (workspace-root-and-path dir)
             (declare (ignore root))
             (unless (and ws-path (uiop:file-exists-p ws-path))
               (usage-error "No clpm.workspace found (run: clpm workspace init)"))
             (let* ((ws (clpm.workspace:read-workspace-file ws-path))
                    (members (sort (copy-list (or (clpm.workspace:workspace-members ws) '()))
                                   #'string<)))
               (dolist (m members)
                 (format t "~A~%" m))
               0))))
        (t
         (usage-error "Unknown subcommand: ~A" sub))))))

;;; registry command

(defun cmd-registry (&rest args)
  "Manage global registries in config.sxp."
  (let ((subcommand (first args))
        (rest (rest args)))
    (cond
      ((or (null subcommand) (string= subcommand "help"))
       (log-error "Usage: clpm registry <list|add|update|trust> [options]")
       (return-from cmd-registry 1))

      ((string= subcommand "list")
       (let* ((cfg (clpm.config:read-config))
              (regs (clpm.config:config-registries cfg)))
         (if (null regs)
             (log-info "No registries configured")
             (dolist (r regs)
               (log-info "~A  ~A~@[  (~A)~]"
                         (clpm.project:registry-ref-name r)
                         (clpm.project:registry-ref-url r)
                         (clpm.project:registry-ref-trust r)))))
       0)

	      ((string= subcommand "add")
	       (let ((name nil)
	             (url nil)
	             (trust nil)
	             (kind :git))
	         (labels ((normalize-trust-arg (s)
	                    (cond
	                      ((null s) nil)
	                      ((or (string= s "none") (string= s "nil")) nil)
	                      (t s))))
	           (loop while rest do
	             (let ((arg (pop rest)))
	               (cond
	                 ((string= arg "--name") (setf name (pop rest)))
	                 ((string= arg "--url") (setf url (pop rest)))
	                 ((string= arg "--trust") (setf trust (normalize-trust-arg (pop rest))))
	                 ((string= arg "--quicklisp") (setf kind :quicklisp))
	                 (t
	                  (log-error "Unknown option: ~A" arg)
	                  (return-from cmd-registry 1)))))

	           (when (eq kind :quicklisp)
	             (unless name
	               (setf name "quicklisp"))
	             (unless url
	               (setf url "https://beta.quicklisp.org/dist/quicklisp.txt"))
	             (unless trust
	               ;; Safer default: pin distinfo digest on first use (TOFU).
	               (setf trust "tofu")))

	           (case kind
	             (:git
	              (unless (and name url trust)
	                (log-error "Missing required options: --name, --url, --trust")
	                (return-from cmd-registry 1)))
	             (:quicklisp
	              (unless (and name url)
	                (log-error "Missing required options: --name, --url")
	                (return-from cmd-registry 1)))
	             (t
	              (log-error "Unknown registry kind: ~S" kind)
	              (return-from cmd-registry 1)))

	           (let* ((cfg (clpm.config:read-config))
	                  (regs (clpm.config:config-registries cfg))
	                  (existing (find name regs
	                                  :key #'clpm.project:registry-ref-name
	                                  :test #'string=)))
	             (if existing
	                 (progn
	                   (setf (clpm.project:registry-ref-kind existing) kind
	                         (clpm.project:registry-ref-url existing) url
	                         (clpm.project:registry-ref-trust existing) trust)
	                   (log-info "Updated registry: ~A" name))
	                 (progn
	                   (push (clpm.project::make-registry-ref
	                          :kind kind
	                          :name name
	                          :url url
	                          :trust trust)
	                         regs)
	                   (setf (clpm.config:config-registries cfg) regs)
	                   (log-info "Added registry: ~A" name)))
	             (clpm.config:write-config cfg))
	           0)))

      ((string= subcommand "init")
       (let ((dir nil)
             (key-id nil)
             (keys-dir nil))
         (loop while rest do
           (let ((arg (pop rest)))
             (cond
               ((string= arg "--dir") (setf dir (pop rest)))
               ((string= arg "--key-id") (setf key-id (pop rest)))
               ((string= arg "--keys-dir") (setf keys-dir (pop rest)))
               (t
                (log-error "Unknown option: ~A" arg)
                (log-error "Usage: clpm registry init --dir <path> --key-id <id> --keys-dir <dir>")
                (return-from cmd-registry 1)))))
         (unless (and (stringp dir) (plusp (length dir)))
           (log-error "Missing --dir <path>")
           (return-from cmd-registry 1))
         (unless (%key-id-valid-p key-id)
           (log-error "Invalid --key-id (use [A-Za-z0-9._-]+): ~S" key-id)
           (return-from cmd-registry 1))
         (unless (and (stringp keys-dir) (plusp (length keys-dir)))
           (log-error "Missing --keys-dir <dir>")
           (return-from cmd-registry 1))

         (let* ((root (uiop:ensure-directory-pathname
                       (clpm.platform:expand-path dir)))
                (keys-root (uiop:ensure-directory-pathname
                            (clpm.platform:expand-path keys-dir)))
                (snapshot-path (merge-pathnames "registry/snapshot.sxp" root))
                (sig-path (merge-pathnames "registry/snapshot.sig" root))
                (embedded-pub-path (merge-pathnames (format nil "registry/keys/~A.pub" key-id)
                                                    root))
                (packages-dir (merge-pathnames "registry/packages/" root))
                (priv-key-path (merge-pathnames (format nil "~A.key" key-id) keys-root))
                (pub-key-path (merge-pathnames (format nil "~A.pub" key-id) keys-root)))

           (unless (uiop:file-exists-p priv-key-path)
             (log-error "Missing private key: ~A" (namestring priv-key-path))
             (return-from cmd-registry 1))
           (unless (uiop:file-exists-p pub-key-path)
             (log-error "Missing public key: ~A" (namestring pub-key-path))
             (return-from cmd-registry 1))
           (when (or (uiop:file-exists-p snapshot-path)
                     (uiop:file-exists-p sig-path))
             (log-error "Registry already initialized at: ~A" (namestring root))
             (return-from cmd-registry 1))

           (ensure-directories-exist snapshot-path)
           (ensure-directories-exist embedded-pub-path)
           (ensure-directories-exist packages-dir)

           ;; Write initial snapshot.
           (clpm.io.sexp:write-canonical-sexp-to-file
            `(:snapshot
              :format 1
              :generated-at ,(clpm.project:rfc3339-timestamp)
              :releases ()
              :provides ())
            snapshot-path
            :pretty t)

           ;; Sign snapshot and write detached signature as hex.
           (labels ((read-seed32 (path)
                      (let* ((text (uiop:read-file-string path))
                             (trim (string-trim '(#\Space #\Newline #\Return #\Tab) text))
                             (bytes (clpm.crypto.sha256:hex-to-bytes trim)))
                        (unless (= (length bytes) 32)
                          (log-error "Invalid private key seed length (expected 32 bytes): ~A"
                                     (namestring path))
                          (return-from cmd-registry 1))
                        bytes))
                    (read-file-bytes (path)
                      (with-open-file (s path :element-type '(unsigned-byte 8))
                        (let ((data (make-array (file-length s)
                                                :element-type '(unsigned-byte 8))))
                          (read-sequence data s)
                          data))))
             (let* ((seed (read-seed32 priv-key-path))
                    (msg (read-file-bytes snapshot-path))
                    (sig (clpm.crypto.ed25519:sign msg seed))
                    (sig-hex (clpm.crypto.sha256:bytes-to-hex sig)))
               (with-open-file (s sig-path :direction :output
                                           :if-exists :error
                                           :external-format :utf-8)
                 (write-string sig-hex s)
                 (terpri s))))

           ;; Embed public key.
           (uiop:copy-file pub-key-path embedded-pub-path)

           (log-info "Initialized registry at: ~A" (namestring root))
           (log-info "Snapshot: ~A" (namestring snapshot-path))
           (log-info "Signature: ~A" (namestring sig-path))
           (log-info "Key: ~A" (namestring embedded-pub-path))
           0)))

      ((string= subcommand "update")
       (let* ((refresh-trust nil)
              (names '())
              (cfg (clpm.config:read-config))
              (refs (clpm.config:config-registries cfg)))
         (loop while rest do
           (let ((arg (pop rest)))
             (cond
               ((string= arg "--refresh-trust")
                (setf refresh-trust t))
               ((and (stringp arg) (plusp (length arg)) (char= (char arg 0) #\-))
                (log-error "Unknown option: ~A" arg)
                (return-from cmd-registry 1))
               (t
                (push arg names)))))
         (setf names (nreverse names))
         (when (null names)
           (setf names nil))
         (dolist (ref refs)
           (let ((name (clpm.project:registry-ref-name ref)))
             (when (or (null names) (member name names :test #'string=))
               (log-info "Updating registry: ~A" name)
               (handler-case
                   (let ((reg (clpm.registry:clone-registry
                               name
                               (clpm.project:registry-ref-url ref)
                               :trust-key (clpm.project:registry-ref-trust ref)
                               :kind (clpm.project:registry-ref-kind ref)
                               :refresh-trust refresh-trust)))
                     (clpm.registry:update-registry reg :refresh-trust refresh-trust))
                 (error (c)
                   (log-error "Failed to update registry ~A: ~A" name c)
                   (return-from cmd-registry 1))))))
         0))

      ((string= subcommand "trust")
       (let ((action (first rest))
             (rest (rest rest)))
         (labels ((usage-error (fmt &rest fmt-args)
                    (apply #'log-error fmt fmt-args)
                    (log-error "Usage: clpm registry trust <list|set|refresh> [args]")
                    (return-from cmd-registry 1))
                  (kind->string (k)
                    (string-downcase (symbol-name k)))
                  (normalize-trust-arg (s)
                    (cond
                      ((null s) nil)
                      ((or (string= s "none") (string= s "nil")) nil)
                      (t s))))
           (cond
             ((or (null action) (string= action "help"))
              (usage-error "Missing trust subcommand"))

             ((string= action "list")
              (let* ((cfg (clpm.config:read-config))
                     (refs (clpm.config:config-registries cfg)))
                (if (null refs)
                    (log-info "No registries configured")
                    (dolist (ref (sort (copy-list refs) #'string<
                                       :key (lambda (r)
                                              (or (clpm.project:registry-ref-name r) ""))))
                      (let* ((name (clpm.project:registry-ref-name ref))
                             (kind (clpm.project:registry-ref-kind ref))
                             (trust (clpm.project:registry-ref-trust ref)))
                        (log-info "~A~C~A~C~A"
                                  name #\Tab (kind->string kind) #\Tab (or trust "-"))))))
              0)

             ((string= action "set")
              (let ((name (first rest))
                    (trust-raw (second rest)))
                (unless (and (stringp name) (plusp (length name))
                             (stringp trust-raw) (plusp (length trust-raw)))
                  (usage-error "Usage: clpm registry trust set <name> <trust>"))
                (let* ((trust (normalize-trust-arg trust-raw))
                       (cfg (clpm.config:read-config))
                       (refs (clpm.config:config-registries cfg))
                       (ref (find name refs
                                  :key #'clpm.project:registry-ref-name
                                  :test #'string=)))
                  (unless ref
                    (usage-error "Unknown registry: ~A" name))
                  (setf (clpm.project:registry-ref-trust ref) trust)
                  (clpm.config:write-config cfg)
                  (log-info "Updated trust for ~A" name)
                  0)))

             ((string= action "refresh")
              (let ((name (first rest)))
                (unless (and (stringp name) (plusp (length name)) (null (rest rest)))
                  (usage-error "Usage: clpm registry trust refresh <name>"))
                (let* ((cfg (clpm.config:read-config))
                       (refs (clpm.config:config-registries cfg))
                       (ref (find name refs
                                  :key #'clpm.project:registry-ref-name
                                  :test #'string=)))
                  (unless ref
                    (usage-error "Unknown registry: ~A" name))
                  (let ((kind (clpm.project:registry-ref-kind ref)))
                    (unless (eq kind :quicklisp)
                      (usage-error "Trust refresh is only supported for Quicklisp registries"))
                    (let ((reg (clpm.registry:clone-registry
                                name
                                (clpm.project:registry-ref-url ref)
                                :trust-key (clpm.project:registry-ref-trust ref)
                                :kind kind
                                :refresh-trust t)))
                      (clpm.registry:update-registry reg :refresh-trust t)
                      (log-info "Refreshed trust for ~A" name)
                      0)))))

             (t
              (usage-error "Unknown trust subcommand: ~A" action))))))

      (t
       (log-error "Unknown registry subcommand: ~A" subcommand)
       1))))

(defun collect-source-paths (lockfile)
  "Collect source paths from lockfile."
  (let ((paths '()))
    (dolist (locked (clpm.project:lockfile-resolved lockfile))
      (let* ((system-id (clpm.project:locked-system-id locked))
             (release (clpm.project:locked-system-release locked))
             (tree-sha256 (clpm.project:locked-release-tree-sha256 release))
             (source-path (when tree-sha256
                            (clpm.store:get-source-path tree-sha256))))
        (when source-path
          (push (cons system-id source-path) paths))))
    (nreverse paths)))

;;; keys command

(defun %key-id-valid-p (id)
  (and (stringp id)
       (plusp (length id))
       (every (lambda (c)
                (or (and (char>= c #\a) (char<= c #\z))
                    (and (char>= c #\A) (char<= c #\Z))
                    (and (char>= c #\0) (char<= c #\9))
                    (member c '(#\- #\_ #\.))))
              id)))

(defun %chmod-if-available (path mode-string)
  (let ((chmod (clpm.platform:which "chmod")))
    (when chmod
      (clpm.platform:run-program (list chmod mode-string (namestring path))
                                 :output nil
                                 :error-output nil))))

(defun cmd-keys (&rest args)
  "Manage Ed25519 keys used for registry signing."
  (let ((subcommand (first args))
        (rest (rest args)))
    (labels ((usage-error (fmt &rest fmt-args)
               (apply #'log-error fmt fmt-args)
               (log-error "Usage: clpm keys generate --out <dir> --id <id>")
               (return-from cmd-keys 1)))
      (cond
        ((or (null subcommand) (string= subcommand "help"))
         (usage-error "Missing keys subcommand"))
        ((string= subcommand "generate")
         (let ((out nil)
               (id nil))
           (loop while rest do
             (let ((arg (pop rest)))
               (cond
                 ((string= arg "--out")
                  (setf out (pop rest)))
                 ((string= arg "--id")
                  (setf id (pop rest)))
                 (t
                  (usage-error "Unknown option: ~A" arg)))))
           (unless (and (stringp out) (plusp (length out)))
             (usage-error "Missing --out <dir>"))
           (unless (%key-id-valid-p id)
             (usage-error "Invalid --id value (use [A-Za-z0-9._-]+): ~S" id))

           (let* ((out-dir (uiop:ensure-directory-pathname
                            (clpm.platform:expand-path out)))
                  (priv-path (merge-pathnames (format nil "~A.key" id) out-dir))
                  (pub-path (merge-pathnames (format nil "~A.pub" id) out-dir)))
             (when (or (uiop:file-exists-p priv-path) (uiop:file-exists-p pub-path))
               (log-error "Key files already exist in: ~A" (namestring out-dir))
               (return-from cmd-keys 1))
             (ensure-directories-exist out-dir)
             (let* ((seed (clpm.platform:secure-random-bytes 32))
                    (pub (clpm.crypto.ed25519:derive-public-key-from-seed seed))
                    (seed-hex (clpm.crypto.sha256:bytes-to-hex seed))
                    (pub-hex (clpm.crypto.sha256:bytes-to-hex pub)))
               (with-open-file (s priv-path :direction :output
                                           :if-exists :error
                                           :external-format :utf-8)
                 (write-string seed-hex s)
                 (terpri s))
               (%chmod-if-available priv-path "600")
               (with-open-file (s pub-path :direction :output
                                          :if-exists :error
                                          :external-format :utf-8)
                 (write-string pub-hex s)
                 (terpri s))
               (%chmod-if-available pub-path "644")
               (log-info "Wrote private key: ~A" (namestring priv-path))
               (log-info "Wrote public key:  ~A" (namestring pub-path))
               0))))
        (t
         (usage-error "Unknown keys subcommand: ~A" subcommand))))))

;;; publish command

(defun %normalize-asdf-dep (dep)
  (cond
    ((stringp dep) dep)
    ((symbolp dep) (string-downcase (symbol-name dep)))
    (t nil)))

(defun %compute-system-deps-from-asd (project-root systems)
  "Compute system dependencies for SYSTEMS by loading their .asd files.

Returns an alist: (system-id . ((dep-system . nil) ...))."
  (let ((project-root (uiop:ensure-directory-pathname project-root))
        (results '()))
    (dolist (sys systems)
      (unless (and (stringp sys) (plusp (length sys)))
        (log-error "Invalid system name in project :systems: ~S" sys)
        (return-from %compute-system-deps-from-asd nil))
      (let ((asd-path (merge-pathnames (format nil "~A.asd" sys) project-root)))
        (unless (uiop:file-exists-p asd-path)
          (log-error "Missing .asd for system ~A: ~A" sys (namestring asd-path))
          (return-from %compute-system-deps-from-asd nil))
        (handler-case
            (progn
              (asdf:load-asd asd-path)
              (let* ((system (asdf:find-system sys nil))
                     (deps-raw (and system (asdf:system-depends-on system)))
                     (deps (remove nil (mapcar #'%normalize-asdf-dep deps-raw))))
                (setf deps (sort (remove-duplicates deps :test #'string=) #'string<))
                (push (cons sys (mapcar (lambda (d) (cons d nil)) deps)) results)))
          (error (c)
            (log-error "Failed to read ASDF deps for ~A: ~A" sys c)
            (return-from %compute-system-deps-from-asd nil)))))
    (sort results #'string< :key #'car)))

(defun cmd-publish (&rest args)
  "Publish a project into a local git registry directory (writes signed metadata)."
  (let ((registry nil)
        (key-id nil)
        (keys-dir nil)
        (project-dir nil)
        (tarball-url nil)
        (tarball-out nil)
        (git-commit-p nil))
    (labels ((usage-error (fmt &rest fmt-args)
               (apply #'log-error fmt fmt-args)
               (log-error "Usage: clpm publish --registry <dir> --key-id <id> --keys-dir <dir> --tarball-url <url> [--tarball-out <path>] [--project <dir>] [--git-commit]")
               (return-from cmd-publish 1)))
      ;; Parse args.
      (loop while args do
        (let ((arg (pop args)))
          (cond
            ((string= arg "--registry") (setf registry (pop args)))
            ((string= arg "--key-id") (setf key-id (pop args)))
            ((string= arg "--keys-dir") (setf keys-dir (pop args)))
            ((string= arg "--project") (setf project-dir (pop args)))
            ((string= arg "--tarball-url") (setf tarball-url (pop args)))
            ((string= arg "--tarball-out") (setf tarball-out (pop args)))
            ((string= arg "--git-commit") (setf git-commit-p t))
            (t
             (usage-error "Unknown option: ~A" arg)))))

      (unless (and (stringp registry) (plusp (length registry)))
        (usage-error "Missing --registry <dir>"))
      (unless (%key-id-valid-p key-id)
        (usage-error "Invalid --key-id (use [A-Za-z0-9._-]+): ~S" key-id))
      (unless (and (stringp keys-dir) (plusp (length keys-dir)))
        (usage-error "Missing --keys-dir <dir>"))
      (unless (and (stringp tarball-url) (plusp (length tarball-url)))
        (usage-error "Missing --tarball-url <url>"))
      (when tarball-out
        (unless (and (stringp tarball-out) (plusp (length tarball-out)))
          (usage-error "Missing --tarball-out <path>")))

      (let* ((registry-root (uiop:ensure-directory-pathname
                             (clpm.platform:expand-path registry)))
             (keys-root (uiop:ensure-directory-pathname
                         (clpm.platform:expand-path keys-dir)))
             (project-root
               (if project-dir
                   (uiop:ensure-directory-pathname (clpm.platform:expand-path project-dir))
                   (nth-value 0 (clpm.project:find-project-root))))
             (manifest-path (and project-root (merge-pathnames "clpm.project" project-root))))
        (unless (uiop:directory-exists-p registry-root)
          (usage-error "Registry path does not exist or is not a directory: ~A" registry))
        (unless (uiop:file-exists-p (merge-pathnames "registry/snapshot.sxp" registry-root))
          (usage-error "Registry is missing registry/snapshot.sxp: ~A" (namestring registry-root)))
        (unless (and project-root manifest-path (uiop:file-exists-p manifest-path))
          (usage-error "Missing clpm.project (use --project <dir> from a project root)"))

        (let* ((project (clpm.project:read-project-file manifest-path))
               (name (clpm.project:project-name project))
               (version (clpm.project:project-version project))
               (systems (clpm.project:project-systems project)))
          (unless (and (stringp name) (plusp (length name))
                       (stringp version) (plusp (length version)))
            (usage-error "Project :name and :version must be set in clpm.project"))
          (unless (and (listp systems) (every #'stringp systems) systems)
            (usage-error "Project :systems must be a non-empty list of strings"))

          (let* ((priv-key-path (merge-pathnames (format nil "~A.key" key-id) keys-root))
                 (pub-key-path (merge-pathnames (format nil "~A.pub" key-id) keys-root)))
            (unless (uiop:file-exists-p priv-key-path)
              (usage-error "Missing private key: ~A" (namestring priv-key-path)))
            (unless (uiop:file-exists-p pub-key-path)
              (usage-error "Missing public key: ~A" (namestring pub-key-path)))

            (labels ((read-seed32 (path)
                       (let* ((text (uiop:read-file-string path))
                              (trim (string-trim '(#\Space #\Newline #\Return #\Tab) text))
                              (bytes (clpm.crypto.sha256:hex-to-bytes trim)))
                         (unless (= (length bytes) 32)
                           (usage-error "Invalid private key seed length (expected 32 bytes): ~A"
                                        (namestring path)))
                         bytes))
                     (read-file-bytes (path)
                       (with-open-file (s path :element-type '(unsigned-byte 8))
                         (let ((data (make-array (file-length s)
                                                 :element-type '(unsigned-byte 8))))
                           (read-sequence data s)
                           data)))
                     (write-sig-hex (msg-bytes sig-path seed)
                       (let* ((sig (clpm.crypto.ed25519:sign msg-bytes seed))
                              (sig-hex (clpm.crypto.sha256:bytes-to-hex sig)))
                         (with-open-file (s sig-path :direction :output
                                                     :if-exists :supersede
                                                     :external-format :utf-8)
                           (write-string sig-hex s)
                           (terpri s)))))
              (let* ((seed (read-seed32 priv-key-path))
                     ;; Create tarball (optionally at a user-provided path).
                     (tar (clpm.platform:find-tar)))
                (unless tar
                  (error 'clpm.errors:clpm-missing-tool-error
                         :tool "tar"
                         :install-hints (clpm.platform:tool-install-hints "tar")))
                (clpm.store:with-temp-dir (tmp)
                  (let* ((tarball-name (format nil "~A-~A.tar.gz" name version))
                         (tarball-path
                           (if tarball-out
                               (let* ((pn (uiop:ensure-pathname
                                           (clpm.platform:expand-path tarball-out)
                                           :defaults project-root
                                           :want-existing nil))
                                      (pn (if (uiop:directory-pathname-p pn)
                                              (merge-pathnames tarball-name (uiop:ensure-directory-pathname pn))
                                              pn)))
                                 pn)
                               (merge-pathnames tarball-name tmp))))
                    (ensure-directories-exist tarball-path)
                    (multiple-value-bind (_out err exit-code)
                        (clpm.platform:run-program
                         (list tar
                               "-czf" (namestring tarball-path)
                               "--exclude=.clpm"
                               "--exclude=dist"
                               "--exclude=clpm.lock"
                               "-C" (namestring project-root)
                               ".")
                         :output :string
                         :error-output :string)
                      (declare (ignore _out))
                      (unless (zerop exit-code)
                        (usage-error "tar failed: ~A" err)))

                    (when tarball-out
                      (log-info "Wrote tarball: ~A" (namestring tarball-path)))

	                    (let* ((artifact-sha256
	                             (clpm.crypto.sha256:bytes-to-hex
	                              (clpm.crypto.sha256:sha256-file tarball-path)))
	                           (system-deps (%compute-system-deps-from-asd project-root systems))
                           (release-ref (format nil "~A@~A" name version))
                           (release-dir (merge-pathnames (format nil "registry/packages/~A/~A/" name version)
                                                         registry-root))
                           (release-path (merge-pathnames "release.sxp" release-dir))
                           (release-sig-path (merge-pathnames "release.sig" release-dir))
                           (snapshot-path (merge-pathnames "registry/snapshot.sxp" registry-root))
                           (snapshot-sig-path (merge-pathnames "registry/snapshot.sig" registry-root)))
                      (unless system-deps
                        (usage-error "Failed to compute system dependencies"))

                      ;; Write release metadata.
                      (ensure-directories-exist release-path)
                      (clpm.io.sexp:write-canonical-sexp-to-file
                       `(:release
                         :format 1
                         :name ,name
                         :version ,version
                         :source (:tarball :url ,tarball-url :sha256 ,artifact-sha256)
                         :artifact-sha256 ,artifact-sha256
                         :systems ,(sort (copy-list systems) #'string<)
                         :system-deps ,system-deps
                         ,@(when (clpm.project:project-license project)
                             (list :license (clpm.project:project-license project)))
                         ,@(when (clpm.project:project-homepage project)
                             (list :homepage (clpm.project:project-homepage project)))
                         ,@(when (clpm.project:project-description project)
                             (list :description (clpm.project:project-description project))))
                       release-path
                       :pretty t)
                      (write-sig-hex (read-file-bytes release-path) release-sig-path seed)

                      ;; Update snapshot.
                      (let* ((snap-form (clpm.io.sexp:read-registry-snapshot snapshot-path))
                             (plist (cdr snap-form))
                             (releases (or (getf plist :releases) '()))
                             (provides (or (getf plist :provides) '())))
                        (pushnew release-ref releases :test #'string=)
                        (setf releases (sort (remove-duplicates releases :test #'string=) #'string<))
                        (dolist (sys systems)
                          (pushnew (cons sys release-ref) provides
                                   :test (lambda (a b)
                                           (and (string= (car a) (car b))
                                                (string= (cdr a) (cdr b))))))
                        (setf provides
                              (sort (remove-duplicates provides
                                                       :test (lambda (a b)
                                                               (and (string= (car a) (car b))
                                                                    (string= (cdr a) (cdr b)))))
                                    (lambda (a b)
                                      (cond
                                        ((string< (car a) (car b)) t)
                                        ((string> (car a) (car b)) nil)
                                        (t (string< (cdr a) (cdr b)))))))
                        (clpm.io.sexp:write-canonical-sexp-to-file
                         `(:snapshot
                           :format 1
                           :generated-at ,(clpm.project:rfc3339-timestamp)
                           :releases ,releases
                           :provides ,provides)
                         snapshot-path
                         :pretty t)
                        (write-sig-hex (read-file-bytes snapshot-path) snapshot-sig-path seed))

                      (when git-commit-p
                        (let ((git (clpm.platform:find-git)))
                          (unless git
                            (error 'clpm.errors:clpm-missing-tool-error
                                   :tool "git"
                                   :install-hints (clpm.platform:tool-install-hints "git")))
                          (multiple-value-bind (_out err1 rc1)
                              (clpm.platform:run-program
                               (list git "add" "registry/snapshot.sxp" "registry/snapshot.sig"
                                     (format nil "registry/packages/~A/~A/release.sxp" name version)
                                     (format nil "registry/packages/~A/~A/release.sig" name version))
                               :directory registry-root
                               :output :string
                               :error-output :string)
                            (declare (ignore _out))
                            (unless (zerop rc1)
                              (usage-error "git add failed: ~A" err1)))
                          (multiple-value-bind (_out err2 rc2)
                              (clpm.platform:run-program
                               (list git "commit" "-m" (format nil "publish: ~A" release-ref))
                               :directory registry-root
                               :output :string
                               :error-output :string)
                            (declare (ignore _out))
                            (unless (zerop rc2)
                              (usage-error "git commit failed: ~A" err2)))))

                      (log-info "Published: ~A" release-ref)
                      (log-info "Release: ~A" (namestring release-path))
                      (log-info "Updated snapshot: ~A" (namestring snapshot-path))
                      0)))))))))))

;;; audit command

(defun cmd-audit (&rest args)
  "Print a provenance and trust report for the current lockfile."
  (let ((jsonp nil))
    (labels ((usage-error (fmt &rest fmt-args)
               (apply #'log-error fmt fmt-args)
               (log-error "Usage: clpm audit [--json]")
               (return-from cmd-audit 1))
             (dash (s) (if (and (stringp s) (plusp (length s))) s "-"))
             (starts-with-p (s prefix)
               (and (stringp s)
                    (stringp prefix)
                    (<= (length prefix) (length s))
                    (string= prefix (subseq s 0 (length prefix))))))
      (loop while args do
        (let ((arg (pop args)))
          (cond
            ((string= arg "--json") (setf jsonp t))
            (t (usage-error "Unknown option: ~A" arg)))))

      (multiple-value-bind (project-root manifest-path lock-path workspace-root _workspace-path)
          (find-effective-project-root)
        (declare (ignore project-root _workspace-path))
        (unless manifest-path
          (when (null workspace-root)
            (log-no-project-found))
          (return-from cmd-audit 1))
        (unless lock-path
          (log-error "No clpm.lock found (run: clpm resolve or clpm install)")
          (return-from cmd-audit 1))

        (let* ((project (clpm.project:read-project-file manifest-path))
               (lock (clpm.project:read-lock-file lock-path))
               (proj-name (or (clpm.project:project-name project)
                              (clpm.project:lockfile-project-name lock)))
               (proj-version (or (clpm.project:project-version project) "-"))
               (generated-at (clpm.project:lockfile-generated-at lock))
               (locked-registries
                 (sort (copy-list (or (clpm.project:lockfile-registries lock) '()))
                       (lambda (a b)
                         (string< (clpm.project:locked-registry-name a)
                                  (clpm.project:locked-registry-name b)))))
               (tarball-count 0)
               (git-count 0)
               (path-count 0)
	               (path-systems '())
	               (git-unpinned-systems '())
	               (quicklisp-no-trust '())
	               (quicklisp-tofu '())
	               (git-sig-missing '()))

	          (dolist (reg locked-registries)
	            (let ((kind (clpm.project:locked-registry-kind reg))
	                  (name (clpm.project:locked-registry-name reg))
	                  (trust (clpm.project:locked-registry-trust reg))
	                  (sig (clpm.project:locked-registry-signature reg)))
	              (when (and (eq kind :quicklisp) (null trust))
	                (push name quicklisp-no-trust))
	              (when (and (eq kind :quicklisp)
	                         (stringp trust)
	                         (string-equal trust "tofu"))
	                (push name quicklisp-tofu))
	              (when (and (eq kind :git) trust (null sig))
	                (push name git-sig-missing))))

          (dolist (locked (clpm.project:lockfile-resolved lock))
            (let* ((id (clpm.project:locked-system-id locked))
                   (release (clpm.project:locked-system-release locked))
                   (source (and release (clpm.project:locked-release-source release))))
              (when source
                (case (clpm.project:locked-source-kind source)
                  (:tarball (incf tarball-count))
                  (:git
                   (incf git-count)
                   (when (null (clpm.project:locked-source-commit source))
                     (push id git-unpinned-systems)))
                  (:path
                   (incf path-count)
                   (push id path-systems))))))

	          (setf path-systems (sort (remove-duplicates path-systems :test #'string=) #'string<)
	                git-unpinned-systems (sort (remove-duplicates git-unpinned-systems :test #'string=) #'string<)
	                quicklisp-no-trust (sort (remove-duplicates quicklisp-no-trust :test #'string=) #'string<)
	                quicklisp-tofu (sort (remove-duplicates quicklisp-tofu :test #'string=) #'string<)
	                git-sig-missing (sort (remove-duplicates git-sig-missing :test #'string=) #'string<))

          (let ((warnings '()))
            (when path-systems
              (push (format nil "path dependencies present: ~{~A~^, ~}" path-systems) warnings))
            (when git-unpinned-systems
              (push (format nil "git dependencies missing commit pin: ~{~A~^, ~}" git-unpinned-systems) warnings))
	            (when quicklisp-no-trust
	              (push (format nil "quicklisp trust not configured: ~{~A~^, ~}" quicklisp-no-trust) warnings))
	            (when quicklisp-tofu
	              (push (format nil "quicklisp trust is tofu (not pinned): ~{~A~^, ~}. Run: clpm registry update"
	                            quicklisp-tofu)
	                    warnings))
	            (when git-sig-missing
	              (push (format nil "git registry snapshot signature not recorded: ~{~A~^, ~}" git-sig-missing) warnings))
            (setf warnings (nreverse warnings))

            (if jsonp
                (let* ((registries-json
                         (mapcar
                          (lambda (reg)
                            (let* ((name (clpm.project:locked-registry-name reg))
                                   (kind (clpm.project:locked-registry-kind reg))
                                   (url (clpm.project:locked-registry-url reg))
                                   (trust (clpm.project:locked-registry-trust reg))
                                   (commit (clpm.project:locked-registry-commit reg))
                                   (sig (clpm.project:locked-registry-signature reg))
                                   (verified (and (eq kind :git) trust sig)))
                              (list :object
                                    (list (cons "name" name)
                                          (cons "kind" (string-downcase (symbol-name kind)))
                                          (cons "url" (or url ""))
                                          (cons "trust" (or trust ""))
                                          (cons "commit" (or commit ""))
                                          (cons "snapshotSigSha256" (or sig ""))
                                          (cons "verified" (if verified t :false))))))
                          locked-registries))
                       (quicklisp-pins
                         (let ((pins '()))
                           (dolist (reg locked-registries)
                             (when (eq (clpm.project:locked-registry-kind reg) :quicklisp)
                               (let* ((name (clpm.project:locked-registry-name reg))
                                      (trust (clpm.project:locked-registry-trust reg))
                                      (distinfo-pin (and (stringp trust)
                                                         (starts-with-p (string-downcase trust) "sha256:")
                                                         trust))
                                      (systems (clpm.project:locked-registry-quicklisp-systems-sha256 reg))
                                      (releases (clpm.project:locked-registry-quicklisp-releases-sha256 reg)))
                                 (push (list :object
                                             (list (cons "name" name)
                                                   (cons "distinfoPin" (or distinfo-pin ""))
                                                   (cons "systemsSha256" (or systems ""))
                                                   (cons "releasesSha256" (or releases ""))))
                                       pins))))
                           (nreverse pins))))
                  (clpm.io.json:write-json
                   (list :object
                         (list (cons "project"
                                     (list :object
                                           (list (cons "name" (or proj-name ""))
                                                 (cons "version" (or proj-version "")))))
                               (cons "lockfile"
                                     (list :object
                                           (list (cons "generatedAt" (or generated-at "")))))
                               (cons "registries" (list :array registries-json))
                               (cons "quicklisp"
                                     (list :object
                                           (list (cons "distinfoPins" (list :array quicklisp-pins)))))
                               (cons "sources"
                                     (list :object
                                           (list (cons "tarball" tarball-count)
                                                 (cons "git" git-count)
                                                 (cons "path" path-count))))
                               (cons "warnings" (list :array warnings))))
                   *standard-output*)
                  (terpri)
                  0)
                (progn
                  (format t "Project: ~A ~A~%" (dash proj-name) (dash proj-version))
                  (format t "Lockfile: generated-at ~A~%" (dash generated-at))
                  (format t "Registries:~%")
                  (dolist (reg locked-registries)
                    (let* ((name (clpm.project:locked-registry-name reg))
                           (kind (clpm.project:locked-registry-kind reg))
                           (url (clpm.project:locked-registry-url reg))
                           (trust (clpm.project:locked-registry-trust reg))
                           (commit (clpm.project:locked-registry-commit reg))
                           (sig (clpm.project:locked-registry-signature reg))
                           (verified
                             (cond
                               ((not (eq kind :git)) "-")
                               ((and trust sig) "yes")
                               (trust "no")
                               (t "-"))))
                      (format t "  ~A~C~A~C~A~Ctrust: ~A~Ccommit: ~A~Csnapshot-sig: ~A~Cverified: ~A~%"
                              (dash name) #\Tab
                              (string-downcase (symbol-name kind)) #\Tab
                              (dash url) #\Tab
                              (dash trust) #\Tab
                              (dash commit) #\Tab
                              (dash sig) #\Tab
                              verified)))

                  (let ((pins '()))
                    (dolist (reg locked-registries)
                      (when (eq (clpm.project:locked-registry-kind reg) :quicklisp)
                        (let* ((name (clpm.project:locked-registry-name reg))
                               (trust (clpm.project:locked-registry-trust reg))
                               (distinfo (cond
                                          ((and (stringp trust)
                                                (starts-with-p (string-downcase trust) "sha256:"))
                                           trust)
                                          ((and (stringp trust) (plusp (length trust))) trust)
                                          (t "-")))
                               (systems (or (clpm.project:locked-registry-quicklisp-systems-sha256 reg) "-"))
                               (releases (or (clpm.project:locked-registry-quicklisp-releases-sha256 reg) "-")))
                          (push (list name distinfo systems releases) pins))))
                    (setf pins (sort pins #'string< :key #'car))
                    (when pins
                      (format t "Quicklisp snapshot pins:~%")
                      (dolist (p pins)
                        (format t "  ~A~Cdistinfo: ~A~Csystems: ~A~Creleases: ~A~%"
                                (first p) #\Tab
                                (second p) #\Tab
                                (third p) #\Tab
                                (fourth p)))))))

                  (format t "Sources:~%")
                  (format t "  tarball: ~D~%" tarball-count)
                  (format t "  git: ~D~%" git-count)
                  (format t "  path: ~D~%" path-count)
                  (when warnings
                    (format t "Warnings:~%")
                    (dolist (w warnings)
                      (format t "  - ~A~%" w)))
                  0))))))

;;; sbom command

(defun cmd-sbom (&rest args)
  "Generate a software bill of materials (SBOM) from the current lockfile."
  (let ((format nil)
        (output nil))
    (labels ((usage-error (fmt &rest fmt-args)
               (apply #'log-error fmt fmt-args)
               (log-error "Usage: clpm sbom --format cyclonedx-json [--output <path>]")
               (return-from cmd-sbom 1))
             (nonempty-string (s)
               (and (stringp s) (plusp (length s))))
             (safe-license->json (license)
               ;; Minimal CycloneDX-compatible license entry.
               (list :object
                     (list (cons "license"
                                 (list :object
                                       (list (cons "id" license))))))))
      ;; Parse args.
      (loop while args do
        (let ((arg (pop args)))
          (cond
            ((or (string= arg "--output") (string= arg "--out"))
             (setf output (pop args))
             (unless (nonempty-string output)
               (usage-error "Missing value for ~A" arg)))
            ((string= arg "--format")
             (setf format (pop args))
             (unless (nonempty-string format)
               (usage-error "Missing value for --format")))
            (t
             (usage-error "Unknown option: ~A" arg)))))

      (unless (and (stringp format) (string= format "cyclonedx-json"))
        (usage-error "Unsupported --format (supported: cyclonedx-json): ~S" format))

      (multiple-value-bind (project-root manifest-path lock-path workspace-root _workspace-path)
          (find-effective-project-root)
        (declare (ignore project-root _workspace-path))
        (unless manifest-path
          (when (null workspace-root)
            (log-no-project-found))
          (return-from cmd-sbom 1))
        (unless lock-path
          (log-error "No clpm.lock found (run: clpm resolve or clpm install)")
          (return-from cmd-sbom 1))

        (let* ((lock (clpm.project:read-lock-file lock-path))
               (locked-registries
                 (sort (copy-list (or (clpm.project:lockfile-registries lock) '()))
                       (lambda (a b)
                         (string< (clpm.project:locked-registry-name a)
                                  (clpm.project:locked-registry-name b)))))
               (registries '()))

          ;; Load registries (best-effort). In offline mode, only load registries
          ;; that are already present locally.
          (dolist (lr locked-registries)
            (let* ((name (clpm.project:locked-registry-name lr))
                   (kind (clpm.project:locked-registry-kind lr))
                   (url (clpm.project:locked-registry-url lr))
                   (trust (clpm.project:locked-registry-trust lr))
                   (ql-systems (clpm.project:locked-registry-quicklisp-systems-sha256 lr))
                   (ql-releases (clpm.project:locked-registry-quicklisp-releases-sha256 lr))
                   (local (clpm.registry:registry-local-path name)))
              (when (and (stringp name) (plusp (length name)))
                (let ((loadp t))
                  (when *offline*
                    (setf loadp
                          (case kind
                            (:git
                             (uiop:directory-exists-p local))
                            (:quicklisp
                             (and (uiop:file-exists-p (merge-pathnames "distinfo.txt" local))
                                  (uiop:file-exists-p (merge-pathnames "systems.txt" local))
                                  (uiop:file-exists-p (merge-pathnames "releases.txt" local))))
                            (t (uiop:directory-exists-p local)))))
                  (when loadp
                    (handler-case
                        (push (clpm.registry:clone-registry name url
                                                            :trust-key trust
                                                            :quicklisp-systems-sha256 ql-systems
                                                            :quicklisp-releases-sha256 ql-releases
                                                            :kind kind)
                              registries)
                      (error (c)
                        (declare (ignore c))
                        nil)))))))
          (setf registries (nreverse registries))

          ;; Build unique components keyed by (name . version).
          (let ((components-ht (make-hash-table :test 'equal)))
            (dolist (locked (clpm.project:lockfile-resolved lock))
              (let* ((release (clpm.project:locked-system-release locked))
                     (name (and release (clpm.project:locked-release-name release)))
                     (version (and release (clpm.project:locked-release-version release)))
                     (src (and release (clpm.project:locked-release-source release)))
                     (sha256 (and release (clpm.project:locked-release-artifact-sha256 release)))
                     (sha1 (and src (clpm.project:locked-source-sha1 src))))
                (when (and (stringp name) (plusp (length name))
                           (stringp version) (plusp (length version)))
                  (setf (gethash (cons name version) components-ht)
                        (list :name name
                              :version version
                              :sha256 sha256
                              :sha1 sha1)))))

            (let ((components
                    (sort (loop for k being the hash-keys of components-ht collect k)
                          (lambda (a b)
                            (cond
                              ((string< (car a) (car b)) t)
                              ((string> (car a) (car b)) nil)
                              (t (string< (cdr a) (cdr b))))))))
              (labels ((find-license (pkg ver)
                         (block found
                           (dolist (reg registries)
                             (let ((meta (ignore-errors (clpm.registry:get-release-metadata reg pkg ver))))
                               (when meta
                                 (let ((license (clpm.registry:release-metadata-license meta)))
                                   (when (and (stringp license) (plusp (length license)))
                                     (return-from found license))))))
                           nil))
                       (component->json (pkg ver)
                         (let* ((info (gethash (cons pkg ver) components-ht))
                                (sha256 (getf info :sha256))
                                (sha1 (getf info :sha1))
                                (license (find-license pkg ver))
                                (hashes '()))
                           (when (and (stringp sha256) (plusp (length sha256)))
                             (push (list :object
                                         (list (cons "alg" "SHA-256")
                                               (cons "content" sha256)))
                                   hashes))
                           (when (and (stringp sha1) (plusp (length sha1)))
                             (push (list :object
                                         (list (cons "alg" "SHA-1")
                                               (cons "content" sha1)))
                                   hashes))
                           (setf hashes (nreverse hashes))
                           (let ((entries
                                   (list (cons "name" pkg)
                                         (cons "version" ver)
                                         (cons "purl" (format nil "pkg:cl/~A@~A" pkg ver)))))
                             (when hashes
                               (setf entries (append entries
                                                     (list (cons "hashes" (list :array hashes))))))
                             (when (and (stringp license) (plusp (length license)))
                               (setf entries
                                     (append entries
                                             (list (cons "licenses"
                                                         (list :array (list (safe-license->json license))))))))
                             (list :object entries)))))

                (let* ((components-json (mapcar (lambda (k)
                                                  (component->json (car k) (cdr k)))
                                                components))
                       (bom
                         (list :object
                               (list (cons "bomFormat" "CycloneDX")
                                     (cons "specVersion" "1.5")
                                     (cons "version" 1)
                                     (cons "metadata"
                                           (list :object
                                                 (list (cons "tools"
                                                             (list :array
                                                                   (list (list :object
                                                                               (list (cons "name" "clpm")
                                                                                     (cons "version" "0.1.0")))))))))
                                     (cons "components" (list :array components-json))))))
                  (labels ((write-to-stream (stream)
                             (clpm.io.json:write-json bom stream)
                             (terpri stream)))
                    (cond
                      ((and output (nonempty-string output))
                       (let ((out-path (uiop:ensure-pathname (clpm.platform:expand-path output)
                                                            :defaults (uiop:getcwd)
                                                            :want-existing nil)))
                         (ensure-directories-exist out-path)
                         (with-open-file (s out-path :direction :output
                                                   :if-exists :supersede
                                                   :external-format :utf-8)
                           (write-to-stream s))
                         0))
                      (t
                       (write-to-stream *standard-output*)
                       0))))))))))))

;;; help command

(defun print-command-help (command &key subcommand)
  "Print help for a COMMAND (keyword) and optional SUBCOMMAND (string)."
  (labels ((p (fmt &rest args)
             (apply #'format t (concatenate 'string fmt "~%") args)))
    (case command
      (:help
       (p "Usage: clpm help <command> [subcommand]")
       (p "")
       (p "Examples:")
       (p "  clpm help new")
       (p "  clpm help registry add")
       0)
      (:doctor
       (p "Usage: clpm doctor")
       (p "")
       (p "Checks:")
       (p "  - SBCL version >= 2.0.0")
       (p "  - ASDF version >= 3.3.0")
       (p "  - Downloader present (curl/wget/powershell)")
       (p "  - tar present")
       (p "  - Registries configured (global config and/or current project)")
       0)
      (:new
       (p "Usage: clpm new <name> --bin|--lib [--dir <path>]")
       (p "")
       (p "Options:")
       (p "  --bin         Create an executable project scaffold")
       (p "  --lib         Create a library project scaffold")
       (p "  --dir <path>  Destination directory (default: current dir)")
       0)
      (:init
       (p "Usage: clpm init [name]")
       (p "")
       (p "Creates clpm.project in the current directory.")
       0)
      (:add
       (p "Usage: clpm add [--dev|--test] [--any|--caret] [--registry <name>] [--path <dir> | --git <url> --ref <ref>] <system>[@^<semver>|@=<exact>]")
       (p "")
       (p "Examples:")
       (p "  clpm add alexandria")
       (p "  clpm add --caret alexandria")
       (p "  clpm add alexandria@^1.4.0")
       (p "  clpm add --path ../my-lib my-lib")
       (p "  clpm add --git https://example.invalid/repo.git --ref main my-lib")
       (p "")
       (p "Options:")
       (p "  --dev         Add to :dev-depends")
       (p "  --test        Add to :test-depends")
       (p "  --any         Explicitly set :constraint nil (any version)")
       (p "  --caret       Set caret constraint based on highest available version")
       (p "  --registry    Select registry when multiple provide the system")
       (p "  --install     Run 'clpm install' after updating manifests")
       (p "  --path <dir>  Use a local path dependency")
       (p "  --git <url>   Use a git dependency")
       (p "  --ref <ref>   Git ref (branch/tag/commit) to resolve")
       0)
      (:remove
       (p "Usage: clpm remove [--dev|--test] <dep>")
       (p "")
       (p "Options:")
       (p "  --dev   Remove from :dev-depends")
       (p "  --test  Remove from :test-depends")
       0)
      (:search
       (p "Usage: clpm search <query> [--limit N] [--json]")
       (p "")
       (p "Search registries for systems matching <query>.")
       (p "")
       (p "Options:")
       (p "  --limit N  Limit number of results (after sorting)")
       (p "  --json     Emit a stable JSON array")
       0)
      (:info
       (p "Usage: clpm info <system> [--json] [--all]")
       (p "")
       (p "Show details about a system and available releases.")
       (p "")
       (p "Options:")
       (p "  --json   Emit a stable JSON object")
       (p "  --all    Include metadata for all candidates")
       0)
      (:tree
       (p "Usage: clpm tree [--package <member>] [--depth N]")
       (p "")
       (p "Print the resolved dependency tree for the current project/workspace.")
       (p "")
       (p "Options:")
       (p "  --package <member>  Workspace member to target (same as global -p/--package)")
       (p "  --depth N           Max depth (0 = roots only)")
       0)
      (:why
       (p "Usage: clpm why <system> [--package <member>]")
       (p "")
       (p "Explain why <system> appears in the resolved dependency graph.")
       (p "")
       (p "Options:")
       (p "  --package <member>  Workspace member to target (same as global -p/--package)")
       0)
      (:scripts
       (p "Usage:")
       (p "  clpm scripts list")
       (p "  clpm scripts run <name> [-- <args...>]")
       (p "")
       (p "List and run project scripts defined in clpm.project.")
       (p "")
       (p "Script forms in clpm.project:")
       (p "  (:script :name \"fmt\" :type :shell :command (\"sh\" \"-c\" \"...\"))")
       (p "  (:script :name \"task\" :type :lisp :system \"my-app\" :function \"my-app::main\")")
       0)
      (:audit
       (p "Usage: clpm audit [--json]")
       (p "")
       (p "Show a provenance and trust report for the current lockfile.")
       0)
      (:sbom
       (p "Usage: clpm sbom --format <cyclonedx-json> [--out <path>]")
       (p "")
       (p "Generate a software bill of materials (SBOM) from the lockfile.")
       0)
      (:keys
       (p "Usage: clpm keys generate --out <dir> --id <id>")
       (p "")
       (p "Manage Ed25519 keys for signing registries and releases.")
       (p "")
       (p "Generated files:")
       (p "  <id>.key  32-byte seed as ASCII hex (64 chars) (keep secret)")
       (p "  <id>.pub  32-byte public key as ASCII hex (64 chars)")
       0)
      (:publish
       (p "Usage: clpm publish --registry <dir> --key-id <id> --keys-dir <dir> --tarball-url <url> [--tarball-out <path>] [--project <dir>] [--git-commit]")
       (p "")
       (p "Publish the current project to a git-backed registry.")
       (p "")
       (p "Notes:")
       (p "  - This writes files into the registry directory; it does not push.")
       (p "  - Use --tarball-out to write the tarball to a file/dir (optional).")
       (p "  - Use --git-commit to commit the changes inside the registry (optional).")
       0)
      (:workspace
       (p "Usage:")
       (p "  clpm workspace init [--dir <path>]")
       (p "  clpm workspace add <member> [--dir <path>]")
       (p "  clpm workspace list [--dir <path>]")
       (p "")
       (p "Manage workspaces and workspace members.")
       0)
      (:registry
       (p "Usage: clpm registry <add|list|update|trust|init> [options]")
       (p "")
       (let ((sub (and (stringp subcommand) (string-downcase subcommand))))
		         (cond
		           ((and sub (string= sub "add"))
	            (p "Usage: clpm registry add --name <name> --url <git-url> --trust <ed25519:key-id>")
	            (p "   or: clpm registry add --quicklisp [--name quicklisp] [--url <dist-url>]")
	            (p "")
	            (p "Example:")
	            (p "  clpm registry add --name main --url https://example.invalid/registry.git --trust ed25519:abcd...")
	            (p "  clpm registry add --quicklisp")
	            0)
           ((and sub (string= sub "list"))
            (p "Usage: clpm registry list")
            0)
           ((and sub (string= sub "update"))
            (p "Usage: clpm registry update [--refresh-trust] [name ...]")
            0)
	           ((and sub (string= sub "trust"))
	            (p "Usage: clpm registry trust <list|set|refresh> [args]")
	            (p "")
	            (p "Subcommands:")
	            (p "  list                 List registries and trust settings")
	            (p "  set <name> <trust>   Set trust string (use 'none' to clear)")
	            (p "  refresh <name>       Refresh pinned trust (Quicklisp only)")
	            0)
	           ((and sub (string= sub "init"))
	            (p "Usage: clpm registry init --dir <path> --key-id <id> --keys-dir <dir>")
	            0)
	           (t
	            (p "Subcommands:")
	            (p "  add      Add or update a configured registry")
	            (p "  list     List configured registries")
	            (p "  update   Update cloned registries (optionally by name)")
	            (p "  trust    Manage registry trust settings")
	            (p "  init     Initialize a new git registry directory")
	            0))))
      (:resolve
       (p "Usage: clpm resolve")
       (p "")
       (p "Resolves dependencies and writes clpm.lock deterministically.")
       0)
      (:fetch
       (p "Usage: clpm fetch [--offline]")
       (p "")
       (p "Downloads dependencies specified in clpm.lock.")
       0)
      (:build
       (p "Usage: clpm build")
       (p "")
       (p "Builds dependencies specified in clpm.lock into the store.")
       0)
      (:install
       (p "Usage: clpm install")
       (p "")
       (p "Runs resolve + fetch + build and activates the project.")
       0)
      (:update
       (p "Usage: clpm update [system ...]")
       (p "")
       (p "Updates dependency selections and rewrites clpm.lock.")
       0)
      (:repl
       (p "Usage: clpm repl [system]")
       (p "")
       (p "Starts SBCL with the project environment loaded.")
       0)
      (:run
       (p "Usage: clpm run [-- <args...>]")
       (p "")
       (p "Runs the entrypoint configured in clpm.project :run.")
       0)
      (:exec
       (p "Usage: clpm exec -- <cmd...>")
       (p "")
       (p "Runs a command in the project environment (after activation).")
       0)
      (:test
       (p "Usage: clpm test")
       (p "")
       (p "Builds test dependencies and runs configured ASDF test systems.")
       0)
      (:package
       (p "Usage: clpm package")
       (p "")
       (p "Builds a distributable executable in dist/ based on clpm.project :package.")
       0)
      (:clean
       (p "Usage: clpm clean [--dist]")
       (p "")
       (p "Removes project-local outputs. Use --dist to remove dist/ as well.")
       0)
      (:gc
       (p "Usage: clpm gc [--dry-run]")
       (p "")
       (p "Garbage collects unreferenced store entries.")
       0)
      (t
       (log-error "Unknown command: ~A" command)
       1))))

(defun cmd-help (&rest args)
  "Print command-specific help."
  (let* ((cmd-name (first args))
         (subcmd (second args)))
    (unless (and (stringp cmd-name) (plusp (length cmd-name)))
      (log-error "Usage: clpm help <command> [subcommand]")
      (return-from cmd-help 1))
    (let ((command (intern (string-upcase cmd-name) :keyword)))
      (print-command-help command :subcommand subcmd))))

;;; doctor command

(defun cmd-doctor ()
  "Check the local environment for running CLPM."
  (let ((failures 0))
    (labels ((ok (fmt &rest args)
               (apply #'format t (concatenate 'string "ok: " fmt "~%") args))
             (note-warn (fmt &rest args)
               (apply #'format t (concatenate 'string "warn: " fmt "~%") args))
             (bad (fmt &rest args)
               (incf failures)
               (apply #'format t (concatenate 'string "error: " fmt "~%") args)))

      ;; SBCL version
      (let* ((min-sbcl "2.0.0")
             (sbcl (clpm.platform:sbcl-version)))
        (if (clpm.solver.version:version>= sbcl min-sbcl)
            (ok "sbcl ~A (>= ~A)" sbcl min-sbcl)
            (bad "sbcl ~A is too old (need >= ~A)" sbcl min-sbcl)))

      ;; ASDF version
      (let* ((min-asdf "3.3.0")
             (asdf (handler-case
                       (clpm.platform:asdf-version)
                     (error (c)
                       (declare (ignore c))
                       nil))))
        (cond
          ((null asdf)
           (bad "asdf not available (need >= ~A)" min-asdf))
          ((clpm.solver.version:version>= asdf min-asdf)
           (ok "asdf ~A (>= ~A)" asdf min-asdf))
          (t
           (bad "asdf ~A is too old (need >= ~A)" asdf min-asdf))))

      ;; Downloader
      (let ((downloader (handler-case
                            (clpm.platform:find-downloader)
                          (error (c)
                            (declare (ignore c))
                            nil))))
        (if downloader
            (ok "downloader: ~A" downloader)
            (bad "no downloader found (need curl or wget)")))

      ;; tar
      (let ((tar (handler-case
                     (clpm.platform:find-tar)
                   (error (c)
                     (declare (ignore c))
                     nil))))
        (if tar
            (ok "tar: ~A" tar)
            (bad "tar not found")))

      ;; Registries configured: global config and/or current project.
      (let ((registries nil)
            (needs-registry-p nil))
        (handler-case
            (multiple-value-bind (project-root manifest-path lock-path)
                (clpm.project:find-project-root)
              (declare (ignore project-root lock-path))
              (labels ((dep-needs-registry-p (dep)
                         (let ((c (clpm.project:dependency-constraint dep)))
                           (cond
                             ((null c) t)
                             ((and (consp c) (member (car c) '(:path :git) :test #'eq))
                              nil)
                             (t t))))
                       (project-needs-registry-p (project)
                         (or (some #'dep-needs-registry-p
                                   (or (clpm.project:project-depends project) '()))
                             (some #'dep-needs-registry-p
                                   (or (clpm.project:project-dev-depends project) '()))
                             (some #'dep-needs-registry-p
                                   (or (clpm.project:project-test-depends project) '())))))
                (if manifest-path
                    (let ((project (clpm.project:read-project-file manifest-path)))
                      (setf needs-registry-p (project-needs-registry-p project))
                      (multiple-value-bind (merged-registries build-options)
                          (clpm.config:merge-project-config project)
                        (declare (ignore build-options))
                        (setf registries merged-registries)))
                    (setf registries (clpm.config:config-registries (clpm.config:read-config))))))
          (error (c)
            (note-warn "Failed to read config/project registries: ~A" c)
            (setf registries nil
                  needs-registry-p nil)))
        (cond
          ((and (listp registries) (plusp (length registries)))
           (ok "registries: ~D configured" (length registries)))
          (needs-registry-p
           (bad "no registries configured (run: clpm registry add ...)"))
          (t
           (note-warn "no registries configured"))))

      (if (zerop failures)
          (progn
            (format t "~&doctor: OK~%")
            0)
          (progn
            (format t "~&doctor: FAILED (~D issue~:P)~%" failures)
            1)))))
