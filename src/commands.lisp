;;;; commands.lisp - CLI command implementations

(in-package #:clpm.commands)

;;; Global options (set from main)
;;; `*verbose*` and the log-* helpers live in clpm.log so lower layers can
;;; use them without depending on clpm.commands; clpm.commands re-exports
;;; them for backwards-compatible callers (e.g. main.lisp).

(defvar *offline* nil "Offline mode - fail if artifacts missing")
(defvar *insecure* nil "Skip signature verification")
(defvar *jobs* 1 "Number of parallel jobs")
(defvar *lisp* nil "Selected Lisp implementation kind (:sbcl/:ccl/:ecl), from --lisp.")
(defvar *target-package* nil "Workspace member to target (from -p/--package).")
(defvar *with-optional* nil
  "Optional-dep opt-in set from --with-optional flags. NIL = no flags passed
this invocation; :all = include every :optional dep; (s1 s2 ...) = include
only these systems. Merged with any persisted opt-in already in the lockfile
when computing the effective set.")

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
      (if *target-package*
          (find-effective-project-root)
          (clpm.project:find-project-root))
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
             (log-error "  clpm project new <name> --workspace [--dir <path>]")
             (log-error "  clpm project new <name> --bin|--lib [--dir <path>]")
             (log-error "  clpm project new <name> --bin|--lib --member-of <workspace-dir>")
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
                        (gitignore-path (merge-pathnames ".gitignore" project-root))
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

                   ;; .gitignore
                   (write-text gitignore-path ".DS_Store
.clpm/
*.fasl
*.fasl-tmp
")

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
                (format s "  clpm project new app --bin --member-of .~%~%")
                (format s "Target a member:~%~%")
                (format s "  clpm -p app deps sync~%")
                (format s "  clpm -p app run test~%")))
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

(defun cmd-project (&rest args)
  "Dispatch project-resource operations."
  (let ((sub (first args))
        (rest (rest args)))
    (labels ((usage ()
               (log-error "Usage:")
               (log-error "  clpm project new <name> --workspace [--dir <path>]")
               (log-error "  clpm project new <name> --bin|--lib [--dir <path>]")
               (log-error "  clpm project new <name> --bin|--lib --member-of <workspace-dir>")
               (log-error "  clpm project init [name]")
               (log-error "  clpm project workspace <init|add|remove|list> ...")
               (log-error "  clpm project package")
               1))
      (cond
        ((or (null sub) (string= sub "help") (string= sub "--help"))
         (usage))
        ((string= sub "new")
         (apply #'cmd-new rest))
        ((string= sub "init")
         (when (rest rest)
           (log-error "Usage: clpm project init [name]")
           (return-from cmd-project 1))
         (cmd-init :name (first rest)))
        ((string= sub "workspace")
         (apply #'cmd-workspace rest))
        ((string= sub "package")
         (when rest
           (log-error "Usage: clpm project package")
           (return-from cmd-project 1))
         (apply #'cmd-package rest))
        (t
         (log-error "Unknown project subcommand: ~A" sub)
         (usage))))))

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
       (when *target-package*
         (log-error "-p/--package selects a workspace member and is only valid from a workspace root.")
         (return-from find-effective-project-root
           (values nil nil nil nil nil)))
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
  "Add dependencies to clpm.project and refresh clpm.lock."
  (multiple-value-bind (project-root manifest-path lock-path workspace-root _workspace-path)
      (find-effective-project-root)
    (declare (ignore lock-path _workspace-path))
    (unless manifest-path
      (when (null workspace-root)
        (log-no-project-found))
      (return-from cmd-add 1))

    (let ((specs '())
          (dev-p nil)
          (test-p nil)
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
              (t
               (push arg specs))))
          (incf i)))

      (setf specs (nreverse specs))

      (unless specs
        (log-error "Usage: clpm deps add [--dev|--test] [--any|--caret] [--registry <name>] [--path <dir> | --git <url> --ref <ref>] <system>[@^<semver>|@=<exact>]...")
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

      (when (and (or path git-url) (rest specs))
        (log-error "--path/--git may only be used with one dependency")
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
             (registry-index nil))
        (labels ((registry-index ()
                   (or registry-index
                       (setf registry-index
                             (clpm.registry:build-registry-index registries))))
                 (provider-names (system-id)
                   (sort (remove-duplicates
                          (mapcar (lambda (entry)
                                    (clpm.registry:registry-name (car entry)))
                                  (or (clpm.registry:index-lookup-system
                                       (registry-index)
                                       system-id)
                                      '()))
                          :test #'string=)
                         #'string<))
                 (highest-version (system-id)
                   (let ((best nil))
                     (dolist (entry (clpm.registry:index-lookup-system
                                     (registry-index)
                                     system-id))
                       (when (or (null registry-name)
                                 (string= registry-name
                                          (clpm.registry:registry-name (car entry))))
                         (let ((release-ref (cdr entry)))
                           (when (stringp release-ref)
                             (let ((at (position #\@ release-ref)))
                               (when at
                                 (let ((version (subseq release-ref (1+ at))))
                                   (when (or (null best)
                                             (clpm.solver.version:version> version best))
                                     (setf best version)))))))))
                     best))
                 (add-spec (spec)
                   (multiple-value-bind (system-id parsed-constraint)
                       (parse-dep-spec spec)
                     (unless (plusp (length system-id))
                       (log-error "Invalid dependency spec: ~A" spec)
                       (return-from cmd-add 1))
                     (when (eq parsed-constraint :invalid)
                       (log-error "Invalid dependency spec: ~A" spec)
                       (return-from cmd-add 1))
                     (when (and parsed-constraint (or path git-url))
                       (log-error "Do not combine @<constraint> with --path/--git")
                       (return-from cmd-add 1))
                     (when (and parsed-constraint (or any-p caret-p))
                       (log-error "Do not combine @<constraint> with --any/--caret")
                       (return-from cmd-add 1))
                     (let ((constraint-form nil)
                           (dep-source nil))
                       ;; Registry disambiguation for non-pinned sources.
                       (when (and (null path) (null git-url))
                         (let ((providers (provider-names system-id)))
                           (when (null providers)
                             (log-error "System not found in configured registries: ~A" system-id)
                             (return-from cmd-add 1))
                           (when (and (null registry-name) (> (length providers) 1))
                             (log-error "System ~A is provided by multiple registries; use --registry <name>:" system-id)
                             (dolist (name providers)
                               (log-error "  ~A" name))
                             (return-from cmd-add 1))
                           (when registry-name
                             (unless (member registry-name providers :test #'string=)
                               (log-error "Registry ~A does not provide ~A. Providers:" registry-name system-id)
                               (dolist (name providers)
                                 (log-error "  ~A" name))
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
                          (let ((v-max (highest-version system-id)))
                            (unless v-max
                              (log-error "No versions found for ~A in configured registries" system-id)
                              (return-from cmd-add 1))
                            (setf constraint-form (list :semver (format nil "^~A" v-max)))))
                         (t
                          (setf constraint-form nil)))
                       (list :system-id system-id
                             :constraint constraint-form
                             :source dep-source))))
                 (deps-slot ()
                   (ecase section
                     (:depends (clpm.project:project-depends project))
                     (:dev-depends (clpm.project:project-dev-depends project))
                     (:test-depends (clpm.project:project-test-depends project))))
                 (set-deps-slot (new)
                   (ecase section
                     (:depends (setf (clpm.project:project-depends project) new))
                     (:dev-depends (setf (clpm.project:project-dev-depends project) new))
                     (:test-depends (setf (clpm.project:project-test-depends project) new)))))
          (let ((updates '())
                (seen-systems '()))
            (dolist (spec specs)
              (let* ((update (add-spec spec))
                     (system-id (getf update :system-id)))
                (when (member system-id seen-systems :test #'string=)
                  (log-error "Duplicate dependency spec: ~A" system-id)
                  (return-from cmd-add 1))
                (push system-id seen-systems)
                (push update updates)))
            (setf updates (nreverse updates))
            (let ((deps (deps-slot)))
              (dolist (update updates)
                (let* ((system-id (getf update :system-id))
                       (constraint-form (getf update :constraint))
                       (dep-source (getf update :source))
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
                    (setf (clpm.project:dependency-source existing) dep-source))))
              (set-deps-slot (sorted-deps deps))
              (clpm.project:write-project-file project manifest-path))

            (log-info "Added ~{~A~^, ~} to ~A"
                      (mapcar (lambda (update)
                                (getf update :system-id))
                              updates)
                      (ecase section
                        (:depends "depends")
                        (:dev-depends "dev-depends")
                        (:test-depends "test-depends")))))

        (uiop:with-current-directory (project-root)
          (let ((*target-package* nil))
            (cmd-resolve)))))))

(defun cmd-remove (&rest args)
  "Remove a dependency from clpm.project and refresh clpm.lock."
  (multiple-value-bind (project-root manifest-path lock-path workspace-root _workspace-path)
      (find-effective-project-root)
    (declare (ignore lock-path _workspace-path))
    (unless manifest-path
      (when (null workspace-root)
        (log-no-project-found))
      (return-from cmd-remove 1))

    (let ((system-id nil)
          (dev-p nil)
          (test-p nil))
      (dolist (arg args)
        (cond
          ((string= arg "--dev") (setf dev-p t))
          ((string= arg "--test") (setf test-p t))
          ((and (plusp (length arg)) (char= (char arg 0) #\-))
           (log-error "Unknown option: ~A" arg)
           (return-from cmd-remove 1))
          ((null system-id) (setf system-id arg))
          (t
           (log-error "Unexpected argument: ~A" arg)
           (return-from cmd-remove 1))))
      (unless system-id
        (log-error "Usage: clpm deps remove <system> [--dev|--test]")
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
          (let ((*target-package* nil))
            (cmd-resolve)))))))

;;; search command

(defun cmd-search (&rest args)
  "Search configured registries for systems matching a query string."
  (labels ((usage-error (fmt &rest fmt-args)
             (apply #'log-error fmt fmt-args)
             (log-error "Usage: clpm deps search <query> [--limit N] [--json]")
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
             (log-error "Usage: clpm deps info <system> [--json] [--all]")
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
                    (cond
                      (allp
                       (let* ((reg (getf c :registry))
                              (pkg (getf c :package))
                              (ver (getf c :version))
                              (meta (clpm.registry:get-release-metadata reg pkg ver))
                              (fields (and meta (source->fields
                                                 (clpm.registry:release-metadata-source meta))))
                              (license (and meta
                                            (clpm.registry:release-metadata-license meta))))
                         (format t "  ~A~C~A~%"
                                 (getf c :registry-name) #\Tab (getf c :release))
                         (when fields
                           (destructuring-bind (kind url hash commit) fields
                             (format t "      source~C~A~C~A"
                                     #\Tab (string-downcase (symbol-name kind))
                                     #\Tab (or url ""))
                             (cond
                               ((and hash (plusp (length hash)))
                                (format t "~Chash:~A~%" #\Tab hash))
                               ((and commit (plusp (length commit)))
                                (format t "~Ccommit:~A~%" #\Tab commit))
                               (t (terpri)))))
                         (when (and license (plusp (length license)))
                           (format t "      license~C~A~%" #\Tab license))))
                      (t
                       (format t "  ~A~C~A~%"
                               (getf c :registry-name) #\Tab (getf c :release)))))
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

(defun %project-optional-system-ids (project)
  "Return the set of system-ids declared as :optional in PROJECT."
  (loop for dep in (append (clpm.project:project-depends project)
                           (clpm.project:project-dev-depends project)
                           (clpm.project:project-test-depends project))
        when (clpm.project:dependency-optional-p dep)
          collect (clpm.project:dependency-system dep)))

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

(defun %print-dependency-tree (roots graph &key depth-limit optional-systems)
  "Print a deterministic dependency tree rooted at ROOTS.

GRAPH is a hash table mapping system-id -> sorted list of dependency system-ids.
OPTIONAL-SYSTEMS is a list of system-ids declared `:optional t` in the project
manifest; root-level entries in that set are tagged \"(optional)\"."
  (let ((expanded (make-hash-table :test 'equal))
        (optional-tbl (and optional-systems
                           (let ((h (make-hash-table :test 'equal)))
                             (dolist (s optional-systems) (setf (gethash s h) t))
                             h))))
    (labels ((root-optional-p (sys)
               (and optional-tbl (gethash sys optional-tbl)))
             (walk (system-id depth path)
               (when (member system-id path :test #'string=)
                 (%print-tree-line depth system-id " (cycle)")
                 (return-from walk nil))
               (let ((opt-suffix (if (and (zerop depth) (root-optional-p system-id))
                                     " (optional)"
                                     "")))
                 (multiple-value-bind (deps presentp) (gethash system-id graph)
                   (unless presentp
                     (%print-tree-line depth system-id
                                       (concatenate 'string " (missing from lockfile)"
                                                    opt-suffix))
                     (return-from walk nil))
                   (when (gethash system-id expanded)
                     (%print-tree-line depth system-id
                                       (concatenate 'string " (*)" opt-suffix))
                     (return-from walk nil))
                   (%print-tree-line depth system-id opt-suffix)
                   (when (and (integerp depth-limit)
                              (>= depth depth-limit))
                     (return-from walk nil))
                   (setf (gethash system-id expanded) t)
                   (dolist (dep deps)
                     (walk dep (1+ depth) (cons system-id path)))))))
      (dolist (root roots)
        (walk root 0 '())))))

(defun cmd-tree (&rest args)
  "Print a dependency tree from the current project's lockfile."
  (labels ((usage-error (fmt &rest fmt-args)
             (apply #'log-error fmt fmt-args)
             (log-error "Usage: clpm [-p <member>] deps tree [--depth N]")
             (return-from cmd-tree 1)))
    (let ((depth-limit nil))
      ;; Parse args
      (let ((i 0))
        (loop while (< i (length args)) do
          (let ((arg (nth i args)))
            (cond
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

      (multiple-value-bind (_project-root manifest-path lock-path workspace-root _workspace-path)
          (find-effective-project-root)
        (declare (ignore _project-root _workspace-path))
        (unless manifest-path
          (when (null workspace-root)
            (log-no-project-found))
          (return-from cmd-tree 1))
        (unless lock-path
          (log-error "No clpm.lock found - run 'clpm deps sync --to lock' first")
          (return-from cmd-tree 1))
        (let* ((project (clpm.project:read-project-file manifest-path))
               (lockfile (clpm.project:read-lock-file lock-path))
               (roots (%project-root-system-ids project)))
          (when (null roots)
            (log-info "No dependencies")
            (return-from cmd-tree 0))
          (%print-dependency-tree roots (%lockfile-graph lockfile)
                                  :depth-limit depth-limit
                                  :optional-systems (%project-optional-system-ids project))
          0)))))

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
             (log-error "Usage: clpm [-p <member>] deps why <system-id>")
             (return-from cmd-why 1)))
    (let ((target nil))
      ;; Parse args
      (let ((i 0))
        (loop while (< i (length args)) do
          (let ((arg (nth i args)))
            (cond
              ((and (stringp arg) (plusp (length arg)) (char= (char arg 0) #\-))
               (usage-error "Unknown option: ~A" arg))
              ((null target)
               (setf target arg))
              (t
               (usage-error "Unexpected argument: ~A" arg))))
          (incf i)))

      (unless (and (stringp target) (plusp (length target)))
        (usage-error "Missing <system-id>"))

      (multiple-value-bind (_project-root manifest-path lock-path workspace-root _workspace-path)
          (find-effective-project-root)
        (declare (ignore _project-root _workspace-path))
        (unless manifest-path
          (when (null workspace-root)
            (log-no-project-found))
          (return-from cmd-why 1))
        (unless lock-path
          (log-error "No clpm.lock found - run 'clpm deps sync --to lock' first")
          (return-from cmd-why 1))
        (let* ((project (clpm.project:read-project-file manifest-path))
               (lockfile (clpm.project:read-lock-file lock-path))
               (roots (%project-root-system-ids project))
               (optional-systems (%project-optional-system-ids project)))
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
              (let* ((root (first path))
                     (optional-p (member root optional-systems :test #'string=)))
                (format t "  ~{~A~^ -> ~}~A~%" path
                        (if optional-p " (optional)" ""))))
            0))))))

;;; resolve command

(defun %merge-optional-sets (persisted cli)
  "Combine the lockfile-persisted opt-in list with this invocation's CLI value.

  PERSISTED is a list of system-id strings (or NIL).
  CLI is NIL, :ALL, or a list of system-id strings.

Returns a value usable directly as the solver's :with-optional argument."
  (cond
    ((eq cli :all) :all)
    ((and (null cli) (null persisted)) nil)
    (t (remove-duplicates (append (or persisted '()) (or cli '()))
                          :test #'string=))))

(defun %effective-opted-in-systems (project with-optional)
  "Resolve WITH-OPTIONAL into the concrete list of system-ids to persist.

  :ALL expands to every declared optional dep; a list is intersected with
declared optional deps so unknown systems do not silently leak into the
lockfile; NIL persists no opt-ins."
  (let ((declared-optionals
          (loop for dep in (append (clpm.project:project-depends project)
                                   (clpm.project:project-dev-depends project)
                                   (clpm.project:project-test-depends project))
                when (clpm.project:dependency-optional-p dep)
                  collect (clpm.project:dependency-system dep))))
    (cond
      ((eq with-optional :all) (sort (copy-list declared-optionals) #'string<))
      ((listp with-optional)
       (sort (intersection with-optional declared-optionals :test #'string=)
             #'string<))
      (t '()))))

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
                       (ignore-errors (clpm.project:read-lock-file lock-path))))
           (persisted (and lockfile (clpm.project:lockfile-opted-in-optionals lockfile)))
           (with-optional (%merge-optional-sets persisted *with-optional*)))

      (when (and lockfile
                 (null *with-optional*)
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
                                                 :lockfile lockfile
                                                 :with-optional with-optional))
                   (effective-opt-in
                     (%effective-opted-in-systems project with-optional))
                   (new-lockfile (clpm.solver:resolution-to-lockfile
                                  resolution project registries
                                  :opted-in-optionals effective-opt-in)))
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
      (log-error "No clpm.lock found - run 'clpm deps sync --to lock' first")
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
      (log-error "No clpm.lock found - run 'clpm deps sync --to lock' first")
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
      (log-info "Run 'clpm repl eval FORM' or 'clpm repl daemon --detach' for live Lisp debugging")
      ;; Manifest may request that the repl daemon come up automatically
      ;; on install. Skipped when a daemon for this project is already running,
      ;; or when stdout isn't a tty (we don't want this happening inside CI).
      (let* ((rb (and project (clpm.project:project-repl project)))
             (autostart (and rb (getf rb :autostart))))
        (when autostart
          (%bridge-maybe-autostart project-root))))
    0))

;;; update command

(defun cmd-update (&rest systems)
  "Update dependencies. With no arguments, re-resolves every system from
scratch. Given one or more system IDs, only those systems are unlocked;
every other system is pinned to its current lockfile selection (so unrelated
deps don't churn)."
  (multiple-value-bind (project-root manifest-path lock-path workspace-root _workspace-path)
      (find-effective-project-root)
    (declare (ignore _workspace-path))
    (unless manifest-path
      (when (null workspace-root)
        (log-no-project-found))
      (return-from cmd-update 1))
    (let* ((project (clpm.project:read-project-file manifest-path))
           (registries (load-project-registries project))
           (selective (not (null systems)))
           (existing-lock (and selective lock-path
                               (clpm.project:read-lock-file lock-path))))
      (when selective
        (unless existing-lock
          (log-error "No clpm.lock found - run 'clpm deps sync' before 'clpm deps update <system>'")
          (return-from cmd-update 1))
        (let ((locked-ids (mapcar #'clpm.project:locked-system-id
                                  (clpm.project:lockfile-resolved existing-lock))))
          (dolist (sys systems)
            (unless (member sys locked-ids :test #'string=)
              (log-error "System ~A is not present in the current lockfile" sys)
              (return-from cmd-update 1)))))
      (if selective
          (log-info "Updating ~{~A~^, ~}..." systems)
          (log-info "Updating all dependencies..."))
      (dolist (reg registries)
        (log-verbose "Updating registry: ~A" (clpm.registry:registry-name reg))
        (handler-case
            (clpm.registry:update-registry reg)
          (error (c)
            (log-error "Failed to update registry ~A: ~A"
                       (clpm.registry:registry-name reg) c))))
      (let* ((persisted (and existing-lock
                              (clpm.project:lockfile-opted-in-optionals existing-lock)))
             (with-optional (%merge-optional-sets persisted *with-optional*)))
        (handler-case
            (let* ((resolution (clpm.solver:solve
                                project registries
                                :lockfile (when selective existing-lock)
                                :unlock-set (if selective systems :all)
                                :with-optional with-optional))
                   (effective-opt-in
                     (%effective-opted-in-systems project with-optional))
                   (new-lockfile (clpm.solver:resolution-to-lockfile
                                  resolution project registries
                                  :opted-in-optionals effective-opt-in))
                   (lock-out (merge-pathnames "clpm.lock" project-root)))
              (clpm.project:write-lock-file new-lockfile lock-out)
              (log-info "Updated clpm.lock"))
          (clpm.errors:clpm-resolve-error (c)
            (log-error "~A" c)
            (return-from cmd-update 2)))))
    0))

(defun cmd-deps-sync (&rest args)
  "Realize dependency state through a selected pipeline stage."
  (let ((stage :active))
    (labels ((usage-error (fmt &rest fmt-args)
               (apply #'log-error fmt fmt-args)
               (log-error "Usage: clpm deps sync [--to lock|source|build|active]")
               (return-from cmd-deps-sync 1))
             (parse-stage (raw)
               (cond
                 ((string= raw "lock") :lock)
                 ((string= raw "source") :source)
                 ((string= raw "build") :build)
                 ((string= raw "active") :active)
                 (t nil)))
             (run-step (fn)
               (let ((rc (funcall fn)))
                 (unless (zerop rc)
                   (return-from cmd-deps-sync rc)))))
      (loop while args do
        (let ((arg (pop args)))
          (cond
            ((string= arg "--to")
             (let ((raw (pop args)))
               (unless (and (stringp raw) (plusp (length raw)))
                 (usage-error "Missing value for --to"))
               (let ((parsed (parse-stage raw)))
                 (unless parsed
                   (usage-error "Invalid sync stage: ~A" raw))
                 (setf stage parsed))))
            (t
             (usage-error "Unknown option: ~A" arg)))))
      (case stage
        (:lock
         (cmd-resolve))
        (:source
         (run-step #'cmd-resolve)
         (cmd-fetch))
        (:build
         (run-step #'cmd-resolve)
         (run-step #'cmd-fetch)
         (cmd-build))
        (:active
         (cmd-install))))))

(defun cmd-deps (&rest args)
  "Dispatch dependency-resource operations."
  (let ((sub (first args))
        (rest (rest args)))
    (labels ((usage ()
               (log-error "Usage:")
               (log-error "  clpm deps add [--dev|--test] [--any|--caret] [--registry <name>] [--path <dir> | --git <url> --ref <ref>] <system>...")
               (log-error "  clpm deps remove [--dev|--test] <system>")
               (log-error "  clpm deps sync [--to lock|source|build|active]")
               (log-error "  clpm deps update [system ...]")
               (log-error "  clpm deps search <query> [--limit N] [--json]")
               (log-error "  clpm deps info <system> [--json] [--all]")
               (log-error "  clpm [-p <member>] deps tree [--depth N]")
               (log-error "  clpm [-p <member>] deps why <system>")
               (log-error "  clpm deps audit [--json]")
               (log-error "  clpm deps sbom --format <format> [--out <path>]")
               1))
      (cond
        ((or (null sub) (string= sub "help") (string= sub "--help"))
         (usage))
        ((string= sub "add")
         (apply #'cmd-add rest))
        ((string= sub "remove")
         (apply #'cmd-remove rest))
        ((string= sub "sync")
         (apply #'cmd-deps-sync rest))
        ((string= sub "update")
         (apply #'cmd-update rest))
        ((string= sub "search")
         (apply #'cmd-search rest))
        ((string= sub "info")
         (apply #'cmd-info rest))
        ((string= sub "tree")
         (apply #'cmd-tree rest))
        ((string= sub "why")
         (apply #'cmd-why rest))
        ((string= sub "audit")
         (apply #'cmd-audit rest))
        ((string= sub "sbom")
         (apply #'cmd-sbom rest))
        (t
         (log-error "Unknown deps subcommand: ~A" sub)
         (usage))))))

;;; repl command -- persistent Lisp image for LLM-driven dev
;;;
;;; Layout per project:
;;;   .clpm/repl.sock   Unix socket the daemon binds (mode 0600)
;;;   .clpm/repl.pid    decimal PID of the running daemon
;;;   .clpm/repl.log    daemon stdout+stderr in --detach mode
;;;
;;; `daemon' loads the project (via .clpm/asdf-config.lisp) and enters the
;;; accept loop. `eval' auto-spawns a `daemon --detach' child if no daemon is
;;; running, then sends one eval request.

(defun %bridge-windows-p ()
  "Are we running on Windows? Bridge picks TCP transport when true."
  (and (find-package "UIOP/OS")
       (fboundp (find-symbol "OS-WINDOWS-P" "UIOP/OS"))
       (funcall (find-symbol "OS-WINDOWS-P" "UIOP/OS"))))

(defun %bridge-paths (project-root)
  "Return (values endpoint pid-path log-path) for PROJECT-ROOT.

ENDPOINT is the per-project advertisement file passed to start-server and
send-request. On Unix that's `.clpm/repl.sock' (the bound Unix
socket); on Windows that's `.clpm/repl.port' (a two-line file
giving the loopback port and a 32-hex shared token)."
  (let* ((dir (merge-pathnames ".clpm/" project-root))
         (endpoint-name (if (%bridge-windows-p)
                            "repl.port"
                            "repl.sock")))
    (values (namestring (merge-pathnames endpoint-name dir))
            (namestring (merge-pathnames "repl.pid" dir))
            (namestring (merge-pathnames "repl.log" dir)))))

(defun %bridge-project-root-id (project-root)
  "Return the canonical project identity used by the repl daemon protocol."
  (namestring (uiop:ensure-directory-pathname (truename project-root))))

(defun %bridge-params-with-project-root (params project-root)
  "Return PARAMS with the daemon project identity attached."
  (let ((project-id (%bridge-project-root-id project-root)))
    (cond
      ((null params)
       (list :object (list (cons "project_root" project-id))))
      ((and (consp params) (eq (car params) :object))
       (list :object
             (acons "project_root" project-id
                    (remove "project_root" (cadr params)
                            :key #'car
                            :test #'string=))))
      (t params))))

(defun %bridge-read-pidfile (path)
  "Return the integer PID stored in PATH, or NIL if the file is missing /
malformed."
  (when (uiop:file-exists-p path)
    (handler-case
        (let* ((text (uiop:read-file-string path))
               (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
          (parse-integer trimmed :junk-allowed nil))
      (error () nil))))

(defun %bridge-pid-alive-p (pid)
  "Best-effort: is PID running? Uses kill(pid, 0). Returns NIL on any error."
  (handler-case
      (progn (sb-posix:kill pid 0) t)
    (error () nil)))

(defun %bridge-clean-stale (sock-path pid-path)
  "Remove SOCK-PATH and PID-PATH if the PID is dead or the pidfile is bogus."
  (let ((pid (%bridge-read-pidfile pid-path)))
    (when (or (null pid) (not (%bridge-pid-alive-p pid)))
      (ignore-errors (delete-file pid-path))
      (ignore-errors (delete-file sock-path))
      t)))

(defun %bridge-write-pidfile (path)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output :if-exists :supersede
                          :external-format :utf-8)
    (format s "~D~%" (sb-posix:getpid))))

(defun %bridge-load-project (project-root)
  "Activate the project and preload its declared systems.

Phase 1: load `.clpm/asdf-config.lisp' if present, which puts the
lockfile-resolved sources on the ASDF source-registry.

Phase 2: read `clpm.project' and `asdf:load-system' each name in
`:systems'. Their transitive dependencies come along for free. We don't
load `:dev-depends'/`:test-depends' here -- those are scoped to dev/test
workflows, not the runtime image."
  (let ((config (merge-pathnames ".clpm/asdf-config.lisp" project-root))
        (manifest (merge-pathnames "clpm.project" project-root)))
    (when (uiop:file-exists-p config)
      (handler-case
          (load config)
        (error (c)
          (log-error "Failed to load project config: ~A" c)
          (return-from %bridge-load-project))))
    (when (uiop:file-exists-p manifest)
      (handler-case
          (let* ((proj (clpm.project:read-project-file manifest))
                 (systems (and proj (clpm.project:project-systems proj)))
                 (rb (and proj (clpm.project:project-repl proj)))
                 (extra (getf rb :preload)))
            (dolist (sys (remove-if-not #'stringp
                                        (append systems
                                                (when (listp extra) extra))))
              (handler-case
                  (asdf:load-system sys :verbose nil)
                (error (c)
                  (log-error "Failed to preload system ~A: ~A" sys c)))))
        (error (c)
          (log-error "Failed to parse project manifest: ~A" c))))))

;; Foreground `daemon' never touches stdio. Detachment is done by the parent
;; via `uiop:launch-program` which inherits the right file descriptors --
;; we don't need fork/setsid/dup2 from inside the daemon.

(defun %bridge-pathlike-p (string)
  (and (stringp string)
       (or (find #\/ string)
           (find #\\ string))))

(defun %bridge-existing-file (path)
  (let ((found (and path (uiop:file-exists-p path))))
    (when (and found (not (uiop:directory-pathname-p found)))
      (namestring (truename found)))))

(defun %bridge-saved-sbcl-executable ()
  #+sbcl
  (let* ((runtime (and sb-ext:*runtime-pathname*
                       (uiop:file-exists-p sb-ext:*runtime-pathname*)))
         (core (and sb-ext:*core-pathname*
                    (uiop:file-exists-p sb-ext:*core-pathname*))))
    (when (and runtime core
               (string= (namestring (truename runtime))
                        (namestring (truename core))))
      (namestring (truename runtime))))
  #-sbcl
  nil)

(defun %bridge-clpm-executable ()
  "Return the executable that can re-enter this CLPM image, or NIL."
  (let ((argv0 (uiop:argv0)))
    (or (and (%bridge-pathlike-p argv0)
             (%bridge-existing-file argv0))
        (%bridge-saved-sbcl-executable)
        (and (stringp argv0)
             (plusp (length argv0))
             (not (%bridge-pathlike-p argv0))
             (which argv0)))))

(defun %bridge-maybe-autostart (project-root)
  "If the manifest sets `:repl (:autostart t ...)`, ensure a daemon is
running for PROJECT-ROOT by launching `daemon --detach' when one is not.

Idempotent: if the existing pidfile points at a live process, do nothing.
Failures are logged but never propagate -- `clpm deps sync' must not fail
because the daemon couldn't come up."
  (handler-case
      (multiple-value-bind (sock pid log) (%bridge-paths project-root)
        (declare (ignore log))
        (%bridge-clean-stale sock pid)
        (let ((existing (%bridge-read-pidfile pid)))
          (when (and existing (%bridge-pid-alive-p existing))
            (log-info "repl daemon already running (pid ~D)" existing)
            (return-from %bridge-maybe-autostart)))
        (let ((clpm-bin (%bridge-clpm-executable)))
          (unless clpm-bin
            (log-error "repl autostart: could not find current clpm executable")
            (return-from %bridge-maybe-autostart))
          (log-info "repl: starting daemon (autostart from manifest)")
          (handler-case
              (uiop:with-current-directory (project-root)
                (uiop:launch-program (list clpm-bin "repl" "daemon"
                                           "--detach")
                                     :output nil :error-output nil :input nil))
            (error (c)
              (log-error "repl autostart failed: ~A" c)))))
    (error (c)
      (log-error "repl autostart: ~A" c))))

(defun %bridge-resolve-project ()
  "Return (values project-root sock pid log) or NIL on error (after logging)."
  (multiple-value-bind (project-root manifest-path lock-path workspace-root)
      (find-effective-project-root)
    (declare (ignore lock-path))
    (unless manifest-path
      (when (null workspace-root)
        (log-no-project-found))
      (return-from %bridge-resolve-project nil))
    (multiple-value-bind (sock pid log) (%bridge-paths project-root)
      (values project-root sock pid log))))

(defun %bridge-daemon-start (args)
  "Start `clpm repl daemon [--detach] [--no-load]'."
  (let ((detach nil)
        (no-load nil))
    (loop while args do
      (let ((arg (pop args)))
        (cond
          ((string= arg "--detach") (setf detach t))
          ((string= arg "--no-load") (setf no-load t))
          (t
           (log-error "Unknown daemon option: ~A" arg)
           (return-from %bridge-daemon-start 1)))))
    (multiple-value-bind (project-root sock pid log)
        (%bridge-resolve-project)
      (unless project-root (return-from %bridge-daemon-start 1))
      ;; If a live daemon is already running, refuse.
      (%bridge-clean-stale sock pid)
      (let ((existing (%bridge-read-pidfile pid)))
        (when (and existing (%bridge-pid-alive-p existing))
          (log-error "Daemon already running (pid ~D, socket ~A)" existing sock)
          (return-from %bridge-daemon-start 1)))
      (cond
        (detach
         (let ((clpm-bin (%bridge-clpm-executable)))
           (unless clpm-bin
             (log-error "Could not find current clpm executable; --detach unavailable")
             (return-from %bridge-daemon-start 1))
           (let ((argv (append (list clpm-bin "repl" "daemon")
                               (when no-load (list "--no-load")))))
             (ensure-directories-exist log)
             (handler-case
                 (uiop:with-current-directory (project-root)
                   (with-open-file (log-stream log :direction :output
                                                    :if-exists :append
                                                    :if-does-not-exist :create
                                                    :external-format :utf-8)
                     (uiop:launch-program argv
                                          :output log-stream
                                          :error-output log-stream
                                          :input nil)))
               (error (c)
                 (log-error "Failed to launch daemon: ~A" c)
                 (return-from %bridge-daemon-start 1)))
             (loop for i from 0 below 50
                   while (not (probe-file sock))
                   do (sleep 0.1))
             (cond
               ((probe-file sock)
                (log-info "Daemon started: ~A" sock)
                0)
               (t
                (log-error "Daemon failed to bind socket within 5s (see ~A)" log)
                1)))))
        (t
         (let ((previous-cwd (ignore-errors (sb-posix:getcwd))))
           (unwind-protect
                (progn
                  (sb-posix:chdir (namestring project-root))
                  (uiop:with-current-directory (project-root)
                    (ensure-directories-exist sock)
                    (%bridge-write-pidfile pid)
                    (unless no-load
                      (%bridge-load-project project-root))
                    (unwind-protect
                         (handler-case
                             (let ((tcp-p (%bridge-windows-p)))
                               (if tcp-p
                                   (clpm.repl:start-server
                                    :transport-kind :tcp
                                    :port-path sock
                                    :log-path log
                                    :project-root (%bridge-project-root-id project-root))
                                   (clpm.repl:start-server
                                    :transport-kind :unix
                                    :socket-path sock
                                    :log-path log
                                    :project-root (%bridge-project-root-id project-root)))
                               0)
                           (error (c)
                             (format *error-output* "daemon crashed: ~A~%" c)
                             1))
                      (ignore-errors (delete-file pid)))))
             (when previous-cwd
               (ignore-errors (sb-posix:chdir previous-cwd))))))))))

(defvar *bridge-cli-json* nil
  "When set, CLI subcommands print raw JSON instead of human text.")

(defparameter +bridge-daemon-frame-names+
  '("%STRUCTURED-BACKTRACE" "%CONDITION-JSON" "%CAPTURE-ERROR-SNAPSHOT"
    "%ENTER-DEBUGGER" "%SIGNAL" "%EVAL-ONE" "%WORKER-LOOP"
    "SIMPLE-EVAL-IN-LEXENV" "EVAL"
    "(FLET BODY IN RUN)" "(FLET WITHOUT-INTERRUPTS-BODY- IN RUN)" "RUN"
    "(FLET ON-CONDITION IN %EVAL-ONE)")
  "Frame names that belong to the daemon/SBCL plumbing and are not
useful to a user. The pretty-print path elides them.")

(defun %bridge-daemon-frame? (name)
  "Is NAME (the printed frame name) part of the daemon/sbcl scaffolding?"
  (and (stringp name)
       (or (member name +bridge-daemon-frame-names+ :test #'string=)
           ;; CLPM.REPL::%FOO-style frame names.
           (and (search "%" name) (search "CLPM.REPL" name)))))

(defun %bridge-user-frames (frames)
  "FRAMES is a JSON array of frame objects. Drop daemon scaffolding from
the *bottom* of the stack and keep the user portion. The first
`SIMPLE-EVAL-IN-LEXENV' frame (and everything below it) is daemon."
  (let ((items (%bridge-array-items frames))
        (kept '()))
    (loop for f in (or items '())
          for fo = (cadr f)
          for name = (%bridge-field fo "name")
          while (not (or (string= (or name "") "SIMPLE-EVAL-IN-LEXENV")
                         (string= (or name "") "%EVAL-ONE")))
          do (unless (%bridge-daemon-frame? name)
               (push fo kept)))
    (nreverse kept)))

(defun %bridge-print-restart (stream restart-obj)
  "Render one restart object as `  NAME [arity hint] [, interactive] -- report'.

`args_arity' may be a non-negative integer (exact required count) or
the string \"variadic\" -- the daemon's signal that SBCL hides a
multi-arg restart-case clause behind an &REST wrapper."
  (let ((name (%bridge-field restart-obj "name"))
        (arity (%bridge-field restart-obj "args_arity"))
        (interactive (%bridge-field restart-obj "interactive"))
        (report (%bridge-field restart-obj "report")))
    (format stream "  ~A" (or name "?"))
    (let ((arity-text
            (cond
              ((and (integerp arity) (plusp arity))
               (format nil "~D arg~:P" arity))
              ((and (stringp arity) (string= arity "variadic"))
               "variadic args"))))
      (when (or arity-text interactive)
        (format stream " (~@[~A~]~:[~;~:[~;, ~]interactive~])"
                arity-text interactive arity-text)))
    (when (and report (stringp report) (plusp (length report)))
      (format stream " -- ~A"
              (if (search #.(string #\Newline) report)
                  (subseq report 0 (position #\Newline report))
                  report)))
    (format stream "~%")))

(defun %bridge-print-frames (stream frame-objs)
  "Render a small user-only backtrace to STREAM, with args + source."
  (when frame-objs
    (format stream "frames:~%")
    (dolist (fo frame-objs)
      (let* ((i (%bridge-field fo "i"))
             (name (%bridge-field fo "name"))
             (args (%bridge-array-items (%bridge-field fo "args")))
             (src (%bridge-unwrap (%bridge-field fo "source")))
             (file (%bridge-field src "file"))
             (line (%bridge-field src "line")))
        (format stream "  ~A: ~A~@[ ~A~]~@[  (~A~@[:~A~])~]~%"
                (or i "?")
                (or name "?")
                (and args (format nil "[~{~A~^ ~}]" args))
                file line)))))

(defun %bridge-pretty-print-success (obj stream)
  "Pretty-print a successful eval result OBJ to STREAM."
  (let* ((value (%bridge-field obj "value"))
         (values-raw (%bridge-array-items (%bridge-field obj "values")))
         (multi (and values-raw (> (length values-raw) 1)))
         (output (%bridge-field obj "output"))
         (eo (%bridge-field obj "error_output"))
         (pkg (%bridge-field obj "package"))
         (elapsed (%bridge-field obj "elapsed_ms"))
         (redef (%bridge-unwrap (%bridge-field obj "redefined"))))
    (when (and output (stringp output) (plusp (length output)))
      (format stream "stdout:~%~A~%" output))
    (when (and eo (stringp eo) (plusp (length eo)))
      (format stream "stderr:~%~A~%" eo))
    (cond
      (multi
       (loop for v in values-raw for i from 0
             do (format stream "; ~D => ~A~%" i v)))
      (value
       (format stream "=> ~A~%" value)))
    (when redef
      (format stream "redefined: ~A ~A~%"
              (or (%bridge-field redef "kind") "?")
              (or (%bridge-field redef "name") "?")))
    (when (and pkg (stringp pkg) (not (string= pkg "COMMON-LISP-USER")))
      (format stream "package: ~A~%" pkg))
    (when (and elapsed (integerp elapsed) (> elapsed 100))
      (format stream "elapsed: ~Dms~%" elapsed))))

(defun %bridge-pretty-print-error (err-obj stream)
  "Pretty-print an error response's error-obj to STREAM.

Shows code, message, restarts, and the user-only portion of the
backtrace. Long internal `vars' dumps are dropped."
  (let* ((code (%bridge-field err-obj "code"))
         (msg (%bridge-field err-obj "message"))
         (details (%bridge-unwrap (%bridge-field err-obj "details")))
         (output (%bridge-field details "output"))
         (eo (%bridge-field details "error_output"))
         (conds (%bridge-array-items (%bridge-field details "conditions")))
         (attempts (%bridge-array-items (%bridge-field details "handler_attempts"))))
    (format stream "error: [~A] ~A~%" (or code "?") (or msg ""))
    (when (and output (stringp output) (plusp (length output)))
      (format stream "stdout:~%~A~%" output))
    (when (and eo (stringp eo) (plusp (length eo)))
      (format stream "stderr:~%~A~%" eo))
    (dolist (c conds)
      (let* ((co (cadr c))
             (ctype (%bridge-field co "type"))
             (restarts (%bridge-array-items (%bridge-field co "restarts")))
             (frames (%bridge-array-items (%bridge-field co "backtrace"))))
        (when (and ctype (stringp ctype) (not (string= ctype "")))
          (format stream "type:  ~A~%" ctype))
        (when restarts
          (format stream "restarts (~D):~%" (length restarts))
          (dolist (r restarts)
            (%bridge-print-restart stream (cadr r)))
          (format stream "  -> rerun with: clpm repl eval FORM --debug --restart NAME [--arg V]~%"))
        (let ((user (%bridge-user-frames (list :array frames))))
          (when user
            (%bridge-print-frames stream user)))))
    (%bridge-render-handler-attempts stream attempts)))

(defun %bridge-render-handler-attempts (stream attempts)
  "Surface --handler specs that matched by type but couldn't fire.
Without this, the eval looks indistinguishable from `no --handler given',
which makes the flag impossible to debug from the CLI."
  (when attempts
    (format stream "handlers tried (~D):~%" (length attempts))
    (dolist (a attempts)
      (let* ((ao (cadr a))
             (type (%bridge-field ao "type"))
             (restart (%bridge-field ao "restart"))
             (avail (%bridge-array-items (%bridge-field ao "available_restarts"))))
        (format stream "  ~A => ~A: no such restart~@[ (available: ~{~A~^, ~})~]~%"
                (or type "?") (or restart "?")
                (when avail (mapcar (lambda (x) (if (stringp x) x "?"))
                                    avail)))))
    (format stream "  -> wrap the form in `(restart-case FORM (~A (...) ...))' or pick a restart that is established.~%"
            (or (let* ((a0 (cadr (first attempts)))
                       (r (and a0 (%bridge-field a0 "restart"))))
                  (and (stringp r) (string-upcase r)))
                "USE-VALUE"))))

(defun %bridge-pretty-print (response stream)
  "Render RESPONSE (a parsed JSON object) as a human summary.

Eval responses get a rich layout (multi-value handling, redefined
banner, restart list, user-only backtrace). Other RPC responses are
JSON-dumped as a last resort — but every CLI subcommand now has its
own renderer so this fallback is rarely hit."
  (cond
    ((not (and (consp response) (eq (car response) :object)))
     (clpm.io.json:write-json response stream) (terpri stream))
    (t
     (let ((result (%bridge-obj response))
           (err (%bridge-err response))
           (warning (%bridge-warning response)))
       (when (and warning (stringp warning))
         (format stream "warning: ~A~%" warning))
       (cond
         (result (%bridge-pretty-print-success result stream))
         (err (%bridge-pretty-print-error err stream))
         (t (clpm.io.json:write-json response stream)
            (terpri stream)))))))

(defun %bridge-send-or-autostart (sock pid project-root method
                                  &key params (autostart t) on-event)
  "Send a request, auto-starting the daemon on connect failure."
  (let ((wire-params (%bridge-params-with-project-root params project-root)))
    (labels ((send-once (timeout)
               (clpm.repl:send-request sock method
                                       :params wire-params
                                       :connect-timeout timeout
                                       :on-event on-event))
             (no-daemon ()
               (log-error "No daemon running for this project. Start one with `clpm repl daemon --detach`.")
               nil)
             (start-and-send ()
               (let ((rc (%bridge-daemon-start (list "--detach"))))
                 (declare (ignore rc))
                 (send-once 5))))
      (let ((resp (send-once 1)))
        (cond
          ((eq resp :no-daemon)
           (if autostart
               (start-and-send)
               (no-daemon)))
          ((%bridge-project-root-error-p resp)
           (%bridge-clean-lifecycle-files sock pid)
           (if autostart
               (start-and-send)
               (no-daemon)))
          (t resp))))))

(defun %bridge-split-on (char string)
  "Split STRING on every occurrence of CHAR (a character). Empty fields
between adjacent separators are preserved; an empty string returns NIL."
  (when (and (stringp string) (plusp (length string)))
    (loop with parts = '()
          with start = 0
          for i from 0 below (length string)
          when (char= (char string i) char) do
            (push (subseq string start i) parts)
            (setf start (1+ i))
          finally (push (subseq string start) parts)
                  (return (nreverse parts)))))

(defun %bridge-parse-handler-spec (spec)
  "Parse one `--handler TYPE=RESTART[:ARG1[,ARG2[,...]]]' string into a
JSON object `{type, restart, args}'. Returns NIL on a malformed spec."
  (let ((eq-pos (position #\= spec)))
    (unless (and eq-pos (plusp eq-pos))
      (log-error "Bad --handler spec ~S (expected TYPE=RESTART[:ARGS])" spec)
      (return-from %bridge-parse-handler-spec nil))
    (let* ((type-text (subseq spec 0 eq-pos))
           (rest-text (subseq spec (1+ eq-pos)))
           (colon (position #\: rest-text))
           (restart-text (if colon (subseq rest-text 0 colon) rest-text))
           (args-text (when colon (subseq rest-text (1+ colon))))
           (args (%bridge-split-on #\, args-text)))
      (list :object
            (list (cons "type" type-text)
                  (cons "restart" restart-text)
                  (cons "args" (list :array (or args '()))))))))

(defun %bridge-eval (args)
  "Handle `clpm repl eval FORM [--package PKG] [--worker W]
                                 [--handler TYPE=RESTART[:ARG,...]]...
                                 [--debug] [--restart NAME] [--frame N]
                                 [--frame-eval FORM] [--keep]
                                 [--break-on TYPE] [--timeout-ms N]
                                 [--no-autostart] [--json]'.

Default rendering is human-readable; pass `--json' for the raw JSON line.

`--handler' may appear multiple times -- each spec is applied as a
declarative restart for any condition matching TYPE. ARGS, if given,
are read+evaluated daemon-side at recovery time.

`--debug' switches to the continuation-aware path. With no selector it
prints the first debugger stop and aborts it; with `--keep' it leaves a
server-owned session for later `call debug-* ...' requests."
  (let ((form nil)
        (package nil)
        (worker nil)
        (autostart t)
        (json nil)
        (debug nil)
        (restart nil)
        (restart-args '())
        (frame nil)
        (frame-eval nil)
        (keep nil)
        (break-on nil)
        (timeout-ms nil)
        (handlers '()))
    (loop while args do
      (let ((arg (pop args)))
        (cond
          ((string= arg "--package")
           (setf package (pop args))
           (unless (stringp package)
             (log-error "Missing value for --package")
             (return-from %bridge-eval 1)))
          ((string= arg "--worker")
           (setf worker (pop args))
           (unless (stringp worker)
             (log-error "Missing value for --worker")
             (return-from %bridge-eval 1)))
          ((string= arg "--handler")
           (let ((spec (pop args)))
             (unless (stringp spec)
               (log-error "Missing value for --handler")
               (return-from %bridge-eval 1))
             (let ((parsed (%bridge-parse-handler-spec spec)))
               (unless parsed (return-from %bridge-eval 1))
               (push parsed handlers))))
          ((string= arg "--no-autostart") (setf autostart nil))
          ((string= arg "--json") (setf json t))
          ((string= arg "--debug") (setf debug t))
          ((string= arg "--restart")
           (setf debug t
                 restart (pop args))
           (unless (stringp restart)
             (log-error "Missing value for --restart")
             (return-from %bridge-eval 1)))
          ((string= arg "--arg")
           (setf debug t)
           (let ((value (pop args)))
             (unless (stringp value)
               (log-error "Missing value for --arg")
               (return-from %bridge-eval 1))
             (push value restart-args)))
          ((string= arg "--frame")
           (setf debug t)
           (let* ((raw (pop args))
                  (n (and raw (ignore-errors
                               (parse-integer raw :junk-allowed nil)))))
             (unless (integerp n)
               (log-error "Invalid integer for --frame: ~A" raw)
               (return-from %bridge-eval 1))
             (setf frame n)))
          ((string= arg "--frame-eval")
           (setf debug t
                 frame-eval (pop args))
           (unless (stringp frame-eval)
             (log-error "Missing value for --frame-eval")
             (return-from %bridge-eval 1)))
          ((string= arg "--keep") (setf debug t keep t))
          ((string= arg "--break-on")
           (setf debug t
                 break-on (pop args))
           (unless (stringp break-on)
             (log-error "Missing value for --break-on")
             (return-from %bridge-eval 1)))
          ((string= arg "--timeout-ms")
           (setf debug t)
           (let* ((raw (pop args))
                  (n (and raw (ignore-errors
                               (parse-integer raw :junk-allowed nil)))))
             (unless (integerp n)
               (log-error "Invalid integer for --timeout-ms: ~A" raw)
               (return-from %bridge-eval 1))
             (setf timeout-ms n)))
          ((null form) (setf form arg))
          (t
           (log-error "Unknown eval option: ~A" arg)
           (return-from %bridge-eval 1)))))
    (unless form
      (log-error "Usage: clpm repl eval FORM [--package PKG] [--worker W] [--handler T=R[:A,...]]... [--debug] [--json]")
      (return-from %bridge-eval 1))
    (when (and restart-args (null restart))
      (log-error "--arg requires --restart")
      (return-from %bridge-eval 1))
    (when (or (and frame (null frame-eval))
              (and frame-eval (null frame)))
      (log-error "--frame and --frame-eval must be supplied together")
      (return-from %bridge-eval 1))
    (let ((continuation-actions 0))
      (when keep (incf continuation-actions))
      (when restart (incf continuation-actions))
      (when (and frame frame-eval) (incf continuation-actions))
      (when (> continuation-actions 1)
        (log-error "Choose only one debug continuation action: --keep, --restart, or --frame/--frame-eval")
        (return-from %bridge-eval 1)))
    (multiple-value-bind (project-root sock pid)
        (%bridge-resolve-project)
      (unless project-root (return-from %bridge-eval 1))
      (cond
        (debug
         (let ((*bridge-cli-json* (or *bridge-cli-json* json)))
           (labels ((foreign-project-p (ping)
                      (or (%bridge-project-root-error-p ping)
                          (let* ((result (%bridge-obj ping))
                                 (reported (%bridge-field result
                                                          "project_root")))
                            (and result
                                 (not (and (stringp reported)
                                           (string= reported
                                                    (%bridge-project-root-id
                                                     project-root))))))))
                    (open-checked ()
                      (let ((conn (clpm.repl:open-connection
                                   sock :connect-timeout 5)))
                        (cond
                          ((eq conn :no-daemon) :no-daemon)
                          (t
                           (let ((ping (clpm.repl:send-on-connection
                                        conn "ping"
                                        :params (%bridge-params-with-project-root
                                                 nil project-root))))
                             (cond
                               ((foreign-project-p ping)
                                (clpm.repl:close-connection conn)
                                (%bridge-clean-lifecycle-files sock pid)
                                :no-daemon)
                               (t conn)))))))
                    (open-or-autostart ()
                      (let ((conn (open-checked)))
                        (cond
                          ((and (eq conn :no-daemon) autostart)
                           (let ((rc (%bridge-daemon-start
                                      (list "--detach"))))
                             (declare (ignore rc))
                             (open-checked)))
                          (t conn)))))
             (let ((conn (open-or-autostart)))
               (cond
                 ((eq conn :no-daemon)
                  (log-error "No daemon running and autostart is disabled or failed")
                  2)
                 (t
                  (let ((opts (list :package package
                                    :worker worker
                                    :project-root project-root
                                    :restart restart
                                    :arg (nreverse restart-args)
                                    :frame frame
                                    :frame-eval frame-eval
                                    :keep keep
                                    :break-on break-on
                                    :timeout-ms timeout-ms
                                    :handlers (and handlers
                                                   (list :array
                                                         (nreverse handlers))))))
                    (unwind-protect
                         (%bridge-debug-on-connection conn opts form)
                      (clpm.repl:close-connection conn)))))))))
        (t
         (let* ((params (%bridge-make-params
                         (list (cons "form" form)
                               (cons "package" package)
                               (cons "worker" worker)
                               (cons "handlers"
                                     (when handlers
                                       (list :array (nreverse handlers)))))))
                (resp (%bridge-send-or-autostart sock pid project-root
                                                 "eval"
                                                 :params params
                                                 :autostart autostart)))
           (cond
             ((null resp) 2)
             ((eq resp :no-daemon)
              (log-error "Could not start daemon")
              2)
             ((eq resp :io-error)
              (log-error "I/O error talking to daemon")
              2)
             (t
              (cond
                ((or json *bridge-cli-json*)
                 (%bridge-emit-json resp))
                (t
                 (%bridge-pretty-print resp *standard-output*)))
              (if (assoc "error" (cadr resp) :test #'string=) 1 0)))))))))

(defun %bridge-project-root-error-p (resp)
  (let* ((err (%bridge-err resp))
         (message (%bridge-field err "message")))
    (and (stringp message)
         (search "project_root" message :test #'char-equal))))

(defun %bridge-clean-lifecycle-files (sock pid)
  (ignore-errors (delete-file pid))
  (ignore-errors (delete-file sock)))

(defun %bridge-ping-daemon (sock project-root)
  "Return (values STATE RESPONSE RESULT) for PROJECT-ROOT's daemon endpoint."
  (let ((project-id (%bridge-project-root-id project-root))
        (ping (clpm.repl:send-request sock "ping"
                                      :params (%bridge-params-with-project-root
                                               nil project-root)
                                      :connect-timeout 1)))
    (cond
      ((%bridge-obj ping)
       (let* ((result (%bridge-obj ping))
              (reported (%bridge-field result "project_root")))
         (cond
           ((and (stringp reported) (string= reported project-id))
            (values :running ping result))
           (t
            (values :project-mismatch ping result)))))
      ((%bridge-project-root-error-p ping)
       (values :project-mismatch ping nil))
      (t
       (values :unresponsive ping nil)))))

(defun %bridge-status (args)
  (declare (ignore args))
  (multiple-value-bind (project-root sock pid log)
      (%bridge-resolve-project)
    (unless project-root (return-from %bridge-status 1))
    (flet ((emit-json (state &rest extras)
             (%bridge-emit-json
              (%json-object*
               (list* "state" state
                      "socket" (namestring sock)
                      "log" (namestring log)
                      extras)))))
      (let ((existing (%bridge-read-pidfile pid)))
        (cond
          ((null existing)
           (if *bridge-cli-json*
               (emit-json "not-running")
               (format t "not running~%"))
           0)
          ((not (%bridge-pid-alive-p existing))
           (%bridge-clean-lifecycle-files sock pid)
           (if *bridge-cli-json*
               (emit-json "stale")
               (format t "stale pidfile (cleaned)~%"))
           0)
          (t
           (multiple-value-bind (state _ping obj)
               (%bridge-ping-daemon sock project-root)
             (declare (ignore _ping))
             (cond
               ((eq state :running)
                (let* ((reported-pid (or (%bridge-field obj "pid") existing))
                       (uptime (%bridge-field obj "uptime_ms"))
                       (lisp (%bridge-field obj "lisp"))
                       (evals (%bridge-field obj "eval_count")))
                  (cond
                    (*bridge-cli-json*
                     (emit-json "running"
                                "pid" reported-pid
                                "uptime_ms" uptime
                                "lisp" lisp
                                "eval_count" evals))
                    (t
                     (format t "running (pid ~D)~%" reported-pid)
                     (format t "  socket: ~A~%" sock)
                     (format t "  log:    ~A~%" log)
                     (when uptime (format t "  uptime: ~,1Fs~%" (/ uptime 1000.0)))
                     (when lisp (format t "  lisp:   ~A~%" lisp))
                     (when evals (format t "  evals:  ~D~%" evals)))))
                0)
               ((eq state :project-mismatch)
                (%bridge-clean-lifecycle-files sock pid)
                (if *bridge-cli-json*
                    (emit-json "stale" "reason" "project-mismatch")
                    (format t "stale daemon state (cleaned)~%"))
                0)
               (t
                (cond
                  (*bridge-cli-json*
                   (emit-json "unresponsive" "pid" existing))
                  (t
                   (format t "running but unresponsive (pid ~D)~%" existing)
                   (format t "  try: clpm repl daemon --stop~%")))
                0)))))))))

(defun %json-object* (kv-list)
  "Build a JSON object from a flat property-list KV-LIST. Nil values
are dropped, matching `%bridge-make-params'."
  (let ((cells '()))
    (loop while kv-list do
      (let ((k (pop kv-list))
            (v (pop kv-list)))
        (when v (push (cons k v) cells))))
    (list :object (nreverse cells))))

(defun %bridge-stop (args)
  (declare (ignore args))
  (multiple-value-bind (project-root sock pid)
      (%bridge-resolve-project)
    (unless project-root (return-from %bridge-stop 1))
    (let ((existing (%bridge-read-pidfile pid)))
      (cond
        ((null existing)
         (format t "not running~%")
         0)
        ((not (%bridge-pid-alive-p existing))
         (%bridge-clean-lifecycle-files sock pid)
         (format t "cleaned stale pidfile~%")
         0)
        (t
         (multiple-value-bind (state _ping obj)
             (%bridge-ping-daemon sock project-root)
           (declare (ignore _ping obj))
           (cond
             ((eq state :project-mismatch)
              (%bridge-clean-lifecycle-files sock pid)
              (format t "stale daemon state (cleaned)~%")
              0)
             ((not (eq state :running))
              (format t "running but unresponsive (pid ~D)~%" existing)
              (format t "  try removing stale files under .clpm/ after checking the process~%")
              1)
             (t
              ;; Graceful shutdown: send the request and wait for the socket file
              ;; to disappear. The socket is the authoritative signal: the pid may
              ;; belong to a long-lived host process that hosts more than one daemon.
              (handler-case
                  (clpm.repl:send-request sock "shutdown"
                                          :params (%bridge-params-with-project-root
                                                   nil project-root)
                                          :connect-timeout 1)
                (error () nil))
              ;; Poll for the socket file going away. `probe-file' isn't safe here
              ;; because on macOS it can fault on a Unix-socket path during the
              ;; brief window when the daemon's unwind-protect is unlinking it;
              ;; `uiop:file-exists-p' uses lstat-based checks that don't truename.
              (loop for i from 0 below 50
                    while (uiop:file-exists-p sock)
                    do (sleep 0.1))
              (%bridge-clean-lifecycle-files sock pid)
              (format t "stopped~%")
              0))))))))

(defun %bridge-daemon (args)
  "Lifecycle command for the repl daemon.

`daemon' is the only public lifecycle constructor: bare `daemon' starts
foreground, `--detach' starts background, `--status' observes state, and
`--stop' requests shutdown."
  (let ((status nil)
        (stop nil)
        (json nil)
        (start-args '()))
    (loop while args do
      (let ((arg (pop args)))
        (cond
          ((string= arg "--status") (setf status t))
          ((string= arg "--stop") (setf stop t))
          ((string= arg "--json") (setf json t))
          (t (push arg start-args)))))
    (when (and status stop)
      (log-error "Use only one of --status or --stop")
      (return-from %bridge-daemon 1))
    (when (and (or status stop) start-args)
      (log-error "Daemon start options are incompatible with --status/--stop: ~{~A~^ ~}"
                 (nreverse start-args))
      (return-from %bridge-daemon 1))
    (when (and json (not status))
      (log-error "Use --json only with `clpm repl daemon --status`")
      (return-from %bridge-daemon 1))
    (let ((*bridge-cli-json* (or *bridge-cli-json* json)))
      (cond
        (status (%bridge-status '()))
        (stop (%bridge-stop '()))
        (t (%bridge-daemon-start (nreverse start-args)))))))

;;; ----------------------------------------------------------------------------
;;; Generic helpers used by the new single-shot CLI surface.

(defun %bridge-obj (resp)
  "Return the parsed result object on success, or NIL.

RESP is the parsed JSON value as returned by `send-request' --
ie. `(:object ((\"id\" . N) (\"result\" . (:object ...))))'."
  (when (and (consp resp) (eq (car resp) :object))
    (let ((cell (assoc "result" (cadr resp) :test #'string=)))
      (and cell (consp (cdr cell)) (eq (car (cdr cell)) :object)
           (cadr (cdr cell))))))

(defun %bridge-err (resp)
  "Return the parsed error object, or NIL."
  (when (and (consp resp) (eq (car resp) :object))
    (let ((cell (assoc "error" (cadr resp) :test #'string=)))
      (and cell (consp (cdr cell)) (eq (car (cdr cell)) :object)
           (cadr (cdr cell))))))

(defun %bridge-warning (resp)
  "Return the response's top-level `warning' string, or NIL."
  (when (and (consp resp) (eq (car resp) :object))
    (let ((cell (assoc "warning" (cadr resp) :test #'string=)))
      (and cell (cdr cell)))))

(defun %bridge-field (obj key)
  "Lookup KEY (a string) in an unwrapped JSON object alist OBJ."
  (when (and obj (consp obj))
    (let ((cell (assoc key obj :test #'string=)))
      (and cell (cdr cell)))))

(defun %bridge-unwrap (val)
  "Coerce a JSON value VAL into its unwrapped alist if it's an `:object'.

For nested object slots we often want to peek inside without writing
`(cadr (cdr cell))' boilerplate. Anything else passes through."
  (cond
    ((and (consp val) (eq (car val) :object)) (cadr val))
    (t val)))

(defun %bridge-frame-field (frame key)
  "Lookup KEY inside a top-level JSON frame (parsed `:object' shape)."
  (%bridge-field (%bridge-unwrap frame) key))

(defun %bridge-array-items (a)
  "Unwrap a JSON array value into its element list, NIL if not an array."
  (when (and (consp a) (eq (car a) :array))
    (cadr a)))

(defun %bridge-make-params (alist)
  "Build a JSON-shaped params object from ALIST, dropping NIL values.

Each cell is `(KEY . VALUE)'; if VALUE is NIL it's omitted (so callers
can pass through optional flags safely)."
  (let ((kept (remove-if #'null alist :key #'cdr)))
    (when kept (list :object kept))))

(defun %bridge-string-list (raw)
  "Wrap a Lisp list of strings as a JSON array for params."
  (list :array (coerce raw 'list)))

(defun %bridge-rpc (method &key params (autostart nil) on-event)
  "Project-aware RPC entry point.

Resolves the project, optionally autostarts the daemon, sends METHOD
with PARAMS, and returns (values response project-root sock pid log).
RESPONSE is one of a parsed object, `:no-daemon', or `:io-error'.

If no project is found, returns (values nil nil nil nil nil) after
printing the appropriate user-visible error."
  (multiple-value-bind (project-root sock pid log)
      (%bridge-resolve-project)
    (unless project-root
      (return-from %bridge-rpc (values nil nil nil nil nil)))
    (let ((resp (%bridge-send-or-autostart sock pid project-root method
                                            :params params
                                            :autostart autostart
                                            :on-event on-event)))
      (values resp project-root sock pid log))))

(defun %bridge-render-error (resp)
  "Print a friendly error line; return non-zero rc."
  (cond
    ((null resp) 2)
    ((eq resp :no-daemon)
     (log-error "No daemon running and could not auto-start (try `clpm repl daemon --detach')")
     2)
    ((eq resp :io-error)
     (log-error "I/O error talking to daemon")
     2)
    (t
     (let* ((err (%bridge-err resp))
            (code (%bridge-field err "code"))
            (msg (%bridge-field err "message")))
       (format *error-output* "error: [~A] ~A~%" (or code "rpc") (or msg "(unspecified)"))
       1))))

(defun %bridge-emit-json (resp)
  "Print RESP as a single JSON line."
  (clpm.io.json:write-json resp *standard-output*)
  (terpri *standard-output*))

(defun %bridge-param-flag-name (flag)
  "Map a CLI flag name to a daemon JSON parameter name."
  (substitute #\_ #\- flag))

(defun %bridge-parse-call-value (text)
  "Parse TEXT as JSON when possible, otherwise treat it as a string.

This lets `call gc --full true' pass a boolean and
`call set-package --name CL-USER' pass a plain string without requiring
quotes around every non-JSON atom."
  (handler-case
      (clpm.io.json:read-json-from-string text)
    (error () text)))

(defun %bridge-object-alist-or-error (json label)
  "Return JSON object's alist, or signal a user-facing parse error."
  (cond
    ((null json) nil)
    ((and (consp json) (eq (car json) :object))
     (cadr json))
    (t
     (log-error "~A must be a JSON object" label)
     :error)))

(defun %bridge-put-param (alist name value)
  "Return ALIST with NAME bound to VALUE, preserving first-insertion order."
  (let ((cell (assoc name alist :test #'string=)))
    (cond
      (cell (setf (cdr cell) value) alist)
      (t (append alist (list (cons name value)))))))

(defparameter +bridge-reserved-call-params+ '("project_root" "token")
  "Protocol params owned by the client transport, not `repl call' users.")

(defun %bridge-reserved-call-param-p (name)
  (member name +bridge-reserved-call-params+ :test #'string=))

(defun %bridge-add-call-param (params name value)
  "Add one public `repl call' parameter, rejecting transport-owned fields."
  (cond
    ((%bridge-reserved-call-param-p name)
     (log-error "Reserved repl call parameter: ~A" name)
     (values params nil))
    (t
     (values (%bridge-put-param params name value) t))))

(defun %bridge-call-params-object (alist)
  "Build params for `call', preserving explicit JSON null values."
  (when alist
    (list :object alist)))

(defun %bridge-split-param-flag (arg)
  "Return (values NAME VALUE HAS-INLINE-VALUE) for a --name or --name=value arg."
  (let* ((body (subseq arg 2))
         (eq-pos (position #\= body)))
    (cond
      (eq-pos
       (values (subseq body 0 eq-pos)
               (subseq body (1+ eq-pos))
               t))
      (t
       (values body nil nil)))))

(defun %bridge-rejected-call-method-message (method)
  (cond
    ((string= method "eval")
     "Use `clpm repl eval FORM` instead of `clpm repl call eval`")
    ((string= method "shutdown")
     "Use `clpm repl daemon --stop` instead of `clpm repl call shutdown`")
    ((string= method "query-response")
     "query-response is a continuation message, not a repl call method")
    (t nil)))

(defun %bridge-call (args)
  "Generic typed method constructor for `clpm repl call METHOD ...'.

`call' addresses an existing REPL image. It never starts the daemon; image
lifecycle belongs to `repl daemon' and the ergonomic `repl eval' path."
  (let ((method (pop args))
        (params '()))
    (unless method
      (log-error "Usage: clpm repl call METHOD [--params-json JSON] [--PARAM VALUE]...")
      (return-from %bridge-call 1))
    (let ((message (%bridge-rejected-call-method-message method)))
      (when message
        (log-error "~A" message)
        (return-from %bridge-call 1)))
    (loop while args do
      (let ((arg (pop args)))
        (cond
          ((string= arg "--params-json")
           (let ((raw (pop args)))
             (unless (stringp raw)
               (log-error "Missing value for --params-json")
               (return-from %bridge-call 1))
             (let ((parsed (handler-case
                               (clpm.io.json:read-json-from-string raw)
                             (error (c)
                               (log-error "Invalid --params-json: ~A" c)
                               :error))))
               (when (eq parsed :error)
                 (return-from %bridge-call 1))
               (let ((alist (%bridge-object-alist-or-error
                             parsed "--params-json")))
                 (when (eq alist :error)
                   (return-from %bridge-call 1))
                 (dolist (cell alist)
                   (multiple-value-bind (next ok)
                       (%bridge-add-call-param params (car cell) (cdr cell))
                     (unless ok
                       (return-from %bridge-call 1))
                     (setf params next)))))))
          ((and (> (length arg) 2)
                (string= "--" (subseq arg 0 2)))
           (multiple-value-bind (name inline-value inline?)
               (%bridge-split-param-flag arg)
             (when (zerop (length name))
               (log-error "Bad empty parameter flag")
               (return-from %bridge-call 1))
             (let ((raw (if inline? inline-value (pop args))))
               (unless (stringp raw)
                 (log-error "Missing value for --~A" name)
                 (return-from %bridge-call 1))
               (multiple-value-bind (next ok)
                   (%bridge-add-call-param
                    params
                    (%bridge-param-flag-name name)
                    (%bridge-parse-call-value raw))
                 (unless ok
                   (return-from %bridge-call 1))
                 (setf params next)))))
          (t
           (log-error "Unexpected call argument: ~A" arg)
           (return-from %bridge-call 1)))))
    (multiple-value-bind (resp)
        (%bridge-rpc method
                     :params (%bridge-call-params-object params)
                     :on-event #'%bridge-emit-json)
      (cond
        ((or (null resp) (eq resp :no-daemon) (eq resp :io-error))
         (%bridge-render-error resp))
        (t
         (%bridge-emit-json resp)
         (if (%bridge-err resp) 1 0))))))

;;; ----------------------------------------------------------------------------
;;; Continuation-aware eval debugger.

(defun %bridge-debug-on-connection (conn opts form)
  "The continuation-aware debugger loop, run on an open CONNECTION."
  (let* ((eval-id 1001)
         (project-root (getf opts :project-root))
         (resolved nil)
         (kept nil)
         (params (%bridge-params-with-project-root
                  (%bridge-make-params
                   (list (cons "form" form)
                         (cons "package" (getf opts :package))
                         (cons "worker" (getf opts :worker))
                         (cons "debug" t)
                         (cons "break_on" (getf opts :break-on))
                         (cons "max_real_ms" (getf opts :timeout-ms))
                         (cons "handlers" (getf opts :handlers))))
                  project-root)))
    (labels ((project-params (raw)
               (%bridge-params-with-project-root raw project-root))
             (handle-stop (event)
               (let* ((cobj (%bridge-unwrap (%bridge-frame-field event "condition")))
                      (session (%bridge-frame-field event "session"))
                      (restarts (%bridge-array-items
                                 (%bridge-field cobj "restarts")))
                      (frames (%bridge-array-items
                               (%bridge-field cobj "backtrace"))))
                 (cond
                   ((and (getf opts :frame) (getf opts :frame-eval))
                    (clpm.repl:send-continuation-on-connection
                     conn eval-id "debug-eval-in-frame"
                     :params (project-params
                              (%bridge-make-params
                               (list (cons "frame" (getf opts :frame))
                                     (cons "form"  (getf opts :frame-eval)))))))
                   ((getf opts :restart)
                    (clpm.repl:send-continuation-on-connection
                     conn eval-id "debug-invoke-restart"
                     :params (project-params
                              (%bridge-make-params
                               (list (cons "name" (getf opts :restart))
                                     (cons "args"
                                           (and (getf opts :arg)
                                                (%bridge-string-list
                                                 (getf opts :arg))))))))
                    (setf resolved t))
                   (t
                    (cond
                      (*bridge-cli-json* (%bridge-emit-json event))
                      (t
                       (format *error-output* "~&!! debugger entered: ~A~%"
                               (or (%bridge-field cobj "message")
                                   (%bridge-field cobj "report")
                                   "(no condition)"))
                       (when session
                         (format *error-output* "session: ~A~%" session))
                       (when restarts
                         (format *error-output* "restarts (~D):~%" (length restarts))
                         (dolist (r restarts)
                           (%bridge-print-restart *error-output* (cadr r))))
                       (let ((user (%bridge-user-frames (list :array frames))))
                         (when user
                           (%bridge-print-frames *error-output* user)))))
                    (cond
                      ((getf opts :keep)
                       (setf kept t
                             resolved :kept)
                       :stop)
                      (t
                       (clpm.repl:send-continuation-on-connection
                        conn eval-id "debug-abort"
                        :params (project-params nil))
                       (setf resolved :aborted))))))))
      (let ((resp
              (clpm.repl:send-on-connection
               conn "eval"
               :id eval-id
               :params params
               :on-event
               (lambda (event)
                 (let ((ename (%bridge-frame-field event "event"))
                       (action nil))
                   (cond
                     ((and ename (string= ename "debugger-entered"))
                      (setf action (handle-stop event)))
                     ((and ename (string= ename "frame-eval-result"))
                      (cond
                        (*bridge-cli-json* (%bridge-emit-json event))
                        (t
                         (format t "frame ~A => ~A~%"
                                 (getf opts :frame)
                                 (or (%bridge-frame-field event "value")
                                     (%bridge-frame-field event "error_output")
                                     "?"))))
                      ;; After printing the frame result we always abort the
                      ;; session: stays single-shot.
                      (clpm.repl:send-continuation-on-connection
                       conn eval-id "debug-abort"
                       :params (project-params nil))
                      (setf resolved t)))
                   action)))))
        (cond
          ((eq resp :no-daemon) 2)
          ((eq resp :io-error)
           (cond
             (kept 3)
             (t
              (log-error "I/O error during debug session") 2)))
          (*bridge-cli-json*
           (%bridge-emit-json resp)
           (if (%bridge-err resp)
               (if (eq resolved :aborted) 3 1)
               0))
          (t
           (cond
             ((%bridge-err resp)
              (let* ((err (%bridge-err resp))
                     (code (%bridge-field err "code"))
                     (msg (%bridge-field err "message")))
                (format *error-output* "result: [~A] ~A~%"
                        (or code "rpc") (or msg "(unspecified)")))
              (if (eq resolved :aborted) 3 1))
             (t
              (let* ((obj (%bridge-obj resp))
                     (value (%bridge-field obj "value")))
                (when value (format t "=> ~A~%" value))
              0)))))))))

;;; ----------------------------------------------------------------------------
;;; Dispatcher.

(defun %bridge-help (args)
  "Print the small public repl CLI surface."
  (when args
    (log-error "Usage: clpm repl [daemon|eval|call] ...")
    (return-from %bridge-help 1))
  (format t "Usage:~%")
  (format t "  clpm repl daemon [--detach] [--no-load] [--status [--json]] [--stop]~%")
  (format t "  clpm repl eval FORM [--package P] [--worker W] [--debug] ...~%")
  (format t "  clpm repl call METHOD [--params-json JSON] [--PARAM VALUE]...~%~%")
  (format t "Use `clpm repl call methods` to list callable daemon RPCs.~%")
  (format t "Use `clpm repl call help --method gc` for a callable method schema.~%")
  (format t "Use `clpm help repl` for CLI details.~%")
  0)

(defun cmd-repl (&rest args)
  "Dispatcher for `clpm repl <subcommand>'.

See `clpm help repl' for the full surface."
  (let ((sub (pop args)))
    (cond
      ((or (null sub) (string= sub "--help") (string= sub "-h"))
       (%bridge-help args))
      ((string= sub "daemon") (%bridge-daemon args))
      ((string= sub "eval") (%bridge-eval args))
      ((string= sub "call") (%bridge-call args))
      (t
       (log-error "Unknown subcommand: ~A (expected daemon, eval, or call)" sub)
       1))))

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
        (log-info "Project not installed/activated (or out of date); running 'clpm deps sync'...")
        (let ((rc (uiop:with-current-directory (project-root)
                    (let ((*target-package* nil))
                      (cmd-install)))))
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

Precedence: CLI `--lisp` (*lisp*) > project :lisp > global config :defaults
:lisp > default :sbcl."
  (cond
    (*lisp*
     (clpm.lisp:parse-lisp-kind *lisp*))
    ((and project (clpm.project:project-lisp project))
     (clpm.lisp:parse-lisp-kind (clpm.project:project-lisp project)))
    (t
     (let* ((config (ignore-errors (clpm.config:read-config)))
            (default (and config (getf (clpm.config:config-defaults config) :lisp))))
       (if default
           (clpm.lisp:parse-lisp-kind default)
           :sbcl)))))

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

(defun %log-run-usage ()
  (log-error "Usage:")
  (log-error "  clpm run [-- <args...>]")
  (log-error "  clpm run exec -- <cmd...>")
  (log-error "  clpm run test")
  (log-error "  clpm run script <name> [-- <args...>]")
  (log-error "  clpm run scripts"))

(defun cmd-run (&rest args)
  "Dispatch project execution operations."
  (let ((sub (first args))
        (rest (rest args)))
    (cond
      ((or (null sub) (string= sub "--"))
       (apply #'cmd-run-entrypoint args))
      ((string= sub "repl")
       (log-error "`clpm run repl` is not public; use `clpm repl eval FORM` or `clpm repl daemon --detach`.")
       1)
      ((string= sub "exec")
       (apply #'cmd-exec rest))
      ((string= sub "test")
       (when rest
         (log-error "Usage: clpm run test")
         (return-from cmd-run 1))
       (cmd-test))
      ((string= sub "script")
       (apply #'cmd-scripts (cons "run" rest)))
      ((string= sub "scripts")
       (when rest
         (log-error "Usage: clpm run scripts")
         (return-from cmd-run 1))
       (cmd-scripts "list"))
      ((or (string= sub "help") (string= sub "--help"))
       (%log-run-usage)
       1)
      (t
       (log-error "Unknown run operation: ~A" sub)
       (%log-run-usage)
       1))))

(defun cmd-run-entrypoint (&rest args)
  "Run the project entrypoint defined in clpm.project :run."
  (unless (or (null args) (string= (first args) "--"))
    (log-error "Usage: clpm run [-- <args...>]")
    (return-from cmd-run-entrypoint 1))
  (multiple-value-bind (project-root manifest-path lock-path workspace-root _workspace-path)
      (find-effective-project-root)
    (declare (ignore lock-path _workspace-path))
    (unless manifest-path
      (when (null workspace-root)
        (log-no-project-found))
      (return-from cmd-run-entrypoint 1))
    (let* ((project (clpm.project:read-project-file manifest-path))
           (run (clpm.project:project-run project)))
      (unless run
        (log-error "No :run entry configured in clpm.project")
        (return-from cmd-run-entrypoint 1))
      (let ((system (getf run :system))
            (fn-spec (getf run :function)))
        (unless (and (stringp system) (stringp fn-spec))
          (log-error "Invalid :run entry: expected (:system <string> :function <string>)")
          (return-from cmd-run-entrypoint 1))
        (multiple-value-bind (config-path rc)
            (ensure-project-activated project-root)
          (unless (zerop rc)
            (return-from cmd-run-entrypoint rc))
          (let* ((run-args (if args (rest args) '()))
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

Usage: clpm run exec -- <cmd...>"
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
        (log-error "Usage: clpm run exec -- <cmd...>")
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
             (log-info "  clpm run scripts")
             (log-info "  clpm run script <name> [-- <args...>]")
             0)
            ((string= sub "list")
             (dolist (name (sort (mapcar (lambda (s) (getf s :name)) parsed) #'string<))
               (format t "~A~%" name))
             0)
            ((string= sub "run")
             (let ((name (second args)))
               (unless (and (stringp name) (plusp (length name)))
                 (log-error "Usage: clpm run script <name> [-- <args...>]")
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
             (log-error "Usage: clpm run <scripts|script> [args]")
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
  :package (:output \"dist/<name>\" :system \"<system>\" :function \"<package>::<fn>\")

Supports SBCL and CCL. SBCL produces a (sb-ext:save-lisp-and-die) image plus
an sh wrapper that adds --end-runtime-options so user args reach the program.
CCL produces a (ccl:save-application :prepend-kernel t) image written directly
to the output path; no wrapper is needed since CCL doesn't grab CLI flags."
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
           (pkg (clpm.project:project-package project))
           (kind (effective-lisp-kind project)))
      (case kind
        ((:sbcl :ccl) nil)
        (:ecl
         (log-error "Packaging on ECL is not yet implemented")
         (return-from cmd-package 1))
        (t
         (log-error "Packaging on ~A is not supported" kind)
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
                        (let ((*target-package* nil))
                          (cmd-install)))))
              (unless (zerop rc)
                (return-from cmd-package rc)))
            (setf lock-path (merge-pathnames "clpm.lock" project-root)))
          (unless (and lock-path (uiop:file-exists-p lock-path))
            (log-error "Missing clpm.lock - run 'clpm deps sync' first")
            (return-from cmd-package 1))
          (unless (uiop:file-exists-p config-path)
            (log-error "Missing activation config - run 'clpm deps sync' first")
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
                   ;; SBCL writes the real image to .bin and uses output-path
                   ;; as the wrapper; CCL writes the image directly to output-path.
                   (image-path (if (eq kind :sbcl)
                                   (uiop:ensure-pathname
                                    (format nil "~A.bin" (namestring output-path))
                                    :want-existing nil :want-file t)
                                   output-path))
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
                   (code-var (intern "CODE" "CL-USER"))
                   ;; CCL forms reference the CCL package, which does not
                   ;; exist while CLPM is compiled by SBCL. Resolve those
                   ;; symbols at run time inside the spawned CCL subprocess
                   ;; via uiop:find-symbol*.
                   (exit-form
                     (case kind
                       (:sbcl `(sb-ext:exit :code ,code-var))
                       (:ccl `(funcall (uiop:find-symbol* "QUIT" "CCL") ,code-var))))
                   (defun-form
                     `(defun ,main-sym ()
                        (let* ((,args-var (uiop:command-line-arguments))
                               (,result-var (uiop:symbol-call ,pkg-key ,fn-key ,args-var))
                               (,code-var (if (integerp ,result-var) ,result-var 0)))
                          ,exit-form)))
                   (save-form
                     (case kind
                       (:sbcl
                        `(sb-ext:save-lisp-and-die ,(namestring image-path)
                                                   :toplevel ',main-sym
                                                   :executable t
                                                   :compression t))
                       (:ccl
                        `(funcall (uiop:find-symbol* "SAVE-APPLICATION" "CCL")
                                  ,(namestring image-path)
                                  :toplevel-function ',main-sym
                                  :prepend-kernel t
                                  :purify t))))
                   (defun-str
                     (with-standard-io-syntax
                       (let ((*package* (find-package "CL-USER")))
                         (prin1-to-string defun-form))))
                   (save-str
                     (with-standard-io-syntax
                       (let ((*package* (find-package "CL-USER")))
                         (prin1-to-string save-form))))
                   (deps (project-dependency-system-ids project '(:depends)))
                   (load-eval-forms
                     (append (lisp-load-systems-eval-forms deps)
                             (list (format nil "(asdf:load-system ~S)" system)
                                   defun-str
                                   save-str)))
                   (subprocess-argv
                     (clpm.lisp:lisp-run-argv
                      kind
                      :load-files (list (namestring config-path))
                      :eval-forms load-eval-forms
                      :noinform t
                      :noninteractive t
                      :disable-debugger t)))
              (ensure-directories-exist output-path)

              (log-info "Packaging ~A -> ~A (lisp: ~A)"
                        system (namestring output-path) kind)
              (multiple-value-bind (out err rc)
                  (clpm.platform:run-program subprocess-argv
                                             :directory project-root
                                             :output :interactive
                                             :error-output :interactive
                                             :timeout 600000)
                (declare (ignore out err))
                (unless (zerop rc)
                  (log-error "Packaging failed (exit code ~D)" rc)
                  (return-from cmd-package rc)))

              (when (eq kind :sbcl)
                ;; SBCL's runtime parses --core etc.; wrapper inserts the
                ;; --end-runtime-options sentinel so user args pass through.
                (write-sbcl-wrapper output-path (file-namestring image-path)))
              (when (eq kind :ccl)
                ;; CCL writes the image directly to output-path; mark it
                ;; executable for symmetry with the SBCL wrapper.
                (unless (chmod-755 output-path)
                  (log-error "Failed to mark CCL image executable: ~A"
                             (namestring output-path))
                  (return-from cmd-package 1)))

              (clpm.io.sexp:write-canonical-sexp-to-file
               `(:package-meta
                 :lock-sha256 ,lock-sha256
                 :lisp-kind ,kind
                 :lisp-version ,(case kind
                                  (:sbcl (clpm.platform:sbcl-version))
                                  (t (clpm.lisp:lisp-version kind)))
                 :platform ,(clpm.platform:platform-triple))
               meta-path)
              (log-info "Wrote package metadata: ~A" (namestring meta-path))
              0))))))))

;;; clean command

(defun cmd-clean (&rest args)
  "Clean project-local outputs.

Default: remove the project's .clpm/ activation cache.
  --dist   also remove the project's dist/ directory.
  --store  untrack this project from projects.sxp and then GC the
           content-addressed store. Entries still reachable from other
           registered projects are kept (GC roots do their job)."
  (let ((clean-dist nil)
        (clean-store nil)
        (rest args))
    (loop while rest do
      (let ((arg (pop rest)))
        (cond
          ((string= arg "--dist")
           (setf clean-dist t))
          ((string= arg "--store")
           (setf clean-store t))
          (t
           (log-error "Unknown option: ~A" arg)
           (log-error "Usage: clpm store clean [--dist] [--store]")
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
          (uiop:delete-directory-tree dist-dir :validate t)))
      (when clean-store
        ;; Untrack this project as a GC root, then GC. Anything still reachable
        ;; from another registered project is preserved.
        (handler-case
            (clpm.store:remove-project-index-root project-root)
          (error (c)
            (log-error "Failed to untrack project: ~A" c)
            (return-from cmd-clean 1)))
        (log-info "Untracked project from GC roots: ~A" (namestring project-root))
        (let ((deleted (clpm.store:gc-store)))
          (log-info "Removed ~D store entr~:@P" (length (or deleted '())))))
      0)))

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

(defun cmd-store (&rest args)
  "Dispatch store-resource cleanup operations."
  (let ((sub (first args))
        (rest (rest args)))
    (labels ((usage ()
               (log-error "Usage:")
               (log-error "  clpm store clean [--dist] [--store]")
               (log-error "  clpm store gc [--dry-run]")
               1))
      (cond
        ((or (null sub) (string= sub "help") (string= sub "--help"))
         (usage))
        ((string= sub "clean")
         (apply #'cmd-clean rest))
        ((string= sub "gc")
         (let ((dry-run nil))
           (dolist (arg rest)
             (cond
               ((string= arg "--dry-run") (setf dry-run t))
               (t
                (log-error "Unknown option: ~A" arg)
                (log-error "Usage: clpm store gc [--dry-run]")
                (return-from cmd-store 1))))
           (cmd-gc :dry-run dry-run)))
        (t
         (log-error "Unknown store subcommand: ~A" sub)
         (usage))))))

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
             (log-error "  clpm project workspace init [--dir <path>]")
             (log-error "  clpm project workspace add <member> [--dir <path>]")
             (log-error "  clpm project workspace remove <member> [--dir <path>]")
             (log-error "  clpm project workspace list [--dir <path>]")
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
         (log-info "  clpm project workspace init [--dir <path>]")
         (log-info "  clpm project workspace add <member> [--dir <path>]")
         (log-info "  clpm project workspace remove <member> [--dir <path>]")
         (log-info "  clpm project workspace list [--dir <path>]")
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
               (usage-error "Usage: clpm project workspace add <member> [--dir <path>]"))
             (let ((norm (%normalize-workspace-member-arg member)))
               (unless norm
                 (return-from cmd-workspace 1))
               (multiple-value-bind (root ws-path)
                   (workspace-root-and-path dir)
                 (unless (and root ws-path (uiop:file-exists-p ws-path))
                   (usage-error "No clpm.workspace found (run: clpm project workspace init)"))
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
        ((string= sub "remove")
         (multiple-value-bind (dir extra)
             (parse-dir-arg rest)
           (let ((member (first extra))
                 (extra (rest extra)))
             (when (or (null member) extra)
               (usage-error "Usage: clpm project workspace remove <member> [--dir <path>]"))
             (let ((norm (%normalize-workspace-member-arg member)))
               (unless norm
                 (return-from cmd-workspace 1))
               (multiple-value-bind (root ws-path)
                   (workspace-root-and-path dir)
                 (declare (ignore root))
                 (unless (and ws-path (uiop:file-exists-p ws-path))
                   (usage-error "No clpm.workspace found (run: clpm project workspace init)"))
                 (let* ((ws (clpm.workspace:read-workspace-file ws-path))
                        (current (or (clpm.workspace:workspace-members ws) '())))
                   (unless (member norm current :test #'string=)
                     (log-error "Workspace member ~A not found" norm)
                     (when current
                       (log-error "Current members: ~{~A~^, ~}" current))
                     (return-from cmd-workspace 1))
                   (setf (clpm.workspace:workspace-members ws)
                         (sort (remove norm current :test #'string= :count 1)
                               #'string<))
                   (clpm.workspace:write-workspace-file ws ws-path)
                   (log-info "Removed member: ~A" norm)
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
               (usage-error "No clpm.workspace found (run: clpm project workspace init)"))
             (let* ((ws (clpm.workspace:read-workspace-file ws-path))
                    (members (sort (copy-list (or (clpm.workspace:workspace-members ws) '()))
                                   #'string<)))
               (dolist (m members)
                 (format t "~A~%" m))
               0))))
        (t
         (usage-error "Unknown subcommand: ~A" sub))))))

;;; registry command

(defun %hex-string-p (s &key length)
  (and (stringp s)
       (or (null length) (= (length s) length))
       (every (lambda (c) (digit-char-p c 16)) s)))

(defun %ed25519-trust-string-p (trust)
  (when (stringp trust)
    (let ((parsed (clpm.crypto.ed25519:parse-key-id trust)))
      (and (string= (car parsed) "ed25519")
           (search ":" trust)
           (%key-id-valid-p (cdr parsed))))))

(defun %sha256-trust-string-p (trust)
  (and (stringp trust)
       (let ((prefix "sha256:"))
         (and (<= (length prefix) (length trust))
              (string= prefix trust :end2 (length prefix))
              (%hex-string-p (subseq trust (length prefix))
                             :length 64)))))

(defun %valid-registry-trust-p (kind trust)
  (case kind
    (:git (%ed25519-trust-string-p trust))
    (:quicklisp
     (or (and (stringp trust) (string-equal trust "tofu"))
         (%sha256-trust-string-p trust)))
    (t nil)))

(defun %registry-trust-description (kind)
  (case kind
    (:git "ed25519:<key-id>")
    (:quicklisp "tofu or sha256:<64-hex-digest>")
    (t "a supported trust string")))

(defun cmd-registry (&rest args)
  "Manage global registries in config.sxp."
  (let ((subcommand (first args))
        (rest (rest args)))
    (cond
      ((or (null subcommand) (string= subcommand "help"))
       (log-error "Usage: clpm registry <list|add|update|trust|init|key|publish> [options]")
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
	         (loop while rest do
	           (let ((arg (pop rest)))
	             (cond
	               ((string= arg "--name") (setf name (pop rest)))
	               ((string= arg "--url") (setf url (pop rest)))
	               ((string= arg "--trust") (setf trust (pop rest)))
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
	             (setf trust "tofu")))

	         (case kind
	           (:git
	            (unless (and name url trust)
	              (log-error "Missing required options: --name, --url, --trust")
	              (return-from cmd-registry 1)))
	           (:quicklisp
	            (unless (and name url trust)
	              (log-error "Missing required options: --name, --url")
	              (return-from cmd-registry 1)))
	           (t
	            (log-error "Unknown registry kind: ~S" kind)
	            (return-from cmd-registry 1)))
	         (unless (%valid-registry-trust-p kind trust)
	           (log-error "Invalid trust for ~A registry ~A: ~A (expected ~A)"
	                      (string-downcase (symbol-name kind))
	                      (or name "<unnamed>")
	                      trust
	                      (%registry-trust-description kind))
	           (return-from cmd-registry 1))

	         (clpm.config:update-config
	          (lambda (cfg)
	            (let* ((regs (clpm.config:config-registries cfg))
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
	              cfg)))
	         0))

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
       (let* ((names '())
              (cfg (clpm.config:read-config))
              (refs (clpm.config:config-registries cfg)))
         (loop while rest do
           (let ((arg (pop rest)))
             (cond
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
                               :quicklisp-systems-sha256
                               (clpm.project:registry-ref-quicklisp-systems-sha256 ref)
                               :quicklisp-releases-sha256
                               (clpm.project:registry-ref-quicklisp-releases-sha256 ref)
                               :kind (clpm.project:registry-ref-kind ref))))
                     (clpm.registry:update-registry reg))
                 (error (c)
                   (log-error "Failed to update registry ~A: ~A" name c)
                   (return-from cmd-registry 1))))))
         0))

      ((string= subcommand "key")
       (apply #'cmd-keys rest))

      ((string= subcommand "publish")
       (apply #'cmd-publish rest))

      ((string= subcommand "trust")
       (let ((action (first rest))
             (rest (rest rest)))
         (labels ((usage-error (fmt &rest fmt-args)
                    (apply #'log-error fmt fmt-args)
                    (log-error "Usage: clpm registry trust <list|set|refresh> [args]")
                    (return-from cmd-registry 1))
                  (kind->string (k)
                    (string-downcase (symbol-name k))))
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
                (let ((found nil))
                  (clpm.config:update-config
                   (lambda (cfg)
                     (let ((ref (find name (clpm.config:config-registries cfg)
                                      :key #'clpm.project:registry-ref-name
                                      :test #'string=)))
                       (when ref
                         (let ((kind (clpm.project:registry-ref-kind ref)))
                           (unless (%valid-registry-trust-p kind trust-raw)
                             (usage-error "Invalid trust for ~A registry ~A: ~A (expected ~A)"
                                          (kind->string kind)
                                          name
                                          trust-raw
                                          (%registry-trust-description kind)))
                           (setf (clpm.project:registry-ref-trust ref) trust-raw))
                         (setf found t))
                       cfg)))
                  (unless found
                    (usage-error "Unknown registry: ~A" name))
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
                                :quicklisp-systems-sha256
                                (clpm.project:registry-ref-quicklisp-systems-sha256 ref)
                                :quicklisp-releases-sha256
                                (clpm.project:registry-ref-quicklisp-releases-sha256 ref)
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

(defun %keys-dir-or-default (override)
  "Return the keys directory to operate on. If OVERRIDE is a non-empty string,
expand it; otherwise return `clpm.platform:keys-dir`."
  (if (and (stringp override) (plusp (length override)))
      (uiop:ensure-directory-pathname (clpm.platform:expand-path override))
      (uiop:ensure-directory-pathname (clpm.platform:keys-dir))))

(defun %read-pub-key-hex (path)
  "Read a public key file (32-byte Ed25519 hex). Returns the trimmed hex
string, or NIL if the file is not a valid 64-char hex line."
  (handler-case
      (with-open-file (s path :direction :input :external-format :utf-8)
        (let* ((line (read-line s nil nil))
               (trim (and line (string-trim '(#\Space #\Tab #\Return #\Newline) line))))
          (and (stringp trim) (= 64 (length trim))
               (every (lambda (c)
                        (find c "0123456789abcdefABCDEF" :test #'char=))
                      trim)
               trim)))
    (error () nil)))

(defun %pub-key-fingerprint (path)
  "Return a short SHA-256 fingerprint (first 16 hex chars) for the public key
in PATH, or NIL if the file isn't a recognizable Ed25519 public key."
  (let ((hex (%read-pub-key-hex path)))
    (when hex
      (let ((bytes (clpm.crypto.sha256:hex-to-bytes hex)))
        (subseq (clpm.crypto.sha256:bytes-to-hex
                 (clpm.crypto.sha256:sha256 bytes))
                0 16)))))

(defun cmd-keys (&rest args)
  "Manage Ed25519 keys used for registry signing."
  (let ((subcommand (first args))
        (rest (rest args)))
    (labels ((usage-error (fmt &rest fmt-args)
               (apply #'log-error fmt fmt-args)
               (log-error "Usage:")
               (log-error "  clpm registry key generate --out <dir> --id <id>")
               (log-error "  clpm registry key list [--keys-dir <dir>]")
               (log-error "  clpm registry key import --pub <path> [--id <id>] [--keys-dir <dir>]")
               (log-error "  clpm registry key verify --pub <path> --file <path> --sig <path>")
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
        ((string= subcommand "list")
         (let ((keys-dir nil))
           (loop while rest do
             (let ((arg (pop rest)))
               (cond
                 ((string= arg "--keys-dir") (setf keys-dir (pop rest)))
                 (t (usage-error "Unknown option: ~A" arg)))))
           (let* ((dir (%keys-dir-or-default keys-dir)))
             (unless (uiop:directory-exists-p dir)
               (log-info "No keys directory: ~A" (namestring dir))
               (return-from cmd-keys 0))
             (let* ((entries (sort (or (directory (merge-pathnames "*.pub" dir)) '())
                                   #'string< :key #'namestring))
                    (any nil))
               (dolist (path entries)
                 (let* ((name (pathname-name path))
                        (fp (%pub-key-fingerprint path)))
                   (cond
                     (fp
                      (setf any t)
                      (format t "~A~Cfingerprint:~A~%" name #\Tab fp))
                     (t
                      (log-error "Skipping unreadable key: ~A" (namestring path))))))
               (unless any
                 (log-info "No keys found in: ~A" (namestring dir)))
               0))))
        ((string= subcommand "import")
         (let ((pub-path nil)
               (id nil)
               (keys-dir nil))
           (loop while rest do
             (let ((arg (pop rest)))
               (cond
                 ((string= arg "--pub") (setf pub-path (pop rest)))
                 ((string= arg "--id") (setf id (pop rest)))
                 ((string= arg "--keys-dir") (setf keys-dir (pop rest)))
                 (t (usage-error "Unknown option: ~A" arg)))))
           (unless (and (stringp pub-path) (plusp (length pub-path)))
             (usage-error "Missing --pub <path>"))
           (let ((expanded (uiop:ensure-pathname
                            (clpm.platform:expand-path pub-path)
                            :want-existing nil :want-file t)))
             (unless (uiop:file-exists-p expanded)
               (log-error "Public key not found: ~A" (namestring expanded))
               (return-from cmd-keys 1))
             (unless (%read-pub-key-hex expanded)
               (log-error "Not a valid Ed25519 public key (expected 32-byte hex on one line): ~A"
                          (namestring expanded))
               (return-from cmd-keys 1))
             (let* ((source-id (or id (pathname-name expanded))))
               (unless (%key-id-valid-p source-id)
                 (log-error "Invalid key id (use [A-Za-z0-9._-]+): ~S" source-id)
                 (return-from cmd-keys 1))
               (let* ((dest-dir (%keys-dir-or-default keys-dir))
                      (dest (merge-pathnames (format nil "~A.pub" source-id) dest-dir)))
                 (ensure-directories-exist dest-dir)
                 (when (uiop:file-exists-p dest)
                   (log-error "Key already exists: ~A" (namestring dest))
                   (return-from cmd-keys 1))
                 (with-open-file (in expanded :direction :input
                                              :external-format :utf-8)
                   (with-open-file (out dest :direction :output
                                             :if-exists :error
                                             :external-format :utf-8)
                     (let ((buf (make-string 4096)))
                       (loop for n = (read-sequence buf in)
                             while (plusp n)
                             do (write-sequence buf out :end n)))))
                 (%chmod-if-available dest "644")
                 (log-info "Imported public key: ~A" (namestring dest))
                 0)))))
        ((string= subcommand "verify")
         (let ((pub-path nil)
               (file-path nil)
               (sig-path nil))
           (loop while rest do
             (let ((arg (pop rest)))
               (cond
                 ((string= arg "--pub") (setf pub-path (pop rest)))
                 ((string= arg "--file") (setf file-path (pop rest)))
                 ((string= arg "--sig") (setf sig-path (pop rest)))
                 (t (usage-error "Unknown option: ~A" arg)))))
           (unless (and (stringp pub-path) (plusp (length pub-path)))
             (usage-error "Missing --pub <path>"))
           (unless (and (stringp file-path) (plusp (length file-path)))
             (usage-error "Missing --file <path>"))
           (unless (and (stringp sig-path) (plusp (length sig-path)))
             (usage-error "Missing --sig <path>"))
           (let ((pp (uiop:ensure-pathname (clpm.platform:expand-path pub-path)
                                           :want-existing nil :want-file t))
                 (fp (uiop:ensure-pathname (clpm.platform:expand-path file-path)
                                           :want-existing nil :want-file t))
                 (sp (uiop:ensure-pathname (clpm.platform:expand-path sig-path)
                                           :want-existing nil :want-file t)))
             (dolist (probe (list (list pp "--pub")
                                  (list fp "--file")
                                  (list sp "--sig")))
               (destructuring-bind (path label) probe
                 (unless (uiop:file-exists-p path)
                   (log-error "~A not found: ~A" label (namestring path))
                   (return-from cmd-keys 1))))
             (handler-case
                 (let ((ok (clpm.crypto.ed25519:verify-file-signature fp sp pp)))
                   (cond
                     (ok
                      (log-info "verify: OK")
                      0)
                     (t
                      (log-error "verify: signature does NOT match")
                      1)))
               (error (c)
                 (log-error "verify failed: ~A" c)
                 1)))))
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
        (tarball-out nil))
    (labels ((usage-error (fmt &rest fmt-args)
               (apply #'log-error fmt fmt-args)
               (log-error "Usage: clpm registry publish --registry <dir> --key-id <id> --keys-dir <dir> --tarball-url <url> [--tarball-out <path>] [--project <dir>]")
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
               (log-error "Usage: clpm deps audit [--json]")
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
          (log-error "No clpm.lock found (run: clpm deps sync --to lock or clpm deps sync)")
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

(defun %sbom-xml-escape (s)
  "Escape XML special characters in S for use as character data or an attribute value."
  (with-output-to-string (out)
    (loop for c across s do
      (case c
        (#\& (write-string "&amp;" out))
        (#\< (write-string "&lt;" out))
        (#\> (write-string "&gt;" out))
        (#\" (write-string "&quot;" out))
        (#\' (write-string "&apos;" out))
        (t (write-char c out))))))

(defun %sbom-spdx-id-sanitize (s)
  "Sanitize S so it can appear in an SPDX identifier (alphanumeric, dot, dash)."
  (with-output-to-string (out)
    (loop for c across s do
      (if (or (alphanumericp c) (char= c #\.) (char= c #\-))
          (write-char c out)
          (write-char #\- out)))))

(defun %sbom-collect-components (lock registries)
  "Return a deterministically sorted list of plists for SBOM emission.

Each plist contains :name :version :sha256 :sha1 :url :kind :commit :license."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (locked (clpm.project:lockfile-resolved lock))
      (let* ((release (clpm.project:locked-system-release locked))
             (name (and release (clpm.project:locked-release-name release)))
             (version (and release (clpm.project:locked-release-version release)))
             (src (and release (clpm.project:locked-release-source release)))
             (sha256 (and release (clpm.project:locked-release-artifact-sha256 release)))
             (sha1 (and src (clpm.project:locked-source-sha1 src)))
             (url (and src (clpm.project:locked-source-url src)))
             (kind (and src (clpm.project:locked-source-kind src)))
             (commit (and src (clpm.project:locked-source-commit src))))
        (when (and (stringp name) (plusp (length name))
                   (stringp version) (plusp (length version)))
          (setf (gethash (cons name version) ht)
                (list :name name :version version
                      :sha256 sha256 :sha1 sha1
                      :url url :kind kind :commit commit)))))
    (let ((sorted-keys
            (sort (loop for k being the hash-keys of ht collect k)
                  (lambda (a b)
                    (cond
                      ((string< (car a) (car b)) t)
                      ((string> (car a) (car b)) nil)
                      (t (string< (cdr a) (cdr b))))))))
      (flet ((find-license (pkg ver)
               (block found
                 (dolist (reg registries)
                   (let ((meta (ignore-errors
                                (clpm.registry:get-release-metadata reg pkg ver))))
                     (when meta
                       (let ((license (clpm.registry:release-metadata-license meta)))
                         (when (and (stringp license) (plusp (length license)))
                           (return-from found license))))))
                 nil)))
        (mapcar (lambda (k)
                  (let ((info (gethash k ht)))
                    (append info
                            (list :license (find-license (car k) (cdr k))))))
                sorted-keys)))))

(defun %sbom-emit-cyclonedx-json (components stream)
  "Write COMPONENTS as a CycloneDX 1.5 JSON BOM to STREAM."
  (labels ((safe-license (license)
             (list :object
                   (list (cons "license"
                               (list :object
                                     (list (cons "id" license)))))))
           (component->json (info)
             (let* ((pkg (getf info :name))
                    (ver (getf info :version))
                    (sha256 (getf info :sha256))
                    (sha1 (getf info :sha1))
                    (license (getf info :license))
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
                   (setf entries
                         (append entries
                                 (list (cons "hashes" (list :array hashes))))))
                 (when (and (stringp license) (plusp (length license)))
                   (setf entries
                         (append entries
                                 (list (cons "licenses"
                                             (list :array
                                                   (list (safe-license license))))))))
                 (list :object entries)))))
    (let* ((components-json (mapcar #'component->json components))
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
      (clpm.io.json:write-json bom stream)
      (terpri stream))))

(defun %sbom-emit-cyclonedx-xml (components stream)
  "Write COMPONENTS as a CycloneDX 1.5 XML BOM to STREAM."
  (flet ((esc (s) (%sbom-xml-escape s)))
    (format stream "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
    (format stream "<bom xmlns=\"http://cyclonedx.org/schema/bom/1.5\" version=\"1\">~%")
    (format stream "  <metadata>~%")
    (format stream "    <tools>~%")
    (format stream "      <tool>~%")
    (format stream "        <name>clpm</name>~%")
    (format stream "        <version>0.1.0</version>~%")
    (format stream "      </tool>~%")
    (format stream "    </tools>~%")
    (format stream "  </metadata>~%")
    (format stream "  <components>~%")
    (dolist (info components)
      (let ((pkg (getf info :name))
            (ver (getf info :version))
            (sha256 (getf info :sha256))
            (sha1 (getf info :sha1))
            (license (getf info :license)))
        (format stream "    <component type=\"library\">~%")
        (format stream "      <name>~A</name>~%" (esc pkg))
        (format stream "      <version>~A</version>~%" (esc ver))
        (format stream "      <purl>pkg:cl/~A@~A</purl>~%" (esc pkg) (esc ver))
        (when (or (and (stringp sha256) (plusp (length sha256)))
                  (and (stringp sha1) (plusp (length sha1))))
          (format stream "      <hashes>~%")
          (when (and (stringp sha256) (plusp (length sha256)))
            (format stream "        <hash alg=\"SHA-256\">~A</hash>~%" (esc sha256)))
          (when (and (stringp sha1) (plusp (length sha1)))
            (format stream "        <hash alg=\"SHA-1\">~A</hash>~%" (esc sha1)))
          (format stream "      </hashes>~%"))
        (when (and (stringp license) (plusp (length license)))
          (format stream "      <licenses>~%")
          (format stream "        <license><id>~A</id></license>~%" (esc license))
          (format stream "      </licenses>~%"))
        (format stream "    </component>~%")))
    (format stream "  </components>~%")
    (format stream "</bom>~%")))

(defun %sbom-emit-spdx-json (components stream &key project-name generated-at)
  "Write COMPONENTS as an SPDX 2.3 JSON document to STREAM."
  (let* ((doc-name (or (and (stringp project-name) (plusp (length project-name))
                            project-name)
                       "clpm-project"))
         (created (or (and (stringp generated-at) (plusp (length generated-at))
                           generated-at)
                      "1970-01-01T00:00:00Z"))
         (namespace (format nil "https://clpm.local/spdx/~A-~A"
                            (%sbom-spdx-id-sanitize doc-name)
                            (%sbom-spdx-id-sanitize created))))
    (labels ((checksum->json (alg value)
               (list :object
                     (list (cons "algorithm" alg)
                           (cons "checksumValue" value))))
             (package->json (info)
               (let* ((pkg (getf info :name))
                      (ver (getf info :version))
                      (sha256 (getf info :sha256))
                      (sha1 (getf info :sha1))
                      (url (getf info :url))
                      (license (getf info :license))
                      (kind (getf info :kind))
                      (commit (getf info :commit))
                      (spdx-id
                        (format nil "SPDXRef-Package-~A-~A"
                                (%sbom-spdx-id-sanitize pkg)
                                (%sbom-spdx-id-sanitize ver)))
                      (download
                        (cond
                          ((and (eq kind :git) (stringp url) (plusp (length url))
                                (stringp commit) (plusp (length commit)))
                           (format nil "git+~A@~A" url commit))
                          ((and (stringp url) (plusp (length url))) url)
                          (t "NOASSERTION")))
                      (checksums '()))
                 (when (and (stringp sha256) (plusp (length sha256)))
                   (push (checksum->json "SHA256" sha256) checksums))
                 (when (and (stringp sha1) (plusp (length sha1)))
                   (push (checksum->json "SHA1" sha1) checksums))
                 (setf checksums (nreverse checksums))
                 (let ((entries
                         (list (cons "SPDXID" spdx-id)
                               (cons "name" pkg)
                               (cons "versionInfo" ver)
                               (cons "downloadLocation" download)
                               (cons "filesAnalyzed" :false))))
                   (when checksums
                     (setf entries
                           (append entries
                                   (list (cons "checksums" (list :array checksums))))))
                   (when (and (stringp license) (plusp (length license)))
                     (setf entries
                           (append entries
                                   (list (cons "licenseConcluded" license)
                                         (cons "licenseDeclared" license)))))
                   (list :object entries)))))
      (let ((doc
              (list :object
                    (list (cons "spdxVersion" "SPDX-2.3")
                          (cons "dataLicense" "CC0-1.0")
                          (cons "SPDXID" "SPDXRef-DOCUMENT")
                          (cons "name" doc-name)
                          (cons "documentNamespace" namespace)
                          (cons "creationInfo"
                                (list :object
                                      (list (cons "created" created)
                                            (cons "creators"
                                                  (list :array
                                                        (list "Tool: clpm-0.1.0"))))))
                          (cons "packages"
                                (list :array (mapcar #'package->json components)))))))
        (clpm.io.json:write-json doc stream)
        (terpri stream)))))

(defvar *sbom-supported-formats*
  '("cyclonedx-json" "cyclonedx-xml" "spdx-json"))

(defun cmd-sbom (&rest args)
  "Generate a software bill of materials (SBOM) from the current lockfile."
  (let ((format nil)
        (output nil))
    (labels ((usage-error (fmt &rest fmt-args)
               (apply #'log-error fmt fmt-args)
               (log-error "Usage: clpm deps sbom --format <~{~A~^|~}> [--out <path>]"
                          *sbom-supported-formats*)
               (return-from cmd-sbom 1))
             (nonempty-string (s)
               (and (stringp s) (plusp (length s)))))
      ;; Parse args.
      (loop while args do
        (let ((arg (pop args)))
          (cond
            ((string= arg "--out")
             (setf output (pop args))
             (unless (nonempty-string output)
               (usage-error "Missing value for ~A" arg)))
            ((string= arg "--format")
             (setf format (pop args))
             (unless (nonempty-string format)
               (usage-error "Missing value for --format")))
            (t
             (usage-error "Unknown option: ~A" arg)))))

      (unless (and (stringp format)
                   (member format *sbom-supported-formats* :test #'string=))
        (usage-error "Unsupported --format (supported: ~{~A~^, ~}): ~S"
                     *sbom-supported-formats* format))

      (multiple-value-bind (project-root manifest-path lock-path workspace-root _workspace-path)
          (find-effective-project-root)
        (declare (ignore project-root _workspace-path))
        (unless manifest-path
          (when (null workspace-root)
            (log-no-project-found))
          (return-from cmd-sbom 1))
        (unless lock-path
          (log-error "No clpm.lock found (run: clpm deps sync --to lock or clpm deps sync)")
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

          (let* ((components (%sbom-collect-components lock registries))
                 (project-name (clpm.project:lockfile-project-name lock))
                 (generated-at (clpm.project:lockfile-generated-at lock)))
            (labels ((write-to-stream (stream)
                       (cond
                         ((string= format "cyclonedx-json")
                          (%sbom-emit-cyclonedx-json components stream))
                         ((string= format "cyclonedx-xml")
                          (%sbom-emit-cyclonedx-xml components stream))
                         ((string= format "spdx-json")
                          (%sbom-emit-spdx-json components stream
                                                :project-name project-name
                                                :generated-at generated-at)))))
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
                 0)))))))))

;;; skill command

(defparameter +agent-skill-markdown-lines+
  '("---"
    "name: clpm"
    "description: Use the Common Lisp Package Manager (CLPM) to work with project manifests, dependency resolution, installs, registries, scripts, packaging, and the repl persistent Lisp image."
    "---"
    ""
    "# CLPM Agent Skill"
    ""
    "Use this skill when you are working in a Common Lisp repository that has a `clpm.project`, `clpm.workspace`, or `clpm.lock`, or when the user asks you to add, resolve, fetch, build, test, package, audit, or inspect CLPM-managed dependencies."
    ""
    "## Mental Model"
    ""
    "- `clpm.project` is the source manifest. Edit it through `clpm` commands when possible."
    "- `clpm.workspace` groups project members. Pass scoped `-p <member>` / `--package <member>` before project-scoped commands when a workspace root has multiple members."
    "- `clpm.lock` records resolved releases and should be regenerated by CLPM, not patched by hand."
    "- The store is content-addressed and shared. Prefer `clpm store clean` / `clpm store gc` over manual deletion."
    "- Registry trust is part of correctness. `--insecure` is accepted only on registry-loading commands, and only for explicit debugging."
    "- Scoped options must appear before the command token. Command-local options stay after the command and are documented in `clpm help <command>`."
    ""
    "## First Moves"
    ""
    "1. Run `clpm doctor` when the environment is suspect."
    "2. Run `clpm help <command>` before using an unfamiliar command."
    "3. In a project, run `clpm deps sync` to resolve, fetch, build, and activate dependencies."
    "4. Use `clpm deps tree` and `clpm deps why <system>` to understand dependency shape before changing it."
    ""
    "## Common Workflows"
    ""
    "### Create or Initialize Projects"
    ""
    "```sh"
    "clpm project new my-app --bin"
    "clpm project new my-lib --lib"
    "clpm project init my-existing-project"
    "```"
    ""
    "### Manage Dependencies"
    ""
    "```sh"
    "clpm deps add alexandria"
    "clpm deps add alexandria bordeaux-threads"
    "clpm deps add alexandria@^1.4.0"
    "clpm deps add --dev fiveam"
    "clpm deps remove alexandria"
    "clpm deps update"
    "clpm deps update alexandria"
    "```"
    ""
    "Prefer `clpm deps add` / `clpm deps remove` over manually editing dependency lists. After dependency changes, run `clpm deps sync` and then the relevant tests."
    ""
    "### Resolve, Fetch, Build"
    ""
    "```sh"
    "clpm deps sync --to lock"
    "clpm deps sync --to source"
    "clpm deps sync --to build"
    "clpm deps sync"
    "```"
    ""
    "`clpm deps sync` is the normal all-in-one path. Use `--to` when debugging one stage."
    ""
    "### Run Code"
    ""
    "```sh"
    "clpm run"
    "clpm run exec -- sbcl --script scripts/check.lisp"
    "clpm run test"
    "clpm run scripts"
    "clpm run script <name> -- <args>"
    "```"
    ""
    "Use `clpm run exec -- <cmd>` when a non-CLPM command needs the activated project environment."
    ""
    "### Inspect Dependencies"
    ""
    "```sh"
    "clpm deps search split-sequence"
    "clpm deps info split-sequence"
    "clpm deps tree --depth 3"
    "clpm deps why alexandria"
    "clpm deps audit"
    "clpm deps sbom --format cyclonedx-json --out sbom.json"
    "```"
    ""
    "Use `--json` on commands that support it when you need machine-readable output."
    ""
    "### Registries and Trust"
    ""
    "```sh"
    "clpm registry list"
    "clpm registry update"
    "clpm registry add --name main --url <git-url> --trust ed25519:<key-id>"
    "clpm registry key list"
    "clpm registry key import --pub registry.pub --id main"
    "```"
    ""
    "If resolution or fetch fails, inspect registry configuration and trust before using scoped `--insecure`; do not clear persistent registry trust."
    ""
    "## REPL"
    ""
    "`clpm repl` gives agents a persistent project-scoped Lisp image. Strongly prefer it for iterative Common Lisp development and debugging because definitions, packages, workers, restarts, frame locals, and debugger sessions can persist across calls."
    ""
    "Use `clpm repl` before starting a fresh `sbcl` or ad hoc script when you need to understand a live condition, inspect a value, redefine one function, time a form, trace calls, or confirm image state. Fresh processes are still right for clean end-to-end tests, packaging, and dependency graph changes."
    ""
    "The public bridge CLI is deliberately small:"
    ""
    "```sh"
    "clpm repl daemon [--detach] [--no-load] [--status] [--stop]"
    "clpm repl eval FORM [--package P] [--worker W] [--debug] ..."
    "clpm repl call METHOD [--params-json JSON] [--PARAM VALUE]..."
    "```"
    ""
    "`daemon` owns lifecycle, `eval` is the ergonomic one-form path, and `call` is the generic RPC constructor. `call` parses parameter values as JSON when possible and otherwise sends strings."
    ""
    "Lifecycle and discovery:"
    ""
    "```sh"
    "clpm repl daemon --detach"
    "clpm repl daemon --status"
    "clpm repl call ping"
    "clpm repl call methods"
    "clpm repl call help --method gc"
    "clpm repl eval '(+ 1 2)'"
    "clpm repl daemon --stop"
    "```"
    ""
    "`eval` auto-starts the daemon if needed. Use `daemon --status` when you need to distinguish no daemon, stale pid/socket files, and an unresponsive daemon."
    ""
    "Debug-first workflow:"
    ""
    "```sh"
    "clpm repl eval '(error \"boom\")' --debug"
    "clpm repl eval '(restart-case (error \"need value\") (use-value (v) v))' --debug --restart USE-VALUE --arg 42"
    "clpm repl eval '(warn \"careful\")' --debug --break-on warning"
    "clpm repl eval '(restart-case (/ 1 0) (use-value (v) v))' --handler division-by-zero=use-value:0"
    "```"
    ""
    "Use `eval --debug` rather than plain `eval` when chasing a condition. First run it without a restart to see the condition, user frames, frame numbers, and available restarts. Then re-run with `--restart`, `--arg`, `--frame`, or `--frame-eval` once you know the intended recovery."
    ""
    "Server-owned debug sessions:"
    ""
    "```sh"
    "clpm repl eval '(restart-case (error \"need value\") (use-value (v) v))' --debug --keep"
    "clpm repl call list-debug-sessions"
    "clpm repl call debug-eval-in-frame --session 1 --frame 4 --form 'x'"
    "clpm repl call debug-invoke-restart --session 1 --name USE-VALUE --args '[\"42\"]'"
    "clpm repl call debug-abort --session 1"
    "```"
    ""
    "Use `--keep` only when you need follow-up calls against a live debugger stop. If more than one session is active, pass `--session N` to every debug-* call."
    ""
    "Everyday image management and introspection:"
    ""
    "```sh"
    "clpm repl call current-package"
    "clpm repl call set-package --name CL-USER"
    "clpm repl call list-workers"
    "clpm repl call interrupt --worker default"
    "clpm repl call reset --worker default"
    "clpm repl call kill-worker --name scratch"
    "clpm repl call loaded-systems"
    "clpm repl call image-info"
    "clpm repl call gc --full true"
    "clpm repl call list-redefinitions"
    "clpm repl call compile-file --path src/foo.lisp"
    "clpm repl call load-file --path src/foo.lisp"
    "clpm repl call inspect --form '(make-hash-table)'"
    "clpm repl call arglist --symbol my-function"
    "clpm repl call find-definition --symbol my-function"
    "clpm repl call xref --symbol my-function --direction calls"
    "clpm repl call macroexpand --form '(my-macro x)' --full true"
    "```"
    ""
    "Watch and cleanup:"
    ""
    "```sh"
    "clpm repl call watch --dir src --glob '*.lisp' --auto-revert true"
    "clpm repl call list-watches"
    "clpm repl call unwatch --id 1"
    "clpm repl call list-traced"
    "clpm repl call untrace"
    "clpm repl call list-debug-sessions"
    "clpm repl call debug-abort --session 1"
    "clpm repl call list-redefinitions"
    "clpm repl daemon --stop"
    "```"
    ""
    "Before stopping work, close kept debugger sessions, unwatch file watches, untrace functions, kill or reset throwaway workers, and run `clpm repl call list-redefinitions`. A non-empty result means the image contains definitions that may still need to be written to source. Use `clpm repl daemon --stop` for normal shutdown; let `daemon --status` or `daemon --stop` clean stale pid/socket files instead of deleting `.clpm/repl.*` by hand."
    ""
    "After changing `clpm.project`, `clpm.lock`, registries, or dependency sources, run `clpm deps sync`, then restart the daemon so its ASDF registry and loaded systems match the new dependency graph."
    ""
    "## Safety Rules for Agents"
    ""
    "- Do not hand-edit generated lockfiles unless the user explicitly asks for emergency surgery."
    "- Do not bypass signature or hash checks silently."
    "- Keep dependency changes small and explain why each system was added or removed."
    "- Prefer focused CLPM commands over shelling into implementation details."
    "- Prefer `clpm repl eval FORM --debug` for Common Lisp bug investigation; use plain `eval` only when you already know the form should succeed."
    "- After changing manifests, run `clpm deps sync` and the narrowest relevant test command."
    "- Before stopping work after repl edits, check `clpm repl call list-redefinitions` for in-image definitions that still need source changes."
    "- Do not leave kept debug sessions, watches, traces, or throwaway workers behind."
    ""
    "## Help"
    ""
    "Use `clpm help` for the command list, `clpm help <command>` for command-specific help, and `clpm skill` to print this SKILL.md markdown."))

(defun cmd-skill (&rest args)
  "Print SKILL.md markdown teaching an agent how to use CLPM."
  (when args
    (log-error "Usage: clpm skill")
    (return-from cmd-skill 1))
  (dolist (line +agent-skill-markdown-lines+)
    (format t "~A~%" line))
  0)

;;; help command

(defun print-command-help (command &key subcommand sub-subcommand)
  "Print help for a COMMAND (keyword) and optional SUBCOMMAND chain.

SUB-SUBCOMMAND, when supplied, drills one level deeper (e.g.
`registry trust set` -> command=:registry subcommand=\"trust\"
sub-subcommand=\"set\")."
  (labels ((p (fmt &rest args)
             (apply #'format t (concatenate 'string fmt "~%") args)))
    (unless (member command
                    '(:help :doctor :project :deps :registry :run :store
                      :skill :repl)
                    :test #'eq)
      (log-error "Unknown command: ~A" command)
      (return-from print-command-help 1))
    (case command
      (:help
       (p "Usage: clpm help <command> [subcommand ...]")
       (p "")
       (p "Examples:")
       (p "  clpm help project new")
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
      (:project
       (let ((sub (and (stringp subcommand) (string-downcase subcommand)))
             (ssub (and (stringp sub-subcommand)
                        (string-downcase sub-subcommand))))
         (cond
           ((and sub (string= sub "new"))
            (p "Usage:")
            (p "  clpm project new <name> --workspace [--dir <path>]")
            (p "  clpm project new <name> --bin|--lib [--dir <path>]")
            (p "  clpm project new <name> --bin|--lib --member-of <workspace-dir>")
            0)
           ((and sub (string= sub "init"))
            (p "Usage: clpm project init [name]")
            (p "")
            (p "Creates clpm.project in the current directory.")
            0)
           ((and sub (string= sub "package"))
            (p "Usage: clpm project package")
            (p "")
            (p "Builds a distributable executable in dist/ based on clpm.project :package.")
            (p "")
            (p "Scoped options:")
            (p "  -p, --package <member>  Workspace member target from a workspace root.")
            (p "  --lisp <impl>  Lisp implementation to use (sbcl|ccl|ecl).")
            0)
           ((and sub (string= sub "workspace"))
            (cond
              ((and ssub (string= ssub "init"))
               (p "Usage: clpm project workspace init [--dir <path>]")
               0)
              ((and ssub (string= ssub "add"))
               (p "Usage: clpm project workspace add <member> [--dir <path>]")
               0)
              ((and ssub (string= ssub "remove"))
               (p "Usage: clpm project workspace remove <member> [--dir <path>]")
               0)
              ((and ssub (string= ssub "list"))
               (p "Usage: clpm project workspace list [--dir <path>]")
               0)
              (t
               (p "Usage:")
               (p "  clpm project workspace init [--dir <path>]")
               (p "  clpm project workspace add <member> [--dir <path>]")
               (p "  clpm project workspace remove <member> [--dir <path>]")
               (p "  clpm project workspace list [--dir <path>]")
               0)))
           (t
            (p "Usage:")
            (p "  clpm project new <name> --workspace [--dir <path>]")
            (p "  clpm project new <name> --bin|--lib [--dir <path>]")
            (p "  clpm project new <name> --bin|--lib --member-of <workspace-dir>")
            (p "  clpm project init [name]")
            (p "  clpm project workspace <init|add|remove|list> ...")
            (p "  clpm project package")
            0))))
      (:deps
       (let ((sub (and (stringp subcommand) (string-downcase subcommand))))
         (cond
           ((and sub (string= sub "add"))
            (p "Usage: clpm deps add [--dev|--test] [--any|--caret] [--registry <name>] [--path <dir> | --git <url> --ref <ref>] <system>[@^<semver>|@=<exact>]...")
            (p "")
            (p "Examples:")
            (p "  clpm deps add alexandria")
            (p "  clpm deps add alexandria bordeaux-threads")
            (p "  clpm deps add --caret alexandria")
            (p "  clpm deps add alexandria@^1.4.0")
            (p "  clpm deps add --path ../my-lib my-lib")
            (p "")
            (p "Run `clpm deps sync` after dependency edits to realize the graph.")
            0)
           ((and sub (string= sub "remove"))
            (p "Usage: clpm deps remove [--dev|--test] <dep>")
            0)
           ((and sub (string= sub "sync"))
            (p "Usage: clpm deps sync [--to lock|source|build|active]")
            (p "")
            (p "Stages:")
            (p "  lock    Resolve dependencies and write clpm.lock")
            (p "  source  Resolve and fetch sources")
            (p "  build   Resolve, fetch, and build")
            (p "  active  Resolve, fetch, build, and activate (default)")
            (p "")
            (p "Scoped options:")
            (p "  --insecure             Skip registry signature checks for this run.")
            (p "  --offline              Use cached artifacts; invalid with --to lock.")
            (p "  -j, --jobs N           Parallel fetch/build jobs beyond the lock stage.")
            (p "  --lisp <impl>          Lisp for build/active realization.")
            (p "  --fetch-retries N      Retry budget for CLPM-managed fetches.")
            (p "  --fetch-timeout SECS   Per-request fetch timeout.")
            (p "  --with-optional SYS    Include one declared optional dependency.")
            (p "  --with-all-optional    Include every declared optional dependency.")
            0)
           ((and sub (string= sub "update"))
            (p "Usage: clpm deps update [system ...]")
            (p "")
            (p "Scoped options:")
            (p "  --insecure             Skip registry signature checks for this run.")
            (p "  --fetch-retries N      Retry budget for CLPM-managed fetches.")
            (p "  --fetch-timeout SECS   Per-request fetch timeout.")
            (p "  --with-optional SYS    Include one declared optional dependency.")
            (p "  --with-all-optional    Include every declared optional dependency.")
            0)
           ((and sub (string= sub "search"))
            (p "Usage: clpm deps search <query> [--limit N] [--json]")
            (p "")
            (p "Scoped options:")
            (p "  --insecure             Skip registry signature checks for this run.")
            (p "  --fetch-retries N      Retry budget for CLPM-managed fetches.")
            (p "  --fetch-timeout SECS   Per-request fetch timeout.")
            0)
           ((and sub (string= sub "info"))
            (p "Usage: clpm deps info <system> [--json] [--all]")
            (p "")
            (p "Scoped options:")
            (p "  --insecure             Skip registry signature checks for this run.")
            (p "  --fetch-retries N      Retry budget for CLPM-managed fetches.")
            (p "  --fetch-timeout SECS   Per-request fetch timeout.")
            0)
           ((and sub (string= sub "tree"))
            (p "Usage: clpm [-p <member>] deps tree [--depth N]")
            0)
           ((and sub (string= sub "why"))
            (p "Usage: clpm [-p <member>] deps why <system>")
            0)
           ((and sub (string= sub "audit"))
            (p "Usage: clpm deps audit [--json]")
            0)
           ((and sub (string= sub "sbom"))
            (p "Usage: clpm deps sbom --format <cyclonedx-json|cyclonedx-xml|spdx-json> [--out <path>]")
            (p "")
            (p "Scoped options:")
            (p "  --offline              Use cached artifacts.")
            (p "  --fetch-retries N      Retry budget for CLPM-managed fetches.")
            (p "  --fetch-timeout SECS   Per-request fetch timeout.")
            0)
           (t
            (p "Usage:")
            (p "  clpm deps add [options] <system>...")
            (p "  clpm deps remove [options] <system>")
            (p "  clpm deps sync [--to lock|source|build|active]")
            (p "  clpm deps update [system ...]")
            (p "  clpm deps search|info|tree|why|audit|sbom ...")
            (p "")
            (p "Scoped options:")
            (p "  -p, --package <member>  Workspace member target from a workspace root.")
            0))))
      (:skill
       (p "Usage: clpm skill")
       (p "")
       (p "Print SKILL.md markdown that teaches an agent how to use CLPM.")
       (p "")
       (p "Example:")
       (p "  clpm skill > SKILL.md")
       0)
      (:registry
       (p "Usage: clpm registry <add|list|update|trust|init|key|publish> [options]")
       (p "")
       (let ((sub (and (stringp subcommand) (string-downcase subcommand))))
         (cond
           ((and sub (string= sub "add"))
            (p "Usage: clpm registry add --name <name> --url <git-url> --trust ed25519:<key-id>")
            (p "   or: clpm registry add --quicklisp [--name quicklisp] [--url <dist-url>] [--trust tofu|sha256:<64-hex-digest>]")
            (p "")
            (p "Example:")
            (p "  clpm registry add --name main --url https://example.invalid/registry.git --trust ed25519:abcd...")
            (p "  clpm registry add --quicklisp --trust tofu")
            (p "")
            (p "Quicklisp note: dists do not publish version constraints between systems,")
            (p "so transitive deps from a QL registry get a `nil` constraint (any version).")
            (p "The synthetic version is the dist date (e.g. 20241012). Pin a specific")
            (p "constraint in your clpm.project to override.")
            (p "When --trust is omitted for Quicklisp, CLPM records tofu.")
            0)
           ((and sub (string= sub "list"))
            (p "Usage: clpm registry list")
            0)
           ((and sub (string= sub "update"))
            (p "Usage: clpm registry update [name ...]")
            (p "")
            (p "Scoped options:")
            (p "  --insecure             Skip registry signature checks for this run.")
            (p "  --fetch-retries N      Retry budget for CLPM-managed fetches.")
            (p "  --fetch-timeout SECS   Per-request fetch timeout.")
            0)
           ((and sub (string= sub "key"))
            (let ((ssub (and (stringp sub-subcommand)
                             (string-downcase sub-subcommand))))
              (cond
                ((and ssub (string= ssub "generate"))
                 (p "Usage: clpm registry key generate --out <dir> --id <id>")
                 0)
                ((and ssub (string= ssub "list"))
                 (p "Usage: clpm registry key list [--keys-dir <dir>]")
                 0)
                ((and ssub (string= ssub "import"))
                 (p "Usage: clpm registry key import --pub <path> [--id <id>] [--keys-dir <dir>]")
                 0)
                ((and ssub (string= ssub "verify"))
                 (p "Usage: clpm registry key verify --pub <path> --file <path> --sig <path>")
                 0)
                (t
                 (p "Usage:")
                 (p "  clpm registry key generate --out <dir> --id <id>")
                 (p "  clpm registry key list [--keys-dir <dir>]")
                 (p "  clpm registry key import --pub <path> [--id <id>] [--keys-dir <dir>]")
                 (p "  clpm registry key verify --pub <path> --file <path> --sig <path>")
                 0))))
           ((and sub (string= sub "trust"))
            (let ((ssub (and (stringp sub-subcommand)
                             (string-downcase sub-subcommand))))
              (cond
                ((and ssub (string= ssub "list"))
                 (p "Usage: clpm registry trust list")
                 (p "")
                 (p "Print each configured registry with its current trust setting.")
                 0)
                ((and ssub (string= ssub "set"))
                 (p "Usage: clpm registry trust set <name> <trust>")
                 (p "")
                 (p "Set the trust string for the registry named <name>.")
                 (p "Git registries require `ed25519:<key-id>`.")
                 (p "Quicklisp registries require `tofu` or `sha256:<64-hex-digest>`.")
                 (p "Use one-run `--insecure` on verifier-bearing commands for debugging")
                 (p "instead of permanently clearing registry trust.")
                 0)
                ((and ssub (string= ssub "refresh"))
                 (p "Usage: clpm registry trust refresh <name>")
                 (p "")
                 (p "Re-pin the Quicklisp dist's systems.txt / releases.txt SHA-256")
                 (p "hashes from the live registry contents.")
                 (p "Only Quicklisp registries support trust refresh; Ed25519 git")
                 (p "registries fail instead of changing trust implicitly.")
                 (p "")
                 (p "Scoped options:")
                 (p "  --fetch-retries N      Retry budget for CLPM-managed fetches.")
                 (p "  --fetch-timeout SECS   Per-request fetch timeout.")
                 0)
                (t
                 (p "Usage: clpm registry trust <list|set|refresh> [args]")
                 (p "")
                 (p "Subcommands:")
                 (p "  list                 List registries and trust settings")
                 (p "  set <name> <trust>   Set typed trust string")
                 (p "  refresh <name>       Refresh pinned trust (Quicklisp only)")
                 0))))
           ((and sub (string= sub "init"))
            (p "Usage: clpm registry init --dir <path> --key-id <id> --keys-dir <dir>")
            0)
           ((and sub (string= sub "publish"))
            (p "Usage: clpm registry publish --registry <dir> --key-id <id> --keys-dir <dir> --tarball-url <url> [--tarball-out <path>] [--project <dir>]")
            (p "")
            (p "Scoped options:")
            (p "  -p, --package <member>  Workspace member target from a workspace root.")
            0)
           (t
            (p "Subcommands:")
            (p "  add      Add or update a configured registry")
            (p "  list     List configured registries")
            (p "  update   Update cloned registries (optionally by name)")
            (p "  trust    Manage registry trust settings")
            (p "  init     Initialize a new git registry directory")
            (p "  key      Manage registry signing keys")
            (p "  publish  Publish a project to a registry")
            0))))
      (:repl
       (let ((sub (and (stringp subcommand) (string-downcase subcommand))))
         (cond
           ((and sub (string= sub "daemon"))
            (p "Usage: clpm repl daemon [--detach] [--no-load] [--status [--json]] [--stop]")
            (p "")
            (p "Start, inspect, or stop the daemon Lisp image for the current")
            (p "project. Bare daemon starts in the foreground; --detach starts")
            (p "in the background.")
            (p "")
            (p "Options:")
            (p "  --detach   Launch in the background and return.")
            (p "  --no-load  Skip loading .clpm/asdf-config.lisp.")
            (p "  --status   Report state and clean stale pid/socket files.")
            (p "  --json     With --status, emit raw JSON.")
            (p "  --stop     Ask the daemon to shut down cleanly.")
            (p "")
            (p "Example:")
            (p "  clpm repl daemon --detach")
            0)
           ((and sub (string= sub "eval"))
            (p "Usage: clpm repl eval <form> [--package <name>] [--worker <name>] [--debug] [--no-autostart] [--json]")
            (p "")
            (p "Evaluate one Lisp form in the daemon. With no daemon running and")
            (p "without --no-autostart, the bridge starts one in the background")
            (p "first. Human output is the default; --json emits raw JSON.")
            (p "")
           (p "Options:")
           (p "  --package <name>  Override *package* for this call only")
           (p "                    (daemon's persistent package is unchanged).")
           (p "  --worker <name>   Run on a named worker.")
            (p "  --json            Emit the raw eval response instead of human output.")
            (p "  --handler T=R[:A] Declarative condition recovery; repeatable.")
            (p "  --debug           Enter the continuation-aware debugger path.")
            (p "  --restart <name>  Pick a restart at the first debugger stop.")
            (p "  --arg <form>      Restart argument form; repeatable.")
            (p "  --frame <n> --frame-eval <form>")
            (p "                    Evaluate in a debugger frame, then abort.")
            (p "  --keep            Leave the debugger session for call debug-*.")
            (p "  --break-on <type> Break on signals of this condition type; use none")
            (p "                    to disable the daemon's default break-on setting.")
            (p "  --timeout-ms N    Abort the eval if real time exceeds N milliseconds.")
            (p "  --no-autostart    Fail with rc=2 if no daemon is running.")
            (p "")
            (p "Example:")
            (p "  clpm repl eval '(+ 1 2)'")
            (p "  clpm repl eval '(error \"boom\")' --debug")
            0)
           ((and sub (string= sub "call"))
            (p "Usage: clpm repl call <method> [--params-json <json>] [--PARAM <value>]...")
            (p "")
           (p "Send one daemon RPC method. Parameter values are parsed as JSON")
           (p "when possible, otherwise passed as strings. Hyphens in flag names")
           (p "map to underscores in JSON parameter names.")
           (p "`project_root` and `token` are transport fields owned by CLPM.")
           (p "The daemon must already be running; call never autostarts it.")
            (p "")
            (p "Examples:")
            (p "  clpm repl call methods")
            (p "  clpm repl call help --method gc")
            (p "  clpm repl call gc --full true")
            (p "  clpm repl call compile-file --path src/foo.lisp")
            (p "  clpm repl call debug-abort --session 1")
            (p "")
            (p "Use `clpm repl eval FORM` for evaluation; call is not an eval alias.")
            (p "Use --params-json for arrays, objects, or explicit null.")
            0)
           (t
            (p "Usage:")
            (p "  clpm repl daemon [--detach] [--no-load] [--status [--json]] [--stop]")
            (p "  clpm repl eval <form> [--package <pkg>] [--worker <name>] [--debug] ...")
            (p "  clpm repl call <method> [--params-json <json>] [--PARAM <value>]...")
            (p "")
            (p "Drive a persistent project-scoped Lisp daemon. `call methods`")
            (p "lists the public callable RPC registry, and `call help --method NAME` returns")
            (p "the exact parameter schema for a method. `call` requires an")
            (p "existing daemon; use `daemon --detach` or `eval` to start one.")
            (p "")
            (p "Run `clpm help repl <subcommand>` for per-subcommand details.")
            (p "")
            (p "Scoped options:")
            (p "  -p, --package <member>  Workspace member target from a workspace root.")
            0))))
      (:run
       (let ((sub (and (stringp subcommand) (string-downcase subcommand))))
         (cond
           ((and sub (string= sub "repl"))
            (log-error "`clpm run repl` is not public; use `clpm repl eval FORM` or `clpm repl daemon --detach`.")
            1)
           ((and sub (string= sub "exec"))
            (p "Usage: clpm run exec -- <cmd...>")
            0)
           ((and sub (string= sub "test"))
            (p "Usage: clpm run test")
            (p "")
            (p "Scoped options:")
            (p "  --lisp <impl>  Lisp implementation to use (sbcl|ccl|ecl).")
            0)
           ((and sub (string= sub "script"))
            (p "Usage: clpm run script <name> [-- <args...>]")
            (p "")
            (p "Scoped options:")
            (p "  --lisp <impl>  Lisp implementation to use (sbcl|ccl|ecl).")
            0)
           ((and sub (string= sub "scripts"))
            (p "Usage: clpm run scripts")
            0)
           (t
            (p "Usage:")
            (p "  clpm run [-- <args...>]")
            (p "  clpm run exec -- <cmd...>")
            (p "  clpm run test")
            (p "  clpm run script <name> [-- <args...>]")
            (p "  clpm run scripts")
            (p "")
            (p "Scoped options:")
            (p "  -p, --package <member>  Workspace member target from a workspace root.")
            (p "  --lisp <impl>  Lisp for entrypoint, test, and script execution.")
            0))))
      (:store
       (let ((sub (and (stringp subcommand) (string-downcase subcommand))))
         (cond
           ((and sub (string= sub "clean"))
            (p "Usage: clpm store clean [--dist] [--store]")
            (p "")
            (p "Scoped options:")
            (p "  -p, --package <member>  Workspace member target from a workspace root.")
            0)
           ((and sub (string= sub "gc"))
            (p "Usage: clpm store gc [--dry-run]")
            0)
           (t
            (p "Usage:")
            (p "  clpm store clean [--dist] [--store]")
            (p "  clpm store gc [--dry-run]")
            0))))
      (t
       (log-error "Unknown command: ~A" command)
       1))))

(defun %help-token (value)
  (and (stringp value) (string-downcase value)))

(defun %help-selector-valid-p (command parts)
  "Return true when COMMAND/PARTS names an actual help page."
  (let* ((parts (mapcar #'%help-token parts))
         (sub (first parts))
         (ssub (second parts)))
    (and (every #'identity parts)
         (null (cddr parts))
         (case command
           (:help (null sub))
           (:doctor (null sub))
           (:skill (null sub))
           (:project
            (cond
              ((null sub) t)
              ((member sub '("new" "init" "package") :test #'string=)
               (null ssub))
              ((string= sub "workspace")
               (or (null ssub)
                   (member ssub '("init" "add" "remove" "list")
                           :test #'string=)))
              (t nil)))
           (:deps
            (or (null sub)
                (and (null ssub)
                     (member sub
                             '("add" "remove" "sync" "update" "search"
                               "info" "tree" "why" "audit" "sbom")
                             :test #'string=))))
           (:registry
            (cond
              ((null sub) t)
              ((member sub '("add" "list" "update" "init" "publish")
                       :test #'string=)
               (null ssub))
              ((string= sub "key")
               (or (null ssub)
                   (member ssub '("generate" "list" "import" "verify")
                           :test #'string=)))
              ((string= sub "trust")
               (or (null ssub)
                   (member ssub '("list" "set" "refresh") :test #'string=)))
              (t nil)))
           (:repl
            (or (null sub)
                (and (null ssub)
                     (member sub '("daemon" "eval" "call") :test #'string=))))
           (:run
            (or (null sub)
                (and (null ssub)
                     (member sub '("repl" "exec" "test" "script" "scripts")
                             :test #'string=))))
           (:store
            (or (null sub)
                (and (null ssub)
                     (member sub '("clean" "gc") :test #'string=))))
           (t nil)))))

(defun cmd-help (&rest args)
  "Print command-specific help.

Accepts `clpm help <command> [subcommand [...]]` so chains like
`clpm help registry trust set` reach the leaf help page."
  (let* ((cmd-name (first args))
         (sub-chain (rest args)))
    (unless (and (stringp cmd-name) (plusp (length cmd-name)))
      (log-error "Usage: clpm help <command> [subcommand [...]]")
      (return-from cmd-help 1))
    (let ((command (intern (string-upcase cmd-name) :keyword)))
      (unless (%help-selector-valid-p command sub-chain)
        (if (member command
                    '(:help :doctor :project :deps :registry :run :store
                      :skill :repl)
                    :test #'eq)
            (log-error "Unknown help target: clpm help ~A"
                       (format nil "~{~A~^ ~}" (cons cmd-name sub-chain)))
            (log-error "Unknown command: ~A" command))
        (return-from cmd-help 1))
      (print-command-help command
                          :subcommand (first sub-chain)
                          :sub-subcommand (second sub-chain)))))

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

      ;; Effective merged config: surfaces global defaults so users can see
      ;; why a particular Lisp or build option is in effect.
      (handler-case
          (let* ((config (clpm.config:read-config))
                 (project (handler-case
                              (multiple-value-bind (proot mpath)
                                  (clpm.project:find-project-root)
                                (declare (ignore proot))
                                (and mpath (clpm.project:read-project-file mpath)))
                            (error () nil))))
            (multiple-value-bind (regs build lisp)
                (clpm.config:merge-project-config (or project (clpm.project:make-project))
                                                  :config config)
              (declare (ignore regs))
              (format t "config: effective lisp = ~A~A~%"
                      (or lisp "sbcl")
                      (cond
                        ((and project (clpm.project:project-lisp project))
                         " (from project)")
                        ((getf (clpm.config:config-defaults config) :lisp)
                         " (from global config)")
                        (t " (default)")))
              (format t "config: effective build options = ~S~%" build)))
        (error (c)
          (note-warn "Failed to read effective config: ~A" c)))

      (if (zerop failures)
          (progn
            (format t "~&doctor: OK~%")
            0)
          (progn
            (format t "~&doctor: FAILED (~D issue~:P)~%" failures)
            1)))))
