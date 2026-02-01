;;;; commands.lisp - CLI command implementations

(in-package #:clpm.commands)

;;; Global options (set from main)

(defvar *verbose* nil "Enable verbose output")
(defvar *offline* nil "Offline mode - fail if artifacts missing")
(defvar *insecure* nil "Skip signature verification")
(defvar *jobs* 1 "Number of parallel jobs")

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
             (log-error "Usage: clpm new <name> --bin|--lib [--dir <path>]")
             (return-from cmd-new 1)))
    (let ((name (first args))
          (kind nil)
          (dir nil))
      (unless (and name (plusp (length name)) (not (char= (char name 0) #\-)))
        (usage-error "Missing project name"))
      ;; Parse flags
      (let ((i 1))
        (loop while (< i (length args)) do
          (let ((arg (nth i args)))
            (cond
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
              (t
               (usage-error "Unknown option: ~A" arg))))
          (incf i)))
      (unless kind
        (usage-error "Missing project kind: specify --bin or --lib"))

      (let* ((base (if dir
                       (uiop:ensure-directory-pathname
                        (uiop:ensure-pathname (clpm.platform:expand-path dir)
                                              :defaults (uiop:getcwd)
                                              :want-existing nil))
                       (uiop:getcwd)))
             (project-root (merge-pathnames (format nil "~A/" name) base))
             (src-dir (merge-pathnames "src/" project-root))
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
                       (format nil "(eql :ok (uiop:symbol-call :~A :hello))" name)))))

        (log-info "Created ~A project: ~A" (string-downcase (symbol-name kind))
                  (namestring project-root))
        0))))

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

(defun cmd-add (&rest args)
  "Add a dependency to clpm.project and update clpm.lock."
  (multiple-value-bind (project-root manifest-path lock-path)
      (clpm.project:find-project-root)
    (declare (ignore lock-path))
    (unless manifest-path
      (log-error "No clpm.project found")
      (return-from cmd-add 1))

    (let ((spec nil)
          (dev-p nil)
          (test-p nil)
          (install-p nil)
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
        (log-error "Usage: clpm add <system>[@^<semver>|@=<exact>] [--dev|--test] [--path <path>|--git <url> --ref <ref>] [--install]")
        (return-from cmd-add 1))

      (when (and dev-p test-p)
        (log-error "Only one of --dev or --test may be specified")
        (return-from cmd-add 1))

      (when (and path git-url)
        (log-error "Only one of --path or --git may be specified")
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
             (constraint-form nil))
        (multiple-value-bind (sys parsed-constraint)
            (parse-dep-spec spec)
          (setf system-id sys)
          (when (eq parsed-constraint :invalid)
            (log-error "Invalid dependency spec: ~A" spec)
            (return-from cmd-add 1))
          (when (and parsed-constraint (or path git-url))
            (log-error "Do not combine @<constraint> with --path/--git")
            (return-from cmd-add 1))
          (cond
            (path
             (setf constraint-form (list :path path)))
            (git-url
              (setf constraint-form (list :git :url git-url :ref git-ref)))
            (parsed-constraint
             (setf constraint-form parsed-constraint))
            (t
             (let ((v-max (highest-system-version registries system-id)))
               (unless v-max
                 (log-error "No versions found for ~A in configured registries" system-id)
                 (return-from cmd-add 1))
               (setf constraint-form (list :semver (format nil "^~A" v-max)))))))

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
                       :constraint constraint-form)
                      deps))
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
  (multiple-value-bind (project-root manifest-path lock-path)
      (clpm.project:find-project-root)
    (declare (ignore lock-path))
    (unless manifest-path
      (log-error "No clpm.project found")
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

;;; resolve command

(defun cmd-resolve ()
  "Resolve dependencies and create/update lockfile."
  (multiple-value-bind (project-root manifest-path lock-path)
      (clpm.project:find-project-root)
    (unless manifest-path
      (log-error "No clpm.project found")
      (return-from cmd-resolve 1))
    (log-info "Resolving dependencies...")
    (let* ((project (clpm.project:read-project-file manifest-path))
           (lockfile (when lock-path
                       (clpm.project:read-lock-file lock-path)))
           (registries (load-project-registries project)))
      ;; Resolve
      (handler-case
          (let ((resolution (clpm.solver:solve project registries
                                               :lockfile lockfile)))
            (let ((new-lockfile (clpm.solver:resolution-to-lockfile
                                 resolution project registries)))
              ;; Write lockfile
              (let ((lock-out (merge-pathnames "clpm.lock" project-root)))
                (clpm.project:write-lock-file new-lockfile lock-out)
                (log-info "Wrote clpm.lock")
                (log-info "Resolved ~D systems"
                          (length (clpm.project:lockfile-resolved new-lockfile))))))
        (clpm.errors:clpm-resolve-error (c)
          (log-error "Failed to resolve dependencies: ~A"
                     (clpm.errors:clpm-error-message c))
          (let ((chain (clpm.errors:clpm-resolve-error-conflict-chain c)))
            (when chain
              (format *error-output* "~&Conflict chain:~%")
              (dolist (line chain)
                (format *error-output* "  ~A~%" line))))
          (return-from cmd-resolve 2))))
    0))

;;; fetch command

(defun cmd-fetch ()
  "Fetch all dependencies from lockfile."
  (multiple-value-bind (project-root manifest-path lock-path)
      (clpm.project:find-project-root)
    (declare (ignore manifest-path))
    (unless lock-path
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
                            :lockfile-path lock-path)))
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
  (multiple-value-bind (project-root manifest-path lock-path)
      (clpm.project:find-project-root)
    (unless lock-path
      (log-error "No clpm.lock found - run 'clpm resolve' first")
      (return-from cmd-build 1))
    (log-info "Building dependencies...")
    (let* ((project (clpm.project:read-project-file manifest-path))
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
                                            :compile-options compile-options)))
                (log-info "Built ~D systems" (length build-results)))
            (clpm.errors:clpm-build-error (c)
              (log-error "~A" c)
              (return-from cmd-build 1))))))
    0))

;;; install command (resolve + fetch + build)

(defun cmd-install ()
  "Install all dependencies (resolve, fetch, build)."
  (multiple-value-bind (project-root manifest-path lock-path)
      (clpm.project:find-project-root)
    (unless manifest-path
      (log-error "No clpm.project found")
      (return-from cmd-install 1))
    (let* ((project (clpm.project:read-project-file manifest-path))
           (compile-options (nth-value 1 (clpm.config:merge-project-config project))))
      ;; Resolve if no lockfile
      (unless lock-path
        (let ((result (cmd-resolve)))
          (unless (zerop result)
            (return-from cmd-install result)))
        (setf lock-path (merge-pathnames "clpm.lock" project-root)))
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
                                     :compile-options compile-options))
      (log-info "Project installed successfully")
      (log-info "Run 'clpm repl' to start SBCL with project loaded"))
    0))

;;; update command

(defun cmd-update (&rest systems)
  "Update dependencies (re-resolve with latest versions)."
  (declare (ignore systems))  ; TODO: selective update
  (multiple-value-bind (project-root manifest-path lock-path)
      (clpm.project:find-project-root)
    (declare (ignore project-root lock-path))
    (unless manifest-path
      (log-error "No clpm.project found")
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
                                               (uiop:pathname-directory-pathname
                                                manifest-path))))
                (clpm.project:write-lock-file new-lockfile lock-out)
                (log-info "Updated clpm.lock"))))
        (clpm.errors:clpm-resolve-error (c)
          (log-error "~A" c)
          (return-from cmd-update 2))))
    0))

;;; repl command

(defun cmd-repl (&key load-system)
  "Start SBCL REPL with project activated."
  (multiple-value-bind (project-root manifest-path lock-path)
      (clpm.project:find-project-root)
    (declare (ignore manifest-path))
    (unless lock-path
      (log-error "No clpm.lock found - run 'clpm install' first")
      (return-from cmd-repl 1))
    (let* ((config-path (merge-pathnames ".clpm/asdf-config.lisp" project-root))
           (args (list "sbcl")))
      (unless (uiop:file-exists-p config-path)
        (log-error "Project not activated - run 'clpm install' first")
        (return-from cmd-repl 1))
      ;; Build SBCL args
      (push "--load" args)
      (push (namestring config-path) args)
      (when load-system
        (push "--eval" args)
        (push (format nil "(asdf:load-system ~S)" load-system) args))
      (setf args (nreverse args))
      ;; Replace current process with SBCL
      (log-info "Starting SBCL...")
      ;; Use run-program since we can't exec in portable CL
      (multiple-value-bind (output error-output exit-code)
          (uiop:run-program args
                            :input :interactive
                            :output :interactive
                            :error-output :interactive
                            :ignore-error-status t)
        (declare (ignore output error-output))
        exit-code))))

;;; run/exec commands

(defun ensure-project-activated (project-root)
  "Ensure PROJECT-ROOT has an activation config; returns (values config-path exit-code)."
  (let ((config-path (merge-pathnames ".clpm/asdf-config.lisp" project-root)))
    (unless (uiop:file-exists-p config-path)
      (log-info "Project not activated; running 'clpm install'...")
      (let ((rc (uiop:with-current-directory (project-root)
                  (cmd-install))))
        (unless (zerop rc)
          (return-from ensure-project-activated (values nil rc)))))
    (if (uiop:file-exists-p config-path)
        (values config-path 0)
        (values nil 1))))

(defun parse-function-spec (spec)
  "Parse \"<package>::<fn>\" and return (values package-name function-name).
Returns NIL values on parse failure."
  (let ((pos (search "::" spec)))
    (when pos
      (let ((pkg (subseq spec 0 pos))
            (fn (subseq spec (+ pos 2))))
        (when (and (plusp (length pkg)) (plusp (length fn)))
          (values pkg fn))))))

(defun cmd-run (&rest args)
  "Run the project entrypoint defined in clpm.project :run."
  (multiple-value-bind (project-root manifest-path lock-path)
      (clpm.project:find-project-root)
    (declare (ignore lock-path))
    (unless manifest-path
      (log-error "No clpm.project found")
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

          (multiple-value-bind (pkg fn)
              (parse-function-spec fn-spec)
            (unless (and pkg fn)
              (log-error "Invalid :run :function: expected <package>::<fn>, got ~S" fn-spec)
              (return-from cmd-run 1))

            (let* ((run-args (if (and args (string= (first args) "--"))
                                 (rest args)
                                 args))
                   (pkg-key (intern (string-upcase pkg) :keyword))
                   (fn-key (intern (string-upcase fn) :keyword))
                   (args-var (intern "CLPM-RUN-ARGS" "CL-USER"))
                   (result-var (intern "CLPM-RUN-RESULT" "CL-USER"))
                   (call-form
                     `(let ((,args-var ',run-args))
                        (let ((,result-var (uiop:symbol-call ,pkg-key ,fn-key ,args-var)))
                          (sb-ext:exit :code (if (integerp ,result-var) ,result-var 0)))))
                   (call-form-str
                     (with-standard-io-syntax
                       (let ((*package* (find-package "CL-USER")))
                         (prin1-to-string call-form))))
                   (sbcl-args (list "sbcl" "--noinform" "--non-interactive" "--disable-debugger"
                                    "--load" (namestring config-path)
                                    "--eval" (format nil "(asdf:load-system ~S)" system)
                                    "--eval" call-form-str)))
              (log-info "Running ~A (~A)..." system fn-spec)
              (multiple-value-bind (output error-output exit-code)
                  (clpm.platform:run-program sbcl-args
                                             :directory project-root
                                             :output :interactive
                                             :error-output :interactive
                                             :timeout 600000)
                (declare (ignore output error-output))
                exit-code))))))))

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
  (multiple-value-bind (project-root manifest-path lock-path)
      (clpm.project:find-project-root)
    (declare (ignore lock-path))
    (unless manifest-path
      (log-error "No clpm.project found")
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
             (url (clpm.project:registry-ref-url ref))
             (trust (clpm.project:registry-ref-trust ref)))
        (log-verbose "Loading registry: ~A" name)
        (push (clpm.registry:clone-registry name url :trust-key trust)
              registries)))
    (nreverse registries)))

;;; registry command

(defun cmd-registry (&rest args)
  "Manage global registries in config.sxp."
  (let ((subcommand (first args))
        (rest (rest args)))
    (cond
      ((or (null subcommand) (string= subcommand "help"))
       (log-error "Usage: clpm registry <list|add|update> [options]")
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
             (trust nil))
         (loop while rest do
           (let ((arg (pop rest)))
             (cond
               ((string= arg "--name") (setf name (pop rest)))
               ((string= arg "--url") (setf url (pop rest)))
               ((string= arg "--trust") (setf trust (pop rest)))
               (t
                (log-error "Unknown option: ~A" arg)
                (return-from cmd-registry 1)))))
         (unless (and name url trust)
           (log-error "Missing required options: --name, --url, --trust")
           (return-from cmd-registry 1))
         (let* ((cfg (clpm.config:read-config))
                (regs (clpm.config:config-registries cfg))
                (existing (find name regs
                                :key #'clpm.project:registry-ref-name
                                :test #'string=)))
           (if existing
               (progn
                 (setf (clpm.project:registry-ref-url existing) url
                       (clpm.project:registry-ref-trust existing) trust)
                 (log-info "Updated registry: ~A" name))
               (progn
                 (push (clpm.project::make-registry-ref
                        :kind :git
                        :name name
                        :url url
                        :trust trust)
                       regs)
                 (setf (clpm.config:config-registries cfg) regs)
                 (log-info "Added registry: ~A" name)))
           (clpm.config:write-config cfg))
         0))

      ((string= subcommand "update")
       (let* ((names rest)
              (cfg (clpm.config:read-config))
              (refs (clpm.config:config-registries cfg)))
         (dolist (ref refs)
           (let ((name (clpm.project:registry-ref-name ref)))
             (when (or (null names) (member name names :test #'string=))
               (log-info "Updating registry: ~A" name)
               (handler-case
                   (let ((reg (clpm.registry:clone-registry
                               name
                               (clpm.project:registry-ref-url ref)
                               :trust-key (clpm.project:registry-ref-trust ref))))
                     (clpm.registry:update-registry reg))
                 (error (c)
                   (log-error "Failed to update registry ~A: ~A" name c)
                   (return-from cmd-registry 1))))))
         0))

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
