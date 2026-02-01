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
          (log-error "~A" c)
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

;;; run command

(defun cmd-run (script-name &rest args)
  "Run a script defined in the project."
  (declare (ignore args))
  (multiple-value-bind (project-root manifest-path lock-path)
      (clpm.project:find-project-root)
    (declare (ignore lock-path))
    (unless manifest-path
      (log-error "No clpm.project found")
      (return-from cmd-run 1))
    (let* ((project (clpm.project:read-project-file manifest-path))
           (scripts (clpm.project:project-scripts project))
           (script (assoc script-name scripts :test #'string=)))
      (unless script
        (log-error "Unknown script: ~A" script-name)
        (log-error "Available scripts: ~{~A~^, ~}"
                   (mapcar #'car scripts))
        (return-from cmd-run 1))
      ;; Execute script
      (let ((script-spec (cdr script)))
        (cond
          ((stringp script-spec)
           ;; Shell command
           (multiple-value-bind (output error-output exit-code)
               (clpm.platform:run-program
                (list "sh" "-c" script-spec)
                :directory project-root
                :output :interactive
                :error-output :interactive)
             (declare (ignore output error-output))
             exit-code))
          ((and (consp script-spec) (eq (car script-spec) :lisp))
           ;; Lisp expression - run in REPL
           (cmd-repl :load-system nil))
          (t
           (log-error "Invalid script specification")
           1))))))

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
