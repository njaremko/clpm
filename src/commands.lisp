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
                   (values release-ref nil))))
           (load-merged-registries ()
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
                        :kind (clpm.project:registry-ref-kind ref)))))))
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

(defun sbcl-load-systems-argv (systems)
  "Return an argv fragment that loads each system in SYSTEMS via ASDF."
  (mapcan (lambda (sys)
            (list "--eval" (format nil "(asdf:load-system ~S)" sys)))
          systems))

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
	                   (deps (project-dependency-system-ids project '(:depends)))
	                   (sbcl-args (append (list "sbcl" "--noinform" "--non-interactive" "--disable-debugger"
	                                            "--load" (namestring config-path))
	                                      (sbcl-load-systems-argv deps)
	                                      (list "--eval" (format nil "(asdf:load-system ~S)" system)
	                                            "--eval" call-form-str))))
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

;;; test command

(defun cmd-test (&rest args)
  "Run project tests.

Uses clpm.project :test metadata:
  :test (:systems (\"<test-system>\" ...))"
  (declare (ignore args))
  (multiple-value-bind (project-root manifest-path lock-path)
      (clpm.project:find-project-root)
    (declare (ignore lock-path))
    (unless manifest-path
      (log-error "No clpm.project found")
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
	                      (sb-ext:exit :code (if ,ok-var 0 1))))
	                 (call-form-str
	                   (with-standard-io-syntax
	                     (let ((*package* (find-package "CL-USER")))
	                       (prin1-to-string call-form))))
	                 (deps (project-dependency-system-ids project '(:depends :test-depends)))
	                 (sbcl-args (append (list "sbcl" "--noinform" "--non-interactive" "--disable-debugger"
	                                          "--load" (namestring config-path))
	                                    (sbcl-load-systems-argv deps)
	                                    (list "--eval" call-form-str))))
	            (multiple-value-bind (output error-output exit-code)
	                (clpm.platform:run-program sbcl-args
	                                           :directory project-root
	                                           :output :interactive
                                           :error-output :interactive
                                           :timeout 600000)
              (declare (ignore output error-output))
              exit-code)))))))

;;; package command

(defun cmd-package (&rest args)
  "Build a distributable executable according to clpm.project :package metadata.

Manifest schema:
  :package (:output \"dist/<name>\" :system \"<system>\" :function \"<package>::<fn>\")"
  (declare (ignore args))
  (multiple-value-bind (project-root manifest-path lock-path)
      (clpm.project:find-project-root)
    (unless manifest-path
      (log-error "No clpm.project found")
      (return-from cmd-package 1))
    (let* ((project (clpm.project:read-project-file manifest-path))
           (pkg (clpm.project:project-package project)))
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
                     `(sb-ext:save-lisp-and-die ,(namestring output-path)
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
	                                      (list "--eval" (format nil "(asdf:load-system ~S)" system)
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

              (clpm.io.sexp:write-canonical-sexp-to-file
               `(:package-meta
                 :lock-sha256 ,lock-sha256
                 :sbcl-version ,(clpm.platform:sbcl-version)
                 :platform ,(clpm.platform:platform-triple))
               meta-path)
              (log-info "Wrote package metadata: ~A" (namestring meta-path))
              0)))))))

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
    (multiple-value-bind (project-root manifest-path lock-path)
        (clpm.project:find-project-root)
      (declare (ignore manifest-path lock-path))
      (unless project-root
        (log-error "No clpm.project found")
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
             (trust (clpm.project:registry-ref-trust ref)))
        (log-verbose "Loading registry: ~A" name)
        (push (clpm.registry:clone-registry name url :trust-key trust :kind kind)
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
           (setf trust nil))
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
                               :trust-key (clpm.project:registry-ref-trust ref)
                               :kind (clpm.project:registry-ref-kind ref))))
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
       (p "Usage: clpm add [--dev|--test] [--path <dir> | --git <url> --ref <ref>] <dep>[@<constraint>]")
       (p "")
       (p "Examples:")
       (p "  clpm add alexandria")
       (p "  clpm add alexandria@^1.4.0")
       (p "  clpm add --path ../my-lib my-lib")
       (p "  clpm add --git https://example.invalid/repo.git --ref main my-lib")
       (p "")
       (p "Options:")
       (p "  --dev         Add to :dev-depends")
       (p "  --test        Add to :test-depends")
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
       (p "Usage: clpm info <system> [--json]")
       (p "")
       (p "Show details about a system and available releases.")
       (p "Not implemented yet.")
       0)
      (:tree
       (p "Usage: clpm tree [--package <member>] [--json]")
       (p "")
       (p "Print the resolved dependency tree for the current project/workspace.")
       (p "Not implemented yet.")
       0)
      (:why
       (p "Usage: clpm why <system> [--package <member>]")
       (p "")
       (p "Explain why <system> appears in the resolved dependency graph.")
       (p "Not implemented yet.")
       0)
      (:scripts
       (p "Usage: clpm scripts <list|run> [args]")
       (p "")
       (p "List and run project scripts defined in clpm.project.")
       (p "Not implemented yet.")
       0)
      (:audit
       (p "Usage: clpm audit [--json]")
       (p "")
       (p "Show a provenance and trust report for the current lockfile.")
       (p "Not implemented yet.")
       0)
      (:sbom
       (p "Usage: clpm sbom --format <cyclonedx-json> [--out <path>]")
       (p "")
       (p "Generate a software bill of materials (SBOM) from the lockfile.")
       (p "Not implemented yet.")
       0)
      (:keys
       (p "Usage: clpm keys <generate|list|show> [args]")
       (p "")
       (p "Manage Ed25519 keys for signing registries and releases.")
       (p "Not implemented yet.")
       0)
      (:publish
       (p "Usage: clpm publish --registry <path-or-url> [--project <dir>]")
       (p "")
       (p "Publish the current project to a git-backed registry.")
       (p "Not implemented yet.")
       0)
      (:workspace
       (p "Usage: clpm workspace <init|add|list> [args]")
       (p "")
       (p "Manage workspaces and workspace members.")
       (p "Not implemented yet.")
       0)
      (:registry
       (p "Usage: clpm registry <add|list|update> [options]")
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
            (p "Usage: clpm registry update [name ...]")
            0)
           (t
            (p "Subcommands:")
            (p "  add      Add or update a configured registry")
            (p "  list     List configured registries")
            (p "  update   Update cloned registries (optionally by name)")
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
