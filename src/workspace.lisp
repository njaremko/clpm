;;;; workspace.lisp - Workspace manifest and discovery

(in-package #:clpm.workspace)

(defstruct workspace
  "A CLPM workspace definition."
  (format 1 :type integer)
  (members nil :type list)
  (depends nil :type list))

(defun %normalize-workspace-member (member &key file)
  (unless (and (stringp member) (plusp (length member)))
    (clpm.errors:signal-error 'clpm.errors:clpm-parse-error
                              "Workspace member path must be a non-empty string, got ~S" member
                              :file file))
  ;; Accept either "member" or "member/" in files, but store without the
  ;; trailing directory separator for a nicer `-p member` UX.
  (let ((trimmed (string-right-trim '(#\/ #\\) member)))
    (when (zerop (length trimmed))
      (clpm.errors:signal-error 'clpm.errors:clpm-parse-error
                                "Workspace member path must not be empty"
                                :file file))
    trimmed))

(defun %parse-workspace-dependency (form &key file)
  (unless (and (consp form) (eq (car form) :dep))
    (clpm.errors:signal-error 'clpm.errors:clpm-parse-error
                              "Workspace dependency must start with :dep, got ~S" form
                              :file file))
  (let ((dep (clpm.project:make-dependency)))
    (loop for (key val) on (cdr form) by #'cddr do
      (case key
        (:system
         (unless (and (stringp val) (plusp (length val)))
           (clpm.errors:signal-error 'clpm.errors:clpm-parse-error
                                     "Workspace dependency :system must be a non-empty string, got ~S" val
                                     :file file))
         (setf (clpm.project:dependency-system dep) val))
        (:constraint
         (setf (clpm.project:dependency-constraint dep) val))
        (:source
         (setf (clpm.project:dependency-source dep) val))
        (t
         (clpm.errors:signal-error 'clpm.errors:clpm-parse-error
                                   "Unknown key in workspace dependency: ~S" key
                                   :file file))))
    (unless (clpm.project:dependency-system dep)
      (clpm.errors:signal-error 'clpm.errors:clpm-parse-error
                                "Workspace dependency is missing :system"
                                :file file))
    dep))

(defun %serialize-workspace-dependency (dep)
  (let ((form `(:dep
                :system ,(clpm.project:dependency-system dep)
                :constraint ,(clpm.project:dependency-constraint dep))))
    (when (clpm.project:dependency-source dep)
      (setf form (append form (list :source (clpm.project:dependency-source dep)))))
    form))

(defun workspace-dependency-marker-p (constraint)
  "Return true when CONSTRAINT is the project-level workspace dependency marker."
  (and (stringp constraint) (string= constraint "workspace")))

(defun workspace-dependency-for-system (ws system-id)
  "Find SYSTEM-ID in WS's workspace-level dependency declarations."
  (find system-id (workspace-depends ws)
        :key #'clpm.project:dependency-system
        :test #'string=))

(defun parse-workspace (form &key file)
  "Parse a workspace manifest FORM.

Expected schema:
  (:workspace
    :format 1
    :members (\"path1\" \"path2\" ...)
    :depends ((:dep :system \"system\" :constraint <constraint>) ...))"
  (unless (and (consp form) (eq (car form) :workspace))
    (clpm.errors:signal-error 'clpm.errors:clpm-parse-error
                              "Workspace file must start with (:workspace ...)"
                              :file file))
  (let ((ws (make-workspace)))
    (loop for (key val) on (cdr form) by #'cddr do
      (case key
        (:format
         (unless (and (integerp val) (plusp val))
           (clpm.errors:signal-error 'clpm.errors:clpm-parse-error
                                     "Workspace :format must be a positive integer, got ~S" val
                                     :file file))
         (setf (workspace-format ws) val))
        (:members
         (unless (and (listp val) (every #'stringp val))
           (clpm.errors:signal-error 'clpm.errors:clpm-parse-error
                                     "Workspace :members must be a list of strings, got ~S" val
                                     :file file))
         (let ((members '()))
           (dolist (m val)
             (let* ((norm (%normalize-workspace-member m :file file))
                    (pn (uiop:ensure-pathname norm
                                              :defaults (uiop:getcwd)
                                              :want-relative nil
                                              :want-absolute nil)))
               (when (uiop:absolute-pathname-p pn)
                 (clpm.errors:signal-error 'clpm.errors:clpm-parse-error
                                           "Workspace member path must be relative, got ~S" m
                                           :file file))
               (let ((dir (pathname-directory pn)))
                 (when (or (member :back dir)
                           (member :up dir)
                           (search ".." norm))
                   (clpm.errors:signal-error 'clpm.errors:clpm-parse-error
                                             "Workspace member path must not escape the workspace root, got ~S" m
                                             :file file)))
               (push norm members)))
           (setf (workspace-members ws)
                 (sort (remove-duplicates (nreverse members) :test #'string=)
                       #'string<))))
        (:depends
         (unless (listp val)
           (clpm.errors:signal-error 'clpm.errors:clpm-parse-error
                                     "Workspace :depends must be a list, got ~S" val
                                     :file file))
         (let ((deps (mapcar (lambda (dep)
                               (%parse-workspace-dependency dep :file file))
                             val)))
           (let ((seen '()))
             (dolist (dep deps)
               (let ((system (clpm.project:dependency-system dep)))
                 (when (member system seen :test #'string=)
                   (clpm.errors:signal-error 'clpm.errors:clpm-parse-error
                                             "Duplicate workspace dependency: ~A" system
                                             :file file))
                 (push system seen))))
           (setf (workspace-depends ws)
                 (sort deps #'string<
                       :key #'clpm.project:dependency-system))))
        (t
         (clpm.errors:signal-error 'clpm.errors:clpm-parse-error
                                   "Unknown key in workspace manifest: ~S" key
                                   :file file))))
    (unless (= (workspace-format ws) 1)
      (clpm.errors:signal-error 'clpm.errors:clpm-parse-error
                                "Unsupported workspace manifest format: ~D" (workspace-format ws)
                                :file file))
    ws))

(defun serialize-workspace (ws)
  "Serialize workspace WS to a canonical S-expression."
  `(:workspace
    :format ,(workspace-format ws)
    :members ,(sort (copy-list (or (workspace-members ws) '()))
                    #'string<)
    :depends ,(mapcar #'%serialize-workspace-dependency
                      (sort (copy-list (or (workspace-depends ws) '()))
                            #'string<
                            :key #'clpm.project:dependency-system))))

(defun %project-has-workspace-dependency-p (project)
  (some #'workspace-dependency-marker-p
        (mapcar #'clpm.project:dependency-constraint
                (append (clpm.project:project-depends project)
                        (clpm.project:project-dev-depends project)
                        (clpm.project:project-test-depends project)))))

(defun %path-constraint-path (constraint)
  (when (and (consp constraint) (eq (car constraint) :path))
    (or (and (rest constraint)
             (stringp (second constraint))
             (second constraint))
        (getf (cdr constraint) :path))))

(defun %normalize-workspace-path-constraint (constraint workspace-root &key file system-id)
  (let ((raw (%path-constraint-path constraint)))
    (if raw
        (let* ((expanded (clpm.platform:expand-path raw))
               (pn (uiop:ensure-pathname expanded
                                         :defaults workspace-root
                                         :want-existing nil))
               (merged (merge-pathnames pn workspace-root))
               (abs (uiop:ensure-directory-pathname merged)))
          (unless (uiop:directory-exists-p abs)
            (clpm.errors:signal-error 'clpm.errors:clpm-parse-error
                                      "Workspace dependency~@[ ~A~] path does not exist: ~A"
                                      system-id
                                      (namestring abs)
                                      :file file))
          (list :path (namestring (uiop:ensure-directory-pathname (truename abs)))))
        constraint)))

(defun %effective-workspace-dependency-constraint (dep workspace-root workspace-path)
  (let ((constraint (clpm.project:dependency-constraint dep)))
    (when (workspace-dependency-marker-p constraint)
      (clpm.errors:signal-error 'clpm.errors:clpm-parse-error
                                "Workspace dependency ~A cannot itself use the workspace marker"
                                (clpm.project:dependency-system dep)
                                :file (namestring workspace-path)))
    (%normalize-workspace-path-constraint
     constraint
     workspace-root
     :file (namestring workspace-path)
     :system-id (clpm.project:dependency-system dep))))

(defun expand-project-workspace-dependencies (project manifest-path
                                              &key workspace-root workspace-path)
  "Expand project dependencies whose constraint is \"workspace\".

The marker is looked up in the nearest clpm.workspace :depends declaration.
Path constraints from the workspace file are resolved relative to the workspace
root and injected into PROJECT as absolute path constraints for the solver."
  (unless (%project-has-workspace-dependency-p project)
    (return-from expand-project-workspace-dependencies project))
  (let* ((manifest-path (uiop:ensure-pathname manifest-path :want-file t))
         (project-root (uiop:pathname-directory-pathname manifest-path)))
    (multiple-value-bind (root ws-path)
        (cond
          ((and workspace-root workspace-path)
           (values workspace-root workspace-path))
          (workspace-root
           (values workspace-root (merge-pathnames "clpm.workspace" workspace-root)))
          (t
           (find-workspace-root project-root)))
      (unless (and root ws-path (uiop:file-exists-p ws-path))
        (clpm.errors:signal-error 'clpm.errors:clpm-parse-error
                                  "Project dependency uses version \"workspace\", but no clpm.workspace was found"
                                  :file (namestring manifest-path)))
      (let ((ws (read-workspace-file ws-path)))
        (labels ((expand-dep (dep)
                   (when (workspace-dependency-marker-p
                          (clpm.project:dependency-constraint dep))
                     (when (clpm.project:dependency-source dep)
                       (clpm.errors:signal-error 'clpm.errors:clpm-parse-error
                                                 "Dependency ~A cannot combine version \"workspace\" with :source"
                                                 (clpm.project:dependency-system dep)
                                                 :file (namestring manifest-path)))
                     (let* ((system (clpm.project:dependency-system dep))
                            (workspace-dep (workspace-dependency-for-system ws system)))
                       (unless workspace-dep
                         (clpm.errors:signal-error 'clpm.errors:clpm-parse-error
                                                   "Workspace dependency not declared: ~A"
                                                   system
                                                   :file (namestring ws-path)))
                       (setf (clpm.project:dependency-constraint dep)
                             (%effective-workspace-dependency-constraint
                              workspace-dep root ws-path)
                             (clpm.project:dependency-source dep)
                             (clpm.project:dependency-source workspace-dep))))
                   dep))
          (setf (clpm.project:project-depends project)
                (mapcar #'expand-dep (clpm.project:project-depends project))
                (clpm.project:project-dev-depends project)
                (mapcar #'expand-dep (clpm.project:project-dev-depends project))
                (clpm.project:project-test-depends project)
                (mapcar #'expand-dep (clpm.project:project-test-depends project)))))
      project)))

(defun read-workspace-file (path)
  "Read a clpm.workspace file and return a workspace struct."
  (let ((form (clpm.io.sexp:read-safe-sexp-from-file path)))
    (parse-workspace form :file (namestring path))))

(defun write-workspace-file (ws path)
  "Write workspace WS to PATH as a canonical S-expression."
  (clpm.io.sexp:write-canonical-sexp-to-file (serialize-workspace ws) path :pretty t))

;;; Discovery

(defun find-workspace-root (&optional (start (uiop:getcwd)))
  "Find the workspace root containing clpm.workspace.
Searches upward from START directory.
Returns (values workspace-root workspace-path) or nil."
  (let ((dir (uiop:ensure-directory-pathname start)))
    (loop
      (let ((workspace-path (merge-pathnames "clpm.workspace" dir)))
        (when (uiop:file-exists-p workspace-path)
          (return (values dir workspace-path))))
      (let ((parent (uiop:pathname-parent-directory-pathname dir)))
        (when (or (null parent) (equal parent dir))
          (return nil))
        (setf dir parent)))))

(defun find-project-or-workspace-root (&optional (start (uiop:getcwd)))
  "Find the nearest project or workspace marker from START.

Searches upward, preferring the nearest marker.

Returns:
  (values root kind manifest-path lock-path workspace-path)

Where KIND is one of:
  - :project   (manifest-path points to clpm.project; lock-path may be nil)
  - :workspace (workspace-path points to clpm.workspace)"
  (let ((dir (uiop:ensure-directory-pathname start)))
    (loop
      (let ((manifest (merge-pathnames "clpm.project" dir))
            (lock (merge-pathnames "clpm.lock" dir))
            (workspace-path (merge-pathnames "clpm.workspace" dir)))
        (cond
          ((uiop:file-exists-p manifest)
           (return (values dir :project manifest
                           (when (uiop:file-exists-p lock) lock)
                           nil)))
          ((uiop:file-exists-p workspace-path)
           (return (values dir :workspace nil nil workspace-path)))))
      (let ((parent (uiop:pathname-parent-directory-pathname dir)))
        (when (or (null parent) (equal parent dir))
          (return nil))
        (setf dir parent)))))
