;;;; workspace.lisp - Workspace manifest and discovery

(in-package #:clpm.workspace)

(defstruct workspace
  "A CLPM workspace definition."
  (format 1 :type integer)
  (members nil :type list))

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

(defun parse-workspace (form &key file)
  "Parse a workspace manifest FORM.

Expected schema:
  (:workspace :format 1 :members (\"path1\" \"path2\" ...))"
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
               (push norm members)))
           (setf (workspace-members ws)
                 (sort (remove-duplicates (nreverse members) :test #'string=)
                       #'string<))))
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
                    #'string<)))

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
