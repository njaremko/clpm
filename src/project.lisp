;;;; project.lisp - Project manifest and lockfile handling

(in-package #:clpm.project)

;;; Project manifest structures

(defstruct project
  "A CLPM project manifest."
  (name nil :type (or null string))
  (version nil :type (or null string))
  (systems nil :type list)
  (run nil :type (or null list))
  (depends nil :type list)
  (dev-depends nil :type list)
  (test-depends nil :type list)
  (registries nil :type list)
  (sbcl-constraints nil :type list)
  (build-options nil :type list)
  (scripts nil :type list))

(defstruct dependency
  "A project dependency."
  (system nil :type (or null string))
  (constraint nil)
  (source nil)
  (optional-p nil :type boolean)
  (features nil :type list))

(defstruct registry-ref
  "A registry reference."
  (name nil :type (or null string))
  (kind :git :type keyword)
  (url nil :type (or null string))
  (trust nil :type (or null string)))

;;; Lockfile structures

(defstruct lockfile
  "A CLPM lockfile."
  (format 1 :type integer)
  (generated-at nil :type (or null string))
  (project-name nil :type (or null string))
  (clpm-version nil :type (or null string))
  (registries nil :type list)
  (resolved nil :type list))

(defstruct locked-registry
  "A locked registry reference."
  (name nil :type (or null string))
  (kind :git :type keyword)
  (url nil :type (or null string))
  (commit nil :type (or null string))
  (trust nil :type (or null string))
  (signature nil :type (or null string)))

(defstruct locked-system
  "A locked system entry."
  (id nil :type (or null string))
  (release nil)
  (deps nil :type list))

(defstruct locked-release
  "A locked release."
  (name nil :type (or null string))
  (version nil :type (or null string))
  (source nil)
  (artifact-sha256 nil :type (or null string))
  (tree-sha256 nil :type (or null string)))

(defstruct locked-source
  "A locked source reference."
  (kind :tarball :type keyword)  ; :tarball, :git, :path
  (url nil :type (or null string))
  (sha256 nil :type (or null string))
  (commit nil :type (or null string))
  (path nil :type (or null string)))

;;; Parsing project manifests

(defun parse-dependency (form)
  "Parse a dependency form."
  (let ((dep (make-dependency)))
    (loop for (key val) on (cdr form) by #'cddr do
      (case key
        (:system (setf (dependency-system dep) val))
        (:constraint (setf (dependency-constraint dep) val))
        (:source (setf (dependency-source dep) val))
        (:optional (setf (dependency-optional-p dep) val))
        (:features (setf (dependency-features dep) val))))
    dep))

(defun parse-registry-ref (form)
  "Parse a registry reference form."
  (let ((ref (make-registry-ref)))
    (cond
      ((and (consp form) (eq (car form) :git))
       (setf (registry-ref-kind ref) :git)
       (loop for (key val) on (cdr form) by #'cddr do
         (case key
           (:url (setf (registry-ref-url ref) val))
           (:name (setf (registry-ref-name ref) val))
           (:trust (setf (registry-ref-trust ref) val)))))
      (t
       (error "Unknown registry format: ~S" form)))
    ref))

(defun parse-manifest (form)
  "Parse a project manifest form into a project struct."
  (unless (and (consp form) (eq (car form) :project))
    (error "Invalid manifest: must start with :project"))
  (let ((project (make-project)))
    (loop for (key val) on (cdr form) by #'cddr do
      (case key
        (:name (setf (project-name project) val))
        (:version (setf (project-version project) val))
        (:systems (setf (project-systems project) val))
        (:run (setf (project-run project) val))
        (:depends
         (setf (project-depends project)
               (mapcar #'parse-dependency val)))
        (:dev-depends
         (setf (project-dev-depends project)
               (mapcar #'parse-dependency val)))
        (:test-depends
         (setf (project-test-depends project)
               (mapcar #'parse-dependency val)))
        (:registries
         (setf (project-registries project)
               (mapcar #'parse-registry-ref val)))
        (:sbcl
         (setf (project-sbcl-constraints project) val))
        (:build
         (setf (project-build-options project) val))
        (:scripts
         (setf (project-scripts project) val))))
    project))

;;; Parsing lockfiles

(defun parse-locked-source (form)
  "Parse a locked source form."
  (let ((src (make-locked-source)))
    (case (car form)
      (:tarball
       (setf (locked-source-kind src) :tarball)
       (loop for (key val) on (cdr form) by #'cddr do
         (case key
           (:url (setf (locked-source-url src) val))
           (:sha256 (setf (locked-source-sha256 src) val)))))
      (:git
       (setf (locked-source-kind src) :git)
       (loop for (key val) on (cdr form) by #'cddr do
         (case key
           (:url (setf (locked-source-url src) val))
           (:commit (setf (locked-source-commit src) val)))))
      (:path
       (setf (locked-source-kind src) :path)
       ;; Support both:
       ;;   (:path "<path>" :tree-sha256 "...")
       ;; and
       ;;   (:path :path "<path>" :tree-sha256 "...")
       (let ((rest (cdr form)))
         (when (and rest (stringp (first rest)))
           (setf (locked-source-path src) (first rest))
           (setf rest (rest rest)))
         (loop for (key val) on rest by #'cddr do
           (case key
             (:path (setf (locked-source-path src) val))
             (:tree-sha256 (setf (locked-source-sha256 src) val)))))))
    src))

(defun parse-locked-release (form)
  "Parse a locked release form."
  (let ((rel (make-locked-release)))
    (loop for (key val) on form by #'cddr do
      (case key
        (:name (setf (locked-release-name rel) val))
        (:version (setf (locked-release-version rel) val))
        (:source (setf (locked-release-source rel) (parse-locked-source val)))
        (:artifact-sha256 (setf (locked-release-artifact-sha256 rel) val))
        (:tree-sha256 (setf (locked-release-tree-sha256 rel) val))))
    ;; Backward/forward compat: allow tree hash to live on :source for :path.
    (when (and (null (locked-release-tree-sha256 rel))
               (locked-release-source rel)
               (eq (locked-source-kind (locked-release-source rel)) :path)
               (locked-source-sha256 (locked-release-source rel)))
      (setf (locked-release-tree-sha256 rel)
            (locked-source-sha256 (locked-release-source rel))))
    rel))

(defun parse-locked-registry (form)
  "Parse a locked registry form."
  (let ((reg (make-locked-registry)))
    (loop for (key val) on (cdr form) by #'cddr do
      (case key
        (:name (setf (locked-registry-name reg) val))
        (:kind (setf (locked-registry-kind reg) val))
        (:url (setf (locked-registry-url reg) val))
        (:commit (setf (locked-registry-commit reg) val))
        (:trust (setf (locked-registry-trust reg) val))
        (:signature (setf (locked-registry-signature reg) val))))
    reg))

(defun parse-locked-system (form)
  "Parse a locked system form."
  (let ((sys (make-locked-system)))
    (loop for (key val) on (cdr form) by #'cddr do
      (case key
        (:id (setf (locked-system-id sys) val))
        (:release (setf (locked-system-release sys) (parse-locked-release val)))
        (:deps (setf (locked-system-deps sys) val))))
    sys))

(defun parse-lockfile (form)
  "Parse a lockfile form into a lockfile struct."
  (unless (and (consp form) (eq (car form) :lock))
    (error "Invalid lockfile: must start with :lock"))
  (let ((lock (make-lockfile)))
    (loop for (key val) on (cdr form) by #'cddr do
      (case key
        (:format (setf (lockfile-format lock) val))
        (:generated-at (setf (lockfile-generated-at lock) val))
        (:project
         (setf (lockfile-project-name lock) (getf val :name))
         (setf (lockfile-clpm-version lock) (getf val :clpm-version)))
        (:registries
         (setf (lockfile-registries lock)
               (mapcar #'parse-locked-registry val)))
        (:resolved
         (setf (lockfile-resolved lock)
               (mapcar #'parse-locked-system val)))))
    lock))

;;; Serialization

(defun serialize-locked-source (src)
  "Serialize a locked source to sexp form."
  (case (locked-source-kind src)
    (:tarball
     `(:tarball :url ,(locked-source-url src)
                :sha256 ,(locked-source-sha256 src)))
    (:git
     `(:git :url ,(locked-source-url src)
            :commit ,(locked-source-commit src)))
    (:path
     `(:path ,(locked-source-path src)
             :tree-sha256 ,(locked-source-sha256 src)))))

(defun serialize-locked-release (rel)
  "Serialize a locked release to sexp form."
  `(:name ,(locked-release-name rel)
    :version ,(locked-release-version rel)
    :source ,(serialize-locked-source (locked-release-source rel))
    :artifact-sha256 ,(locked-release-artifact-sha256 rel)
    :tree-sha256 ,(locked-release-tree-sha256 rel)))

(defun serialize-locked-registry (reg)
  "Serialize a locked registry to sexp form."
  `(:registry
    :name ,(locked-registry-name reg)
    :kind ,(locked-registry-kind reg)
    :url ,(locked-registry-url reg)
    :commit ,(locked-registry-commit reg)
    :trust ,(locked-registry-trust reg)
    :signature ,(locked-registry-signature reg)))

(defun serialize-locked-system (sys)
  "Serialize a locked system to sexp form."
  `(:system
    :id ,(locked-system-id sys)
    :release ,(serialize-locked-release (locked-system-release sys))
    :deps ,(locked-system-deps sys)))

(defun serialize-lockfile (lock)
  "Serialize a lockfile to canonical sexp form."
  `(:lock
    :format ,(lockfile-format lock)
    :generated-at ,(lockfile-generated-at lock)
    :project (:name ,(lockfile-project-name lock)
              :clpm-version ,(lockfile-clpm-version lock))
    :registries ,(mapcar #'serialize-locked-registry
                         (sort (copy-list (lockfile-registries lock))
                               #'string< :key #'locked-registry-name))
    :resolved ,(mapcar #'serialize-locked-system
                       (sort (copy-list (lockfile-resolved lock))
                             #'string< :key #'locked-system-id))))

;;; File I/O

(defun normalize-project-path-dependencies (project manifest-path)
  "Normalize (:path ...) dependency constraints in PROJECT relative to MANIFEST-PATH.

The manifest format allows relative paths; internally we store absolute,
directory pathnames (as strings) for determinism."
  (let* ((manifest-path (uiop:ensure-pathname manifest-path :want-file t))
         (base-dir (uiop:pathname-directory-pathname manifest-path)))
    (labels ((normalize-dep (dep)
               (let ((c (dependency-constraint dep)))
                 (when (and (consp c) (eq (car c) :path))
                   (let* ((raw (cadr c))
                          (expanded (clpm.platform:expand-path raw))
                          (pn (uiop:ensure-pathname expanded
                                                    :defaults base-dir
                                                    :want-existing nil))
                          (abs (uiop:ensure-directory-pathname pn))
                          (tru (uiop:ensure-directory-pathname (truename abs))))
                     (setf (dependency-constraint dep)
                           (list :path (namestring tru))))))
                dep))
      (setf (project-depends project) (mapcar #'normalize-dep (project-depends project))
            (project-dev-depends project) (mapcar #'normalize-dep (project-dev-depends project))
            (project-test-depends project) (mapcar #'normalize-dep (project-test-depends project))))
    project))

(defun read-project-file (path)
  "Read a clpm.project file and return a project struct."
  (let ((form (clpm.io.sexp:read-manifest path)))
    (normalize-project-path-dependencies (parse-manifest form) path)))

(defun write-project-file (project path)
  "Write a project struct to a clpm.project file."
  (labels ((serialize-dep (d)
             (let ((form `(:dep
                           :system ,(dependency-system d)
                           :constraint ,(dependency-constraint d))))
               (when (dependency-source d)
                 (setf form (append form (list :source (dependency-source d)))))
               (when (dependency-optional-p d)
                 (setf form (append form (list :optional t))))
               (when (dependency-features d)
                 (setf form (append form (list :features (dependency-features d)))))
               form))
           (serialize-registry (r)
             `(,(registry-ref-kind r)
               :url ,(registry-ref-url r)
               :name ,(registry-ref-name r)
               :trust ,(registry-ref-trust r))))
    (let ((form `(:project
                  :name ,(project-name project)
                  :version ,(project-version project)
                  :systems ,(project-systems project)
                  ,@(when (project-run project)
                      (list :run (project-run project)))
                  :depends ,(mapcar #'serialize-dep (project-depends project))
                  :dev-depends ,(mapcar #'serialize-dep (project-dev-depends project))
                  :test-depends ,(mapcar #'serialize-dep (project-test-depends project))
                  :registries ,(mapcar #'serialize-registry (project-registries project))
                  :sbcl ,(project-sbcl-constraints project)
                  :build ,(project-build-options project)
                  :scripts ,(project-scripts project))))
      (clpm.io.sexp:write-canonical-sexp-to-file form path))))

(defun read-lock-file (path)
  "Read a clpm.lock file and return a lockfile struct."
  (let ((form (clpm.io.sexp:read-lockfile path)))
    (parse-lockfile form)))

(defun write-lock-file (lock path)
  "Write a lockfile struct to a clpm.lock file."
  (clpm.io.sexp:write-canonical-sexp-to-file (serialize-lockfile lock) path))

;;; Project discovery

(defun find-project-root (&optional (start (uiop:getcwd)))
  "Find the project root containing clpm.project.
Searches upward from START directory.
Returns (values project-root manifest-path lock-path) or nil."
  (let ((dir (uiop:ensure-directory-pathname start)))
    (loop
      (let ((manifest (merge-pathnames "clpm.project" dir))
            (lock (merge-pathnames "clpm.lock" dir)))
        (when (uiop:file-exists-p manifest)
          (return (values dir manifest
                          (when (uiop:file-exists-p lock) lock)))))
      (let ((parent (uiop:pathname-parent-directory-pathname dir)))
        (when (or (null parent) (equal parent dir))
          (return nil))
        (setf dir parent)))))

;;; RFC 3339 timestamp

(defun rfc3339-timestamp ()
  "Return current time as RFC 3339 string."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour min sec)))
