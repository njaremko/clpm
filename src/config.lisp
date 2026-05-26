;;;; config.lisp - Global CLPM configuration

(in-package #:clpm.config)

(defstruct config
  "Global CLPM configuration."
  (format 1 :type integer)
  (registries nil :type list) ; list of clpm.project:registry-ref
  (defaults nil :type list))  ; plist of defaults (e.g. :build ...)

(defun config-path ()
  "Return the path to the global config file."
  (merge-pathnames "config.sxp" (clpm.platform:config-dir)))

(defun %config-lock-path ()
  "Return the lock file co-located with the global config."
  (merge-pathnames "config.sxp.lock" (clpm.platform:config-dir)))

(defun parse-registry-ref (form)
  "Parse a registry reference FORM into a clpm.project:registry-ref."
  (let ((ref (clpm.project::make-registry-ref)))
    (cond
      ((and (consp form) (eq (car form) :git))
       (setf (clpm.project:registry-ref-kind ref) :git)
       (loop for (key val) on (cdr form) by #'cddr do
         (case key
           (:url (setf (clpm.project:registry-ref-url ref) val))
           (:name (setf (clpm.project:registry-ref-name ref) val))
           (:trust (setf (clpm.project:registry-ref-trust ref) val)))))
      ((and (consp form) (eq (car form) :quicklisp))
       (setf (clpm.project:registry-ref-kind ref) :quicklisp)
       (loop for (key val) on (cdr form) by #'cddr do
         (case key
           (:url (setf (clpm.project:registry-ref-url ref) val))
           (:name (setf (clpm.project:registry-ref-name ref) val))
           (:trust (setf (clpm.project:registry-ref-trust ref) val))
           (:systems-sha256 (setf (clpm.project:registry-ref-quicklisp-systems-sha256 ref) val))
           (:releases-sha256 (setf (clpm.project:registry-ref-quicklisp-releases-sha256 ref) val)))))
      (t
       (error 'clpm.errors:clpm-parse-error
              :message (format nil "Unknown registry format: ~S" form))))
    ref))

(defun serialize-registry-ref (ref)
  "Serialize REF to a config registry form."
  `(,(clpm.project:registry-ref-kind ref)
    :url ,(clpm.project:registry-ref-url ref)
    :name ,(clpm.project:registry-ref-name ref)
    :trust ,(clpm.project:registry-ref-trust ref)
    ,@(when (and (eq (clpm.project:registry-ref-kind ref) :quicklisp)
                 (clpm.project:registry-ref-quicklisp-systems-sha256 ref))
        (list :systems-sha256 (clpm.project:registry-ref-quicklisp-systems-sha256 ref)))
    ,@(when (and (eq (clpm.project:registry-ref-kind ref) :quicklisp)
                 (clpm.project:registry-ref-quicklisp-releases-sha256 ref))
        (list :releases-sha256 (clpm.project:registry-ref-quicklisp-releases-sha256 ref)))))

(defun parse-config (form)
  "Parse a config FORM into a config struct."
  (unless (and (consp form) (eq (car form) :config))
    (error 'clpm.errors:clpm-parse-error
           :message "Config must start with :config"))
  (let ((cfg (make-config)))
    (loop for (key val) on (cdr form) by #'cddr do
      (case key
        (:format (setf (config-format cfg) val))
        (:registries (setf (config-registries cfg)
                           (mapcar #'parse-registry-ref val)))
        (:defaults (setf (config-defaults cfg) val))))
    cfg))

(defun %read-config-unlocked ()
  (let ((path (config-path)))
    (if (uiop:file-exists-p path)
        (parse-config (clpm.io.sexp:read-safe-sexp-from-file path))
        (make-config :format 1 :registries nil :defaults nil))))

(defun read-config ()
  "Read the global config file under a shared lock.

If absent, returns an empty config."
  (clpm.platform:with-file-lock ((%config-lock-path) )
    (%read-config-unlocked)))

(defun %write-config-unlocked (cfg)
  (let* ((path (config-path))
         (registries (sort (copy-list (config-registries cfg))
                           #'string<
                           :key (lambda (r)
                                  (or (clpm.project:registry-ref-name r) ""))))
         (form `(:config
                 :format ,(config-format cfg)
                 :registries ,(mapcar #'serialize-registry-ref registries)
                 :defaults ,(config-defaults cfg)))
         (tmp-path (make-pathname :type "tmp" :defaults path)))
    (ensure-directories-exist path)
    (clpm.io.sexp:write-canonical-sexp-to-file form tmp-path)
    (rename-file tmp-path path)))

(defun write-config (cfg)
  "Write CFG to the global config file in canonical format.

Acquires an exclusive lock and writes atomically (tmp + rename). Callers that
read-modify-write should prefer `update-config` to keep the read and write
inside a single critical section."
  (clpm.platform:with-file-lock ((%config-lock-path) )
    (%write-config-unlocked cfg)))

(defun update-config (mutator)
  "Read the global config under an exclusive lock, call MUTATOR with the config
struct, then write the (possibly mutated) struct back atomically.

MUTATOR may either mutate the struct in place or return a new struct; the
return value is what is written. Returns the written struct."
  (clpm.platform:with-file-lock ((%config-lock-path) )
    (let* ((cfg (%read-config-unlocked))
           (result (funcall mutator cfg))
           (out (or result cfg)))
      (%write-config-unlocked out)
      out)))

(defun plist-merge (base override)
  "Return a plist that is BASE with OVERRIDE keys applied, purely functionally."
  (labels ((remove-from-plist (plist target-key)
             (loop for (k v) on plist by #'cddr
                   unless (eq k target-key)
                     append (list k v)))
           (merge-lists (b o)
             (cond
               ((null o) b)
               (t
                (let* ((key (first o))
                       (val (second o))
                       (rest-o (cddr o))
                       (rest-b (remove-from-plist b key)))
                  (merge-lists (list* key val rest-b) rest-o))))))
    (merge-lists (copy-list (or base '())) (or override '()))))

(defun merge-project-config (project &key (config (read-config)))
  "Merge CONFIG with PROJECT settings.

Returns (values registries build-options lisp).

Merge rules:
- project registries append after global registries
- project build options override config defaults
- project :lisp overrides config defaults :lisp; if neither is set the
  returned lisp value is NIL and callers fall back to :sbcl"
  (let* ((merged-registries (append (config-registries config)
                                    (clpm.project:project-registries project)))
         (global-build (getf (config-defaults config) :build))
         (project-build (clpm.project:project-build-options project))
         (merged-build (plist-merge global-build project-build))
         (global-lisp (getf (config-defaults config) :lisp))
         (project-lisp (clpm.project:project-lisp project))
         (merged-lisp (or project-lisp global-lisp)))
    (values merged-registries merged-build merged-lisp)))
