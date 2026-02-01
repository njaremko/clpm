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
           (:trust (setf (clpm.project:registry-ref-trust ref) val)))))
      (t
       (error 'clpm.errors:clpm-parse-error
              :message (format nil "Unknown registry format: ~S" form))))
    ref))

(defun serialize-registry-ref (ref)
  "Serialize REF to a config registry form."
  `(,(clpm.project:registry-ref-kind ref)
    :url ,(clpm.project:registry-ref-url ref)
    :name ,(clpm.project:registry-ref-name ref)
    :trust ,(clpm.project:registry-ref-trust ref)))

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

(defun read-config ()
  "Read the global config file.

If absent, returns an empty config."
  (let ((path (config-path)))
    (if (uiop:file-exists-p path)
        (parse-config (clpm.io.sexp:read-safe-sexp-from-file path))
        (make-config :format 1 :registries nil :defaults nil))))

(defun write-config (cfg)
  "Write CFG to the global config file in canonical format."
  (let* ((path (config-path))
         (registries (sort (copy-list (config-registries cfg))
                           #'string<
                           :key (lambda (r)
                                  (or (clpm.project:registry-ref-name r) ""))))
         (form `(:config
                 :format ,(config-format cfg)
                 :registries ,(mapcar #'serialize-registry-ref registries)
                 :defaults ,(config-defaults cfg))))
    (ensure-directories-exist path)
    (clpm.io.sexp:write-canonical-sexp-to-file form path)))

(defun plist-merge (base override)
  "Return a plist that is BASE with OVERRIDE keys applied."
  (let ((out (copy-list (or base '()))))
    (loop for (k v) on (or override '()) by #'cddr do
      (setf (getf out k) v))
    out))

(defun merge-project-config (project &key (config (read-config)))
  "Merge CONFIG with PROJECT settings.

Returns (values registries build-options).
Merge rules:
- project registries append after global registries
- project build options override config defaults"
  (let* ((merged-registries (append (config-registries config)
                                    (clpm.project:project-registries project)))
         (global-build (getf (config-defaults config) :build))
         (project-build (clpm.project:project-build-options project))
         (merged-build (plist-merge global-build project-build)))
    (values merged-registries merged-build)))
