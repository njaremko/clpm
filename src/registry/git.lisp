;;;; registry/git.lisp - Git-based registry handling

(in-package #:clpm.registry)

;; Forward declarations (defined in registry/quicklisp.lisp).
(declaim (ftype (function (&rest t) t)
                clone-quicklisp-registry
                update-quicklisp-registry
                load-quicklisp-registry-snapshot))

;;; Registry structures

(defstruct registry
  "A CLPM registry."
  (name nil :type (or null string))
  (kind :git :type keyword)
  (url nil :type (or null string))
  (path nil :type (or null pathname))  ; local path to cloned registry
  (trust-key nil :type (or null string))
  (snapshot-sig-sha256 nil :type (or null string))
  (snapshot nil)
  (release-table (make-hash-table :test 'equal)))

(defstruct snapshot
  "A registry snapshot."
  (format 1 :type integer)
  (generated-at nil :type (or null string))
  (releases nil :type list)
  (provides nil :type list))  ; alist of (system-id . "package@version")

(defstruct release-metadata
  "Release metadata from registry."
  (name nil :type (or null string))
  (version nil :type (or null string))
  (source nil)  ; (:tarball :url ... :sha256 ...) or (:git ...)
  (artifact-sha256 nil :type (or null string))
  (systems nil :type list)  ; list of system-id strings
  (system-deps nil :type list)  ; alist of (system-id . ((dep-system . constraint) ...))
  (native-requires nil :type list)
  (license nil :type (or null string))
  (homepage nil :type (or null string))
  (description nil :type (or null string)))

;;; Registry paths

(defun registry-local-path (name)
  "Return local path for registry with given name."
  (merge-pathnames (format nil "~A/" name)
                   (clpm.platform:registry-dir)))

;;; Git operations

(defun git-clone (url dest)
  "Clone git repository from URL to DEST."
  (let ((git (clpm.platform:find-git)))
    (unless git
      (error 'clpm.errors:clpm-fetch-error
             :message "git not found in PATH"
             :url url))
    (multiple-value-bind (output error-output exit-code)
        (clpm.platform:run-program
         (list git "clone" "--depth" "1" url (namestring dest)))
      (declare (ignore output))
      (unless (zerop exit-code)
        (error 'clpm.errors:clpm-fetch-error
               :message (format nil "git clone failed: ~A" error-output)
               :url url
               :status exit-code)))))

(defun git-pull (repo-path)
  "Pull updates in repository at REPO-PATH."
  (let ((git (clpm.platform:find-git)))
    (unless git
      (error 'clpm.errors:clpm-fetch-error
             :message "git not found in PATH"))
    (multiple-value-bind (output error-output exit-code)
        (clpm.platform:run-program
         (list git "pull" "--ff-only")
         :directory repo-path)
      (declare (ignore output))
      (unless (zerop exit-code)
        (error 'clpm.errors:clpm-fetch-error
               :message (format nil "git pull failed: ~A" error-output)
               :status exit-code)))))

(defun git-rev-parse (repo-path &optional (ref "HEAD"))
  "Get full commit SHA for REF in repository at REPO-PATH."
  (let ((git (clpm.platform:find-git)))
    (unless git
      (return-from git-rev-parse nil))
    (multiple-value-bind (output error-output exit-code)
        (clpm.platform:run-program
         (list git "rev-parse" ref)
         :directory repo-path)
      (declare (ignore error-output))
      (when (zerop exit-code)
        (string-trim '(#\Space #\Newline #\Return) output)))))

(defun git-checkout (repo-path ref)
  "Checkout REF (commit/tag/branch) in repository at REPO-PATH."
  (let ((git (clpm.platform:find-git)))
    (unless git
      (error 'clpm.errors:clpm-fetch-error
             :message "git not found in PATH"))
    (multiple-value-bind (output error-output exit-code)
        (clpm.platform:run-program
         (list git "checkout" ref)
         :directory repo-path)
      (declare (ignore output))
      (unless (zerop exit-code)
        (error 'clpm.errors:clpm-fetch-error
               :message (format nil "git checkout failed: ~A" error-output)
               :status exit-code)))))

;;; Registry operations

(defun clone-registry (name url &key trust-key (kind :git))
  "Clone or load a registry from URL.

KIND is one of:
- :git       A CLPM git registry.
- :quicklisp A Quicklisp dist registry (distinfo URL).

Returns a registry struct."
  (let* ((local-path (registry-local-path name))
         (reg (make-registry :name name
                             :kind kind
                             :url url
                             :path local-path
                             :trust-key trust-key)))
    (case kind
      (:git
       (if (uiop:directory-exists-p local-path)
           ;; Already exists, just load
           (load-registry-snapshot reg)
           ;; Clone fresh
           (progn
             (ensure-directories-exist local-path)
             (git-clone url local-path)
             (load-registry-snapshot reg))))
      (:quicklisp
       ;; Download dist metadata if missing, then load.
       (clone-quicklisp-registry reg))
      (t
       (error 'clpm.errors:clpm-parse-error
              :message (format nil "Unknown registry kind: ~S" kind))))
    reg))

(defun update-registry (registry)
  "Update a registry by pulling latest changes.
Returns updated registry."
  (case (registry-kind registry)
    (:git
     (let ((local-path (registry-local-path (registry-name registry))))
       (unless (uiop:directory-exists-p local-path)
         (error 'clpm.errors:clpm-fetch-error
                :message (format nil "Registry ~A not cloned" (registry-name registry))))
       (git-pull local-path)
       (load-registry-snapshot registry)
       registry))
    (:quicklisp
     (update-quicklisp-registry registry))
    (t
     (error 'clpm.errors:clpm-parse-error
            :message (format nil "Unknown registry kind: ~S" (registry-kind registry))))))

(defun load-registry (name)
  "Load an already-cloned registry by name.
Returns registry struct or nil if not found."
  (let ((local-path (registry-local-path name)))
    (when (uiop:directory-exists-p local-path)
      (let ((reg (make-registry :name name
                                :kind :git
                                :path local-path)))
        (load-registry-snapshot reg)
        reg))))

;;; Snapshot parsing

(defun load-registry-snapshot (registry)
  "Load and parse the snapshot for REGISTRY.
Modifies REGISTRY in place."
  (case (registry-kind registry)
    (:git
     (let* ((local-path (registry-local-path (registry-name registry)))
            (snapshot-path (merge-pathnames "registry/snapshot.sxp" local-path)))
       (unless (uiop:file-exists-p snapshot-path)
         (error 'clpm.errors:clpm-parse-error
                :message "Snapshot file not found"
                :file snapshot-path))
       ;; Verify signature if trust key is set
       (when (registry-trust-key registry)
         (if (and (boundp 'clpm.commands:*insecure*)
                  (symbol-value 'clpm.commands:*insecure*))
             (progn
               (setf (registry-snapshot-sig-sha256 registry) nil)
               (clpm.commands::log-info
                "WARNING: --insecure: skipping snapshot signature verification for registry ~A"
                (registry-name registry)))
             (setf (registry-snapshot-sig-sha256 registry)
                   (verify-snapshot-signature local-path (registry-trust-key registry)))))
       ;; Parse snapshot
       (let ((form (clpm.io.sexp:read-registry-snapshot snapshot-path)))
         (setf (registry-snapshot registry) (parse-snapshot form)))
       registry))
    (:quicklisp
     (load-quicklisp-registry-snapshot registry))
    (t
     (error 'clpm.errors:clpm-parse-error
            :message (format nil "Unknown registry kind: ~S" (registry-kind registry))))))

(defun parse-snapshot (form)
  "Parse a snapshot form into a snapshot struct."
  (let ((snap (make-snapshot)))
    (loop for (key val) on (cdr form) by #'cddr do
      (case key
        (:format (setf (snapshot-format snap) val))
        (:generated-at (setf (snapshot-generated-at snap) val))
        (:releases (setf (snapshot-releases snap) val))
        (:provides (setf (snapshot-provides snap) val))))
    snap))

(defun read-file-bytes (path)
  "Read PATH as an octet vector."
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((data (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence data s)
      data)))

(defun resolve-trust-key-path (repo-path trust-key)
  "Return (values key-id key-path) for TRUST-KEY and REPO-PATH."
  (let* ((key-id-info (clpm.crypto.ed25519:parse-key-id trust-key))
         (key-id (cdr key-id-info))
         (key-path (merge-pathnames (format nil "~A.pub" key-id)
                                    (clpm.platform:keys-dir))))
    (unless (uiop:file-exists-p key-path)
      ;; Try embedded key in registry
      (setf key-path (merge-pathnames (format nil "registry/keys/~A.pub" key-id)
                                      repo-path)))
    (unless (uiop:file-exists-p key-path)
      (error 'clpm.errors:clpm-signature-error
             :message "Public key not found"
             :key-id key-id))
    (values key-id key-path)))

(defun signature-sha256-hex (signature)
  "Compute SHA-256 hex digest of Ed25519 SIGNATURE bytes."
  (clpm.crypto.sha256:bytes-to-hex
   (clpm.crypto.sha256:sha256 (clpm.crypto.ed25519::signature-bytes signature))))

(defun verify-registry-file-signature (repo-path file-path sig-path trust-key)
  "Verify FILE-PATH against SIG-PATH using TRUST-KEY.

Returns the SHA-256 hex digest of the detached signature bytes on success.
Signals CLPM-SIGNATURE-ERROR on any failure."
  (unless (uiop:file-exists-p sig-path)
    (error 'clpm.errors:clpm-signature-error
           :message "Signature file not found"
           :file sig-path))
  (multiple-value-bind (key-id key-path)
      (resolve-trust-key-path repo-path trust-key)
    (handler-case
        (let* ((key (clpm.crypto.ed25519:load-public-key key-path))
               (sig (clpm.crypto.ed25519::read-detached-signature sig-path))
               (msg (read-file-bytes file-path)))
          (unless key
            (error 'clpm.errors:clpm-signature-error
                   :message "Could not load public key"
                   :file key-path
                   :key-id key-id))
          (unless (clpm.crypto.ed25519:verify-signature msg sig key)
            (error 'clpm.errors:clpm-signature-error
                   :message "Signature verification failed"
                   :file file-path
                   :key-id key-id))
          (signature-sha256-hex sig))
      (clpm.errors:clpm-signature-error (c)
        (error c))
      (error (c)
        (error 'clpm.errors:clpm-signature-error
               :message (format nil "~A" c)
               :file file-path
               :key-id key-id)))))

(defun verify-snapshot-signature (repo-path trust-key)
  "Verify snapshot signature.

Returns the SHA-256 hex digest of the detached signature bytes on success.
Signals CLPM-SIGNATURE-ERROR if invalid."
  (let ((snapshot-path (merge-pathnames "registry/snapshot.sxp" repo-path))
        (sig-path (merge-pathnames "registry/snapshot.sig" repo-path)))
    (verify-registry-file-signature repo-path snapshot-path sig-path trust-key)))

(defun verify-release-metadata-signature (repo-path package-name version trust-key)
  "Verify release metadata signature for PACKAGE-NAME@VERSION.

Signals CLPM-SIGNATURE-ERROR if invalid."
  (let ((release-path (merge-pathnames
                       (format nil "registry/packages/~A/~A/release.sxp"
                               package-name version)
                       repo-path))
        (sig-path (merge-pathnames
                   (format nil "registry/packages/~A/~A/release.sig"
                           package-name version)
                   repo-path)))
    (verify-registry-file-signature repo-path release-path sig-path trust-key)
    t))

;;; Release metadata loading

(defun get-release-metadata (registry package-name version)
  "Get release metadata for PACKAGE-NAME@VERSION from REGISTRY."
  (case (registry-kind registry)
    (:git
     (let* ((local-path (registry-local-path (registry-name registry)))
            (release-path (merge-pathnames
                           (format nil "registry/packages/~A/~A/release.sxp"
                                   package-name version)
                           local-path)))
       (unless (uiop:file-exists-p release-path)
         (return-from get-release-metadata nil))
       (when (registry-trust-key registry)
         (unless (and (boundp 'clpm.commands:*insecure*)
                      (symbol-value 'clpm.commands:*insecure*))
           (verify-release-metadata-signature local-path package-name version
                                              (registry-trust-key registry))))
       (let ((form (clpm.io.sexp:read-release-metadata release-path)))
         (parse-release-metadata form))))
    (:quicklisp
     (let* ((release-ref (format nil "~A@~A" package-name version))
            (meta (gethash release-ref (registry-release-table registry))))
       meta))
    (t
     (error 'clpm.errors:clpm-parse-error
            :message (format nil "Unknown registry kind: ~S" (registry-kind registry))))))

(defun parse-release-source (form)
  "Parse a release source form."
  (case (car form)
    (:tarball
     (list :tarball
           :url (getf (cdr form) :url)
           :sha256 (getf (cdr form) :sha256)
           :sha1 (getf (cdr form) :sha1)))
    (:git
     (list :git
           :url (getf (cdr form) :url)
           :commit (getf (cdr form) :commit)))
    (t form)))

(defun parse-release-metadata (form)
  "Parse a release metadata form."
  (let ((rel (make-release-metadata)))
    (loop for (key val) on (cdr form) by #'cddr do
      (case key
        (:name (setf (release-metadata-name rel) val))
        (:version (setf (release-metadata-version rel) val))
        (:source (setf (release-metadata-source rel) (parse-release-source val)))
        (:artifact-sha256 (setf (release-metadata-artifact-sha256 rel) val))
        (:systems (setf (release-metadata-systems rel) val))
        (:system-deps (setf (release-metadata-system-deps rel) val))
        (:native-requires (setf (release-metadata-native-requires rel) val))
        (:license (setf (release-metadata-license rel) val))
        (:homepage (setf (release-metadata-homepage rel) val))
        (:description (setf (release-metadata-description rel) val))))
    rel))

;;; System candidate lookup

(defun find-system-candidates (registry system-id)
  "Find all releases providing SYSTEM-ID in REGISTRY.
Returns list of (package-name . version) pairs."
  (let ((snapshot (registry-snapshot registry))
        (candidates '()))
    (when snapshot
      (dolist (entry (snapshot-provides snapshot))
        (when (string-equal (car entry) system-id)
          (let ((release-ref (cdr entry)))
            ;; Parse "package@version" format
            (let ((at-pos (position #\@ release-ref)))
              (when at-pos
                (push (cons (subseq release-ref 0 at-pos)
                            (subseq release-ref (1+ at-pos)))
                      candidates)))))))
    (nreverse candidates)))

(defun find-all-candidates (registries system-id)
  "Find candidates for SYSTEM-ID across all REGISTRIES.
Returns alist of (registry . ((package . version) ...))."
  (loop for reg in registries
        for candidates = (find-system-candidates reg system-id)
        when candidates
          collect (cons reg candidates)))

;;; Build index for solver

(defstruct registry-index
  "Index of registry contents for efficient lookup."
  (system->candidates (make-hash-table :test 'equal))
  (release->metadata (make-hash-table :test 'equal)))

(defun build-registry-index (registries)
  "Build an index from multiple registries for solver use."
  (let ((index (make-registry-index)))
    (dolist (reg registries)
      (let ((snapshot (registry-snapshot reg)))
        (when snapshot
          ;; Index provides
          (dolist (entry (snapshot-provides snapshot))
            (let ((system-id (car entry))
                  (release-ref (cdr entry)))
              (push (cons reg release-ref)
                    (gethash system-id (registry-index-system->candidates index)))))
          ;; Index releases
          (dolist (release-ref (snapshot-releases snapshot))
            (let ((at-pos (position #\@ release-ref)))
              (when at-pos
                (let* ((pkg (subseq release-ref 0 at-pos))
                       (ver (subseq release-ref (1+ at-pos)))
                       (meta (get-release-metadata reg pkg ver)))
                  (when meta
                    (setf (gethash release-ref
                                   (registry-index-release->metadata index))
                          (cons reg meta))))))))))
    index))

(defun index-lookup-system (index system-id)
  "Look up candidates for SYSTEM-ID in INDEX.
Returns list of (registry . release-ref) pairs."
  (gethash system-id (registry-index-system->candidates index)))

(defun index-lookup-release (index release-ref)
  "Look up metadata for RELEASE-REF in INDEX.
Returns (registry . release-metadata) or nil."
  (gethash release-ref (registry-index-release->metadata index)))
