;;;; store.lisp - Content-addressed storage

(in-package #:clpm.store)

;;; Store paths

(defun %hex-char-p (char)
  (or (and (char>= char #\0) (char<= char #\9))
      (and (char>= char #\a) (char<= char #\f))
      (and (char>= char #\A) (char<= char #\F))))

(defun %sha256-hex-digest (value kind)
  (unless (and (stringp value)
               (= 64 (length value))
               (every #'%hex-char-p value))
    (error 'clpm.errors:clpm-user-error
           :message (format nil "Invalid ~A: expected 64 hexadecimal SHA-256 digest, got ~S"
                            kind
                            value)))
  (string-downcase value))

(defun source-path (tree-sha256)
  "Return path for source tree with given hash."
  (let ((tree-sha256 (%sha256-hex-digest tree-sha256 "source tree identity")))
    (uiop:merge-pathnames*
     (make-pathname :directory `(:relative "sources" "sha256" ,tree-sha256))
     (clpm.platform:store-dir))))

(defun artifact-path (artifact-sha256)
  "Return path for artifact with given hash."
  (let ((artifact-sha256 (%sha256-hex-digest artifact-sha256 "artifact identity")))
    (uiop:merge-pathnames*
     (make-pathname :directory '(:relative "artifacts" "sha256")
                    :name artifact-sha256)
     (clpm.platform:store-dir))))

(defun %artifact-lock-path (artifact-sha256)
  (let ((artifact-sha256 (%sha256-hex-digest artifact-sha256 "artifact identity")))
    (uiop:merge-pathnames*
     (make-pathname :directory '(:relative "artifacts" "sha256")
                    :name (format nil ".~A" artifact-sha256)
                    :type "lock")
     (clpm.platform:store-dir))))

(defun %artifact-temp-path (artifact-sha256)
  (let ((artifact-sha256 (%sha256-hex-digest artifact-sha256 "artifact identity")))
    (uiop:merge-pathnames*
     (make-pathname :directory '(:relative "artifacts" "sha256")
                    :name (format nil ".~A.~D" artifact-sha256 (random (expt 2 32)))
                    :type "tmp")
     (clpm.platform:store-dir))))

(defun build-path (build-id)
  "Return path for build with given ID."
  (let ((build-id (%sha256-hex-digest build-id "build identity")))
    (uiop:merge-pathnames*
     (make-pathname :directory `(:relative "builds" ,build-id))
     (clpm.platform:store-dir))))

(defun %source-lock-path (tree-sha256)
  (let ((tree-sha256 (%sha256-hex-digest tree-sha256 "source tree identity")))
    (uiop:merge-pathnames*
     (make-pathname :directory '(:relative "sources" "sha256")
                    :name (format nil ".~A" tree-sha256)
                    :type "lock")
     (clpm.platform:store-dir))))

(defun %source-src-path (source-root)
  (uiop:merge-pathnames*
   (make-pathname :directory '(:relative "src"))
   source-root))

(defun %source-meta-path (source-root)
  (make-pathname :name "meta" :type "sxp" :defaults source-root))

(defun tmp-path ()
  "Return temp directory path."
  (uiop:merge-pathnames*
   (make-pathname :directory `(:relative "tmp" ,(format nil "~A" (random (expt 2 32)))))
   (clpm.platform:store-dir)))

;;; Existence checks

(defun %artifact-complete-p (artifact-sha256 path)
  (and (uiop:file-exists-p path)
       (handler-case
           (string= (%sha256-hex-digest artifact-sha256 "artifact identity")
                    (clpm.crypto.sha256:bytes-to-hex
                     (clpm.crypto.sha256:sha256-file path)))
         (error () nil))))

(defun %source-meta-complete-p (tree-sha256 meta-path)
  (and (uiop:file-exists-p meta-path)
       (handler-case
           (let ((form (clpm.io.sexp:read-safe-sexp-from-file meta-path)))
             (and (consp form)
                  (eq (car form) :source)
                  (string= tree-sha256 (getf (cdr form) :tree-sha256))))
         (error () nil))))

(defun %source-complete-p (tree-sha256 path)
  (let ((tree-sha256 (%sha256-hex-digest tree-sha256 "source tree identity")))
    (and (uiop:directory-exists-p path)
         (uiop:directory-exists-p (%source-src-path path))
         (%source-meta-complete-p tree-sha256 (%source-meta-path path)))))

(defun source-exists-p (tree-sha256)
  "Check if source tree exists in store."
  (%source-complete-p tree-sha256 (source-path tree-sha256)))

(defun artifact-exists-p (artifact-sha256)
  "Check if artifact exists in store."
  (let ((artifact-sha256 (%sha256-hex-digest artifact-sha256 "artifact identity")))
    (%artifact-complete-p artifact-sha256 (artifact-path artifact-sha256))))

(defun build-exists-p (build-id)
  "Check if build exists in store."
  (let ((path (build-path build-id)))
    (and (uiop:directory-exists-p path)
         (uiop:file-exists-p (merge-pathnames "manifest.sxp" path)))))

;;; Store artifacts

(defun %rename-file-exact (tmp-path dest-path)
  #+(and sbcl unix)
  (sb-posix:rename (namestring tmp-path) (namestring dest-path))
  #-(and sbcl unix)
  (error 'clpm.errors:clpm-user-error
         :message "Atomic store publication requires SBCL on Unix")
  dest-path)

(defun %store-artifact-bytes (bytes artifact-sha256 dest-path)
  (let ((tmp-path (%artifact-temp-path artifact-sha256)))
    (unwind-protect
         (progn
           (ensure-directories-exist tmp-path)
           (with-open-file (s tmp-path :direction :output
                                       :element-type '(unsigned-byte 8)
                                       :if-exists :supersede)
             (write-sequence bytes s))
           (%rename-file-exact tmp-path dest-path))
      (when (uiop:file-exists-p tmp-path)
        (ignore-errors (delete-file tmp-path))))))

(defun %store-artifact-file (path artifact-sha256 dest-path)
  (let ((tmp-path (%artifact-temp-path artifact-sha256)))
    (unwind-protect
         (progn
           (ensure-directories-exist tmp-path)
           (uiop:copy-file path tmp-path)
           (let ((copied-hash
                   (clpm.crypto.sha256:bytes-to-hex
                    (clpm.crypto.sha256:sha256-file tmp-path))))
             (unless (string-equal copied-hash artifact-sha256)
               (error 'clpm.errors:clpm-hash-mismatch-error
                      :expected artifact-sha256
                      :actual copied-hash
                      :artifact "artifact copy")))
           (%rename-file-exact tmp-path dest-path))
      (when (uiop:file-exists-p tmp-path)
        (ignore-errors (delete-file tmp-path))))))

(defun %ensure-artifact (artifact-sha256 dest-path writer)
  (clpm.platform:with-file-lock ((%artifact-lock-path artifact-sha256))
    (unless (%artifact-complete-p artifact-sha256 dest-path)
      (funcall writer))
    dest-path))

(defun store-artifact (data expected-sha256)
  "Store artifact data in content-addressed store.
DATA is a byte vector or a path to a file.
Returns path where artifact was stored.
Signals error if hash doesn't match."
  (etypecase data
    ((array (unsigned-byte 8) (*))
     (let* ((source-bytes data)
            (actual-hash (clpm.crypto.sha256:bytes-to-hex
                          (clpm.crypto.sha256:sha256 source-bytes)))
            (dest-path (artifact-path actual-hash)))
       ;; Verify hash
       (unless (string-equal actual-hash expected-sha256)
         (error 'clpm.errors:clpm-hash-mismatch-error
                :expected expected-sha256
                :actual actual-hash
                :artifact "artifact"))
       (%ensure-artifact
        actual-hash
        dest-path
        (lambda ()
          (%store-artifact-bytes source-bytes actual-hash dest-path)))))
    ((or pathname string)
     (let* ((actual-hash (clpm.crypto.sha256:bytes-to-hex
                          (clpm.crypto.sha256:sha256-file data)))
            (dest-path (artifact-path actual-hash)))
       ;; Verify hash
       (unless (string-equal actual-hash expected-sha256)
         (error 'clpm.errors:clpm-hash-mismatch-error
                :expected expected-sha256
                :actual actual-hash
                :artifact "artifact"))
       (%ensure-artifact
        actual-hash
        dest-path
        (lambda ()
          (%store-artifact-file data actual-hash dest-path)))))))

(defun %write-source-meta (dest-path tree-sha256)
  (let ((meta-path (%source-meta-path dest-path))
        (tmp-path (merge-pathnames "meta.sxp.tmp" dest-path)))
    (unwind-protect
         (progn
           (clpm.io.sexp:write-canonical-sexp-to-file
            `(:source
              :tree-sha256 ,tree-sha256
              :stored-at ,(get-universal-time))
            tmp-path)
           (%rename-file-exact tmp-path meta-path))
      (when (uiop:file-exists-p tmp-path)
        (ignore-errors (delete-file tmp-path))))))

(defun store-source (source-dir expected-tree-sha256)
  "Store source directory in content-addressed store.
SOURCE-DIR is a pathname to the directory.
Returns path where source was stored.
Signals error if tree hash doesn't match."
  (let* ((actual-hash (clpm.crypto.sha256:bytes-to-hex
                       (clpm.crypto.sha256:sha256-tree source-dir)))
         (dest-path (source-path actual-hash)))
    ;; Verify hash
    (when expected-tree-sha256
      (unless (string-equal actual-hash expected-tree-sha256)
        (error 'clpm.errors:clpm-hash-mismatch-error
               :expected expected-tree-sha256
               :actual actual-hash
               :artifact "source tree")))
    (clpm.platform:with-file-lock ((%source-lock-path actual-hash))
      (unless (%source-complete-p actual-hash dest-path)
        (when (uiop:directory-exists-p dest-path)
          (uiop:delete-directory-tree dest-path :validate t))
        (handler-case
            (let ((src-dir (%source-src-path dest-path)))
              (ensure-directories-exist src-dir)
              (copy-directory-tree source-dir src-dir)
              (%write-source-meta dest-path actual-hash))
          (error (c)
            (when (uiop:directory-exists-p dest-path)
              (ignore-errors
               (uiop:delete-directory-tree dest-path :validate t)))
            (error c)))))
    (values dest-path actual-hash)))

(defun copy-directory-tree (source dest)
  "Copy directory tree from SOURCE to DEST."
  (let ((source (uiop:ensure-directory-pathname source))
        (dest (uiop:ensure-directory-pathname dest)))
    (ensure-directories-exist dest)
    (dolist (entry (clpm.io.fs:walk-files source))
      (let* ((rel-path (car entry))
             (src-path (cdr entry))
             (dest-path (merge-pathnames rel-path dest)))
        (ensure-directories-exist dest-path)
        (uiop:copy-file src-path dest-path)))))

;;; Get paths

(defun get-source-path (tree-sha256)
  "Get path to source directory for given tree hash.
Returns nil if not in store."
  (let ((root (source-path tree-sha256)))
    (when (%source-complete-p tree-sha256 root)
      (%source-src-path root))))

(defun get-artifact-path (artifact-sha256)
  "Get path to artifact file for given hash.
Returns nil if not in store."
  (let ((path (artifact-path artifact-sha256)))
    (when (uiop:file-exists-p path)
      path)))

(defun get-build-path (build-id)
  "Get path to build directory for given ID.
Returns nil if not in store."
  (let ((path (build-path build-id)))
    (when (and (uiop:directory-exists-p path)
               (uiop:file-exists-p (merge-pathnames "manifest.sxp" path)))
      path)))

;;; Build ID computation

(defun compute-build-id (tree-sha256 compile-options &key (lisp-kind :sbcl) lisp-version)
  "Compute deterministic build ID for a source tree.
TREE-SHA256 is the source tree hash.
COMPILE-OPTIONS is a plist of compile settings.

Build ID = sha256(
  'clpm-build-v1\\0' +
  tree-sha256 + '\\0' +
  lisp-kind + '\\0' +
  lisp-version + '\\0' +
  platform-triple + '\\0' +
  asdf-version + '\\0' +
  normalized-compile-policy + '\\0' +
  features-hash)"
  (let* ((tree-sha256 (%sha256-hex-digest tree-sha256 "source tree identity"))
         (compile-policy (format nil "speed=~D;safety=~D;debug=~D"
                                 (or (getf compile-options :speed) 1)
                                 (or (getf compile-options :safety) 1)
                                 (or (getf compile-options :debug) 1)))
         (kind (clpm.lisp:parse-lisp-kind lisp-kind))
         (kind-str (string-downcase (symbol-name kind)))
         (version (or lisp-version
                      (case kind
                        (:sbcl (clpm.platform:sbcl-version))
                        (t (clpm.lisp:lisp-version kind)))))
         (input (format nil "clpm-build-v2~C~A~C~A~C~A~C~A~C~A~C~A~C~A"
                        #\Null
                        tree-sha256 #\Null
                        kind-str #\Null
                        version #\Null
                        (clpm.platform:platform-triple) #\Null
                        (clpm.platform:asdf-version) #\Null
                        compile-policy #\Null
                        (clpm.platform:features-hash))))
    (clpm.crypto.sha256:bytes-to-hex
     (clpm.crypto.sha256:sha256 input))))

;;; Store a build

(defun store-build (build-id source-dir fasl-dir manifest)
  "Store build outputs in the store.
BUILD-ID is the computed build ID.
SOURCE-DIR is the source tree path.
FASL-DIR is the directory containing compiled fasls.
MANIFEST is build metadata plist."
  (declare (ignore source-dir))
  (let* ((dest (build-path build-id))
         (manifest-path (merge-pathnames "manifest.sxp" dest)))
    ;; Treat the manifest as the completion marker so readers never pick up a
    ;; partially written build directory.
    (unless (uiop:file-exists-p manifest-path)
      (ensure-directories-exist dest)
      ;; Copy fasls
      (let ((fasl-dest (merge-pathnames "fasl/" dest)))
        (ensure-directories-exist fasl-dest)
        (when (uiop:directory-exists-p fasl-dir)
          (copy-directory-tree fasl-dir fasl-dest)))
      ;; Write manifest last.
      (with-open-file (s manifest-path
                         :direction :output
                         :if-exists :supersede)
        (clpm.io.sexp:write-canonical-sexp
         `(:build
           :build-id ,build-id
           :sbcl-version ,(clpm.platform:sbcl-version)
           :platform ,(clpm.platform:platform-triple)
           :asdf-version ,(clpm.platform:asdf-version)
           :built-at ,(get-universal-time)
           ,@manifest)
         s)))
    dest))

;;; Projects index (shared between activation and GC)

(defun projects-index-path ()
  "Return the absolute path to the global projects index."
  (merge-pathnames "projects.sxp" (clpm.platform:data-dir)))

(defun %projects-index-lock-path ()
  "Return the lock file path co-located with the projects index."
  (merge-pathnames "projects.sxp.lock" (clpm.platform:data-dir)))

(defun %read-project-index-roots-unlocked (path)
  "Read PATH as a projects index. Caller must hold the lock when consistency
across read-modify-write is required.

Returns (values roots found-p)."
  (if (not (uiop:file-exists-p path))
      (values nil nil)
      (handler-case
          (let* ((form (clpm.io.sexp:read-safe-sexp-from-file path))
                 (plist (cdr form)))
            (unless (and (consp form) (eq (car form) :projects)
                         (eql (getf plist :format) 1))
              (return-from %read-project-index-roots-unlocked
                (values nil nil)))
            (let ((roots (getf plist :roots)))
              (unless (listp roots)
                (return-from %read-project-index-roots-unlocked
                  (values nil nil)))
              (values (remove-duplicates
                       (remove-if-not #'stringp roots)
                       :test #'string=)
                      t)))
        (error ()
          (values nil nil)))))

(defun read-project-index-roots ()
  "Read the projects index under a shared lock.

Returns (values roots found-p)."
  (clpm.platform:with-file-lock ((%projects-index-lock-path) )
    (%read-project-index-roots-unlocked (projects-index-path))))

(defun %write-project-index-roots-unlocked (path roots)
  "Write ROOTS to PATH atomically (tmp + rename). Caller holds the lock."
  (ensure-directories-exist path)
  (let ((tmp-path (make-pathname :type "tmp" :defaults path)))
    (clpm.io.sexp:write-canonical-sexp-to-file
     `(:projects
       :format 1
       :roots ,(sort (copy-list roots) #'string<))
     tmp-path)
    (rename-file tmp-path path)))

(defun upsert-project-index-root (project-root)
  "Add PROJECT-ROOT to the projects index. Read-modify-write under exclusive lock.

Returns the updated list of roots."
  (let* ((project-root
           (uiop:ensure-directory-pathname
            (truename (uiop:ensure-directory-pathname project-root))))
         (root (namestring project-root))
         (index-path (projects-index-path)))
    (clpm.platform:with-file-lock ((%projects-index-lock-path) )
      (let* ((existing (%read-project-index-roots-unlocked index-path))
             (roots (remove-duplicates (cons root existing) :test #'string=)))
        (%write-project-index-roots-unlocked index-path roots)
        roots))))

(defun remove-project-index-root (project-root)
  "Remove PROJECT-ROOT from the projects index. Idempotent.

Returns the updated list of roots."
  (let* ((project-root
           (uiop:ensure-directory-pathname
            (handler-case
                (truename (uiop:ensure-directory-pathname project-root))
              (error ()
                (uiop:ensure-directory-pathname project-root)))))
         (root (namestring project-root))
         (index-path (projects-index-path)))
    (clpm.platform:with-file-lock ((%projects-index-lock-path) )
      (let* ((existing (%read-project-index-roots-unlocked index-path))
             (roots (remove root existing :test #'string=)))
        (%write-project-index-roots-unlocked index-path roots)
        roots))))

(defun %mark-strings (strings table)
  (when (listp strings)
    (dolist (s strings)
      (when (stringp s)
        (setf (gethash s table) t)))))

(defun %mark-from-lockfile (lock-path source-table artifact-table)
  (handler-case
      (let* ((form (clpm.io.sexp:read-lockfile lock-path))
             (resolved (getf (cdr form) :resolved)))
        (when (listp resolved)
          (dolist (sys resolved)
            (when (and (consp sys) (eq (car sys) :system))
              (let* ((release (getf (cdr sys) :release))
                     (tree-sha256 (when (listp release)
                                    (getf release :tree-sha256)))
                     (artifact-sha256 (when (listp release)
                                        (getf release :artifact-sha256))))
                (when (stringp tree-sha256)
                  (setf (gethash tree-sha256 source-table) t))
                (when (stringp artifact-sha256)
                  (setf (gethash artifact-sha256 artifact-table) t)))))))
    (error ()
      nil)))

(defun %mark-from-env (env-path source-table artifact-table build-table)
  (handler-case
      (let* ((form (clpm.io.sexp:read-safe-sexp-from-file env-path))
             (plist (cdr form)))
        (when (and (consp form) (eq (car form) :env) (listp plist))
          (%mark-strings (getf plist :source-tree-sha256s) source-table)
          (%mark-strings (getf plist :artifact-sha256s) artifact-table)
          (%mark-strings (getf plist :build-ids) build-table)))
    (error ()
      nil)))

(defun %dir-leaf-name (dir)
  (let* ((dir (uiop:ensure-directory-pathname dir))
         (parts (pathname-directory dir)))
    (car (last parts))))

(defun gc-store (&key dry-run)
  "Garbage collect unreferenced store entries.
If DRY-RUN is true, only report what would be deleted.
Returns list of paths that were (or would be) deleted."
  (let ((deleted '())
        (source-table (make-hash-table :test 'equal))
        (artifact-table (make-hash-table :test 'equal))
        (build-table (make-hash-table :test 'equal)))

    ;; Mark phase: scan real roots from the global projects index.
    (multiple-value-bind (roots found-p)
        (read-project-index-roots)
      (unless found-p
        ;; Without a roots index we can't safely determine reachability.
        (return-from gc-store nil))

      (dolist (root roots)
        (let* ((root-dir (uiop:ensure-directory-pathname root))
               (lock-path (merge-pathnames "clpm.lock" root-dir))
               (env-path (merge-pathnames ".clpm/env.sexp" root-dir)))
          (when (uiop:file-exists-p lock-path)
            (%mark-from-lockfile lock-path source-table artifact-table))
          (when (uiop:file-exists-p env-path)
            (%mark-from-env env-path source-table artifact-table build-table)))))

    ;; Sweep phase: delete unreferenced entries.
    (let ((store (clpm.platform:store-dir)))
      ;; Sources
      (let ((base (merge-pathnames "sources/sha256/" store)))
        (when (uiop:directory-exists-p base)
          (dolist (entry (clpm.io.fs:list-directory-entries base))
            (when (uiop:directory-pathname-p entry)
              (let ((tree-sha256 (%dir-leaf-name entry)))
                (when (and (stringp tree-sha256)
                           (not (gethash tree-sha256 source-table)))
                  (push entry deleted)
                  (unless dry-run
                    (uiop:delete-directory-tree entry :validate t))))))))
      ;; Artifacts
      (let ((base (merge-pathnames "artifacts/sha256/" store)))
        (when (uiop:directory-exists-p base)
          (dolist (entry (clpm.io.fs:list-directory-entries base))
            (unless (uiop:directory-pathname-p entry)
              (let ((artifact-sha256 (file-namestring entry)))
                (when (and (stringp artifact-sha256)
                           (not (gethash artifact-sha256 artifact-table)))
                  (push entry deleted)
                  (unless dry-run
                    (ignore-errors (delete-file entry)))))))))
      ;; Builds
      (let ((base (merge-pathnames "builds/" store)))
        (when (uiop:directory-exists-p base)
          (dolist (entry (clpm.io.fs:list-directory-entries base))
            (when (uiop:directory-pathname-p entry)
              (let ((build-id (%dir-leaf-name entry)))
                (when (and (stringp build-id)
                           (not (gethash build-id build-table)))
                  (push entry deleted)
                  (unless dry-run
                    (uiop:delete-directory-tree entry :validate t)))))))))

    deleted))

;;; Temporary directory management

(defun with-temp-directory (fn)
  "Call FN with a temporary directory path, cleaning up after."
  (let ((tmp (tmp-path)))
    (ensure-directories-exist tmp)
    (unwind-protect
         (funcall fn tmp)
      (ignore-errors
       (uiop:delete-directory-tree tmp :validate t)))))

(defmacro with-temp-dir ((var) &body body)
  "Execute BODY with VAR bound to a temporary directory path."
  `(with-temp-directory (lambda (,var) ,@body)))
