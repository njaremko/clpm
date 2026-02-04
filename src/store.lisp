;;;; store.lisp - Content-addressed storage

(in-package #:clpm.store)

;;; Store paths

(defun source-path (tree-sha256)
  "Return path for source tree with given hash."
  (merge-pathnames (format nil "sources/sha256/~A/" tree-sha256)
                   (clpm.platform:store-dir)))

(defun artifact-path (artifact-sha256)
  "Return path for artifact with given hash."
  (merge-pathnames (format nil "artifacts/sha256/~A" artifact-sha256)
                   (clpm.platform:store-dir)))

(defun build-path (build-id)
  "Return path for build with given ID."
  (merge-pathnames (format nil "builds/~A/" build-id)
                   (clpm.platform:store-dir)))

(defun tmp-path ()
  "Return temp directory path."
  (merge-pathnames (format nil "tmp/~A/" (random (expt 2 32)))
                   (clpm.platform:store-dir)))

;;; Existence checks

(defun source-exists-p (tree-sha256)
  "Check if source tree exists in store."
  (uiop:directory-exists-p (source-path tree-sha256)))

(defun artifact-exists-p (artifact-sha256)
  "Check if artifact exists in store."
  (uiop:file-exists-p (artifact-path artifact-sha256)))

(defun build-exists-p (build-id)
  "Check if build exists in store."
  (let ((path (build-path build-id)))
    (and (uiop:directory-exists-p path)
         (uiop:file-exists-p (merge-pathnames "manifest.sxp" path)))))

;;; Store artifacts

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
       ;; Store if not already present
       (unless (uiop:file-exists-p dest-path)
         (ensure-directories-exist dest-path)
         (with-open-file (s dest-path :direction :output
                                      :element-type '(unsigned-byte 8)
                                      :if-exists :supersede)
           (write-sequence source-bytes s)))
       dest-path))
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
       ;; Store if not already present (streaming copy).
       (unless (uiop:file-exists-p dest-path)
         (ensure-directories-exist dest-path)
         (uiop:copy-file data dest-path))
       dest-path))))

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
    ;; Store if not already present
    (unless (uiop:directory-exists-p dest-path)
      (ensure-directories-exist dest-path)
      (let ((src-dir (merge-pathnames "src/" dest-path)))
        (ensure-directories-exist src-dir)
        (copy-directory-tree source-dir src-dir)
        ;; Write metadata
        (with-open-file (s (merge-pathnames "meta.sxp" dest-path)
                           :direction :output
                           :if-exists :supersede)
          (format s "(source~%  :tree-sha256 ~S~%  :stored-at ~S)~%"
                  actual-hash
                  (get-universal-time)))))
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
  (let ((path (merge-pathnames "src/" (source-path tree-sha256))))
    (when (uiop:directory-exists-p path)
      path)))

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
  (let* ((compile-policy (format nil "speed=~D;safety=~D;debug=~D"
                                 (or (getf compile-options :speed) 1)
                                 (or (getf compile-options :safety) 1)
                                 (or (getf compile-options :debug) 1)))
         (kind (clpm.lisp:parse-lisp-kind lisp-kind))
         (kind-str (string-downcase (symbol-name kind)))
         (version (or lisp-version
                      (case kind
                        (:sbcl (clpm.platform:sbcl-version))
                        (t (clpm.lisp:lisp-version kind)))))
         (input (format nil "clpm-build-v1~C~A~C~A~C~A~C~A~C~A~C~A~C~A"
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

;;; Garbage collection

(defun %projects-index-path ()
  (merge-pathnames "projects.sxp" (clpm.platform:data-dir)))

(defun %read-project-index-roots (path)
  "Read PATH as a projects index and return (values roots found-p)."
  (if (not (uiop:file-exists-p path))
      (values nil nil)
      (handler-case
          (let* ((form (clpm.io.sexp:read-safe-sexp-from-file path))
                 (plist (cdr form)))
            (unless (and (consp form) (eq (car form) :projects)
                         (eql (getf plist :format) 1))
              (return-from %read-project-index-roots (values nil nil)))
            (let ((roots (getf plist :roots)))
              (unless (listp roots)
                (return-from %read-project-index-roots (values nil nil)))
              (values (remove-duplicates
                       (remove-if-not #'stringp roots)
                       :test #'string=)
                      t)))
        (error ()
          (values nil nil)))))

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
        (%read-project-index-roots (%projects-index-path))
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
