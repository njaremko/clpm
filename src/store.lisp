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
  (uiop:directory-exists-p (build-path build-id)))

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
    (when (uiop:directory-exists-p path)
      path)))

;;; Build ID computation

(defun compute-build-id (tree-sha256 compile-options)
  "Compute deterministic build ID for a source tree.
TREE-SHA256 is the source tree hash.
COMPILE-OPTIONS is a plist of compile settings.

Build ID = sha256(
  'clpm-build-v1\\0' +
  tree-sha256 + '\\0' +
  sbcl-version + '\\0' +
  platform-triple + '\\0' +
  asdf-version + '\\0' +
  normalized-compile-policy + '\\0' +
  features-hash)"
  (let* ((compile-policy (format nil "speed=~D;safety=~D;debug=~D"
                                 (or (getf compile-options :speed) 1)
                                 (or (getf compile-options :safety) 1)
                                 (or (getf compile-options :debug) 1)))
         (input (format nil "clpm-build-v1~C~A~C~A~C~A~C~A~C~A~C~A"
                        #\Null
                        tree-sha256 #\Null
                        (clpm.platform:sbcl-version) #\Null
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
  (let ((dest (build-path build-id)))
    (unless (uiop:directory-exists-p dest)
      (ensure-directories-exist dest)
      ;; Copy fasls
      (let ((fasl-dest (merge-pathnames "fasl/" dest)))
        (ensure-directories-exist fasl-dest)
        (when (uiop:directory-exists-p fasl-dir)
          (copy-directory-tree fasl-dir fasl-dest)))
      ;; Write manifest
      (with-open-file (s (merge-pathnames "manifest.sxp" dest)
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

(defun gc-store (&key dry-run)
  "Garbage collect unreferenced store entries.
If DRY-RUN is true, only report what would be deleted.
Returns list of paths that were (or would be) deleted."
  (let ((deleted '())
        (referenced (make-hash-table :test 'equal)))
    ;; Mark phase: collect all referenced hashes from lockfiles
    ;; TODO: scan all known lockfiles
    ;; For now, this is a placeholder

    ;; Sweep phase: delete unreferenced entries
    (dolist (subdir '("sources/sha256/" "artifacts/sha256/" "builds/"))
      (let ((base (merge-pathnames subdir (clpm.platform:store-dir))))
        (when (uiop:directory-exists-p base)
          (dolist (entry (directory (merge-pathnames "*" base)))
            (let ((hash (file-namestring (uiop:pathname-directory-pathname entry))))
              (unless (gethash hash referenced)
                (push entry deleted)
                (unless dry-run
                  (uiop:delete-directory-tree entry :validate t))))))))

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
