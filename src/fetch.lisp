;;;; fetch.lisp - Artifact fetching and extraction

(in-package #:clpm.fetch)

;;; Downloader interface

(defun fetch-url (url dest-path &key (progress t))
  "Fetch URL to DEST-PATH using available downloader."
  (let ((downloader (clpm.platform:find-downloader)))
    (unless downloader
      (error 'clpm.errors:clpm-fetch-error
             :message "No downloader available (need curl, wget, or PowerShell)"
             :url url))
    (ecase downloader
      (:curl (fetch-with-curl url dest-path :progress progress))
      (:wget (fetch-with-wget url dest-path :progress progress))
      (:powershell (fetch-with-powershell url dest-path)))))

(defun fetch-with-curl (url dest-path &key progress)
  "Fetch URL using curl."
  (let ((args (list "curl" "-fsSL")))
    (when progress
      (push "-#" args))
    (push "-o" args)
    (push (namestring dest-path) args)
    (push url args)
    (setf args (nreverse args))
    (multiple-value-bind (output error-output exit-code)
        (clpm.platform:run-program args)
      (declare (ignore output))
      (unless (zerop exit-code)
        (error 'clpm.errors:clpm-fetch-error
               :message (or error-output "curl failed")
               :url url
               :status exit-code)))))

(defun fetch-with-wget (url dest-path &key progress)
  "Fetch URL using wget."
  (let ((args (list "wget")))
    (unless progress
      (push "-q" args))
    (push "-O" args)
    (push (namestring dest-path) args)
    (push url args)
    (setf args (nreverse args))
    (multiple-value-bind (output error-output exit-code)
        (clpm.platform:run-program args)
      (declare (ignore output))
      (unless (zerop exit-code)
        (error 'clpm.errors:clpm-fetch-error
               :message (or error-output "wget failed")
               :url url
               :status exit-code)))))

(defun fetch-with-powershell (url dest-path)
  "Fetch URL using PowerShell (Windows)."
  (let ((cmd (format nil "Invoke-WebRequest -Uri '~A' -OutFile '~A'"
                     url (namestring dest-path))))
    (multiple-value-bind (output error-output exit-code)
        (clpm.platform:run-program
         (list "powershell.exe" "-Command" cmd))
      (declare (ignore output))
      (unless (zerop exit-code)
        (error 'clpm.errors:clpm-fetch-error
               :message (or error-output "PowerShell download failed")
               :url url
               :status exit-code)))))

;;; Git fetch

(defun fetch-git (url dest-path &key commit)
  "Clone git repository from URL to DEST-PATH, optionally checking out COMMIT."
  (let ((git (clpm.platform:find-git)))
    (unless git
      (error 'clpm.errors:clpm-fetch-error
             :message "git not found in PATH"
             :url url))
    ;; Clone
    (multiple-value-bind (output error-output exit-code)
        (clpm.platform:run-program
         (list git "clone" url (namestring dest-path)))
      (declare (ignore output))
      (unless (zerop exit-code)
        (error 'clpm.errors:clpm-fetch-error
               :message (format nil "git clone failed: ~A" error-output)
               :url url
               :status exit-code)))
    ;; Checkout specific commit if requested
    (when commit
      (multiple-value-bind (output error-output exit-code)
          (clpm.platform:run-program
           (list git "checkout" commit)
           :directory dest-path)
        (declare (ignore output))
        (unless (zerop exit-code)
          (error 'clpm.errors:clpm-fetch-error
                 :message (format nil "git checkout failed: ~A" error-output)
                 :url url
                 :status exit-code))))))

;;; Archive extraction

(defun extract-archive (archive-path dest-dir)
  "Extract archive at ARCHIVE-PATH to DEST-DIR.
Supports .tar.gz, .tgz, .tar, .zip"
  (let ((name (file-namestring archive-path)))
    (ensure-directories-exist dest-dir)
    (cond
      ((or (ends-with-p name ".tar.gz")
           (ends-with-p name ".tgz"))
       (extract-tar-gz archive-path dest-dir))
      ((ends-with-p name ".tar")
       (extract-tar archive-path dest-dir))
      ((ends-with-p name ".zip")
       (extract-zip archive-path dest-dir))
      (t
       (error 'clpm.errors:clpm-fetch-error
              :message (format nil "Unsupported archive format: ~A" name))))))

(defun ends-with-p (string suffix)
  "Check if STRING ends with SUFFIX."
  (let ((slen (length string))
        (suflen (length suffix)))
    (and (>= slen suflen)
         (string= string suffix :start1 (- slen suflen)))))

(defun extract-tar-gz (archive-path dest-dir)
  "Extract .tar.gz archive."
  (let ((tar (clpm.platform:find-tar)))
    (unless tar
      (error 'clpm.errors:clpm-fetch-error
             :message "tar not found in PATH"))
    (multiple-value-bind (output error-output exit-code)
        (clpm.platform:run-program
         (list tar "-xzf" (namestring archive-path)
               "-C" (namestring dest-dir)))
      (declare (ignore output))
      (unless (zerop exit-code)
        (error 'clpm.errors:clpm-fetch-error
               :message (format nil "tar extraction failed: ~A" error-output)
               :status exit-code)))))

(defun extract-tar (archive-path dest-dir)
  "Extract .tar archive."
  (let ((tar (clpm.platform:find-tar)))
    (unless tar
      (error 'clpm.errors:clpm-fetch-error
             :message "tar not found in PATH"))
    (multiple-value-bind (output error-output exit-code)
        (clpm.platform:run-program
         (list tar "-xf" (namestring archive-path)
               "-C" (namestring dest-dir)))
      (declare (ignore output))
      (unless (zerop exit-code)
        (error 'clpm.errors:clpm-fetch-error
               :message (format nil "tar extraction failed: ~A" error-output)
               :status exit-code)))))

(defun extract-zip (archive-path dest-dir)
  "Extract .zip archive."
  (let ((unzip (clpm.platform:which "unzip")))
    (if unzip
        (multiple-value-bind (output error-output exit-code)
            (clpm.platform:run-program
             (list unzip "-q" (namestring archive-path)
                   "-d" (namestring dest-dir)))
          (declare (ignore output))
          (unless (zerop exit-code)
            (error 'clpm.errors:clpm-fetch-error
                   :message (format nil "unzip failed: ~A" error-output)
                   :status exit-code)))
        ;; Try PowerShell on Windows
        #+windows
        (let ((cmd (format nil "Expand-Archive -Path '~A' -DestinationPath '~A'"
                           (namestring archive-path)
                           (namestring dest-dir))))
          (multiple-value-bind (output error-output exit-code)
              (clpm.platform:run-program
               (list "powershell.exe" "-Command" cmd))
            (declare (ignore output))
            (unless (zerop exit-code)
              (error 'clpm.errors:clpm-fetch-error
                     :message (format nil "PowerShell unzip failed: ~A" error-output)
                     :status exit-code))))
        #-windows
        (error 'clpm.errors:clpm-fetch-error
               :message "unzip not found in PATH"))))

;;; High-level fetch interface

(defun fetch-artifact (source expected-sha256 &key (verify t))
  "Fetch artifact according to SOURCE specification.
SOURCE is a plist like (:tarball :url ... :sha256 ...) or (:git :url ... :commit ...)
Returns (values source-path tree-sha256).
If VERIFY is true, verifies hash matches EXPECTED-SHA256."
  (let ((kind (car source)))
    (clpm.store:with-temp-dir (tmp)
      (ecase kind
        (:tarball
         (let* ((url (getf (cdr source) :url))
                (artifact-file (merge-pathnames "artifact.tar.gz" tmp)))
           ;; Download
           (fetch-url url artifact-file)
           ;; Verify hash
           (when verify
             (let ((actual-hash (clpm.crypto.sha256:bytes-to-hex
                                 (clpm.crypto.sha256:sha256-file artifact-file))))
               (unless (string-equal actual-hash expected-sha256)
                 (error 'clpm.errors:clpm-hash-mismatch-error
                        :expected expected-sha256
                        :actual actual-hash
                        :artifact url))))
           ;; Store artifact
           (clpm.store:store-artifact artifact-file expected-sha256)
           ;; Extract and store source
           (let ((extract-dir (merge-pathnames "extract/" tmp)))
             (ensure-directories-exist extract-dir)
             (extract-archive artifact-file extract-dir)
             ;; Find extracted directory (often has one top-level dir)
             (let ((contents (directory (merge-pathnames "*" extract-dir))))
               (if (and (= (length contents) 1)
                        (uiop:directory-pathname-p (first contents)))
                   (clpm.store:store-source (first contents) nil)
                   (clpm.store:store-source extract-dir nil))))))
        (:git
         (let* ((url (getf (cdr source) :url))
                (commit (getf (cdr source) :commit))
                (clone-dir (merge-pathnames "repo/" tmp)))
           (fetch-git url clone-dir :commit commit)
           ;; Store source
           (clpm.store:store-source clone-dir nil)))
        (:path
         ;; Local path - just verify and return
         (let ((path (second source)))
           (unless (uiop:directory-exists-p path)
             (error 'clpm.errors:clpm-fetch-error
                    :message (format nil "Path does not exist: ~A" path)))
	           (let ((actual-hash (clpm.crypto.sha256:bytes-to-hex
	                               (clpm.crypto.sha256:sha256-tree path))))
	             (values path actual-hash))))))))

(defun verify-and-store (artifact-path expected-sha256)
  "Verify artifact hash and store in content-addressed store.
Returns path in store."
  (let ((actual-hash (clpm.crypto.sha256:bytes-to-hex
                      (clpm.crypto.sha256:sha256-file artifact-path))))
    (unless (string-equal actual-hash expected-sha256)
      (error 'clpm.errors:clpm-hash-mismatch-error
             :expected expected-sha256
             :actual actual-hash
             :artifact (namestring artifact-path)))
    (clpm.store:store-artifact artifact-path expected-sha256)))

;;; Fetch all dependencies from lockfile

(defun fetch-lockfile-deps (lockfile &key lockfile-path (parallel nil))
  "Fetch all dependencies specified in LOCKFILE.
If PARALLEL, fetch multiple in parallel (not yet implemented).
Returns list of (system-id . source-path) pairs."
  (declare (ignore parallel))
  (let ((results '()))
    (dolist (locked (clpm.project:lockfile-resolved lockfile))
      (let* ((system-id (clpm.project:locked-system-id locked))
             (release (clpm.project:locked-system-release locked))
             (source (clpm.project:locked-release-source release))
             (artifact-sha256 (clpm.project:locked-release-artifact-sha256 release))
             (tree-sha256 (clpm.project:locked-release-tree-sha256 release)))
        ;; Check if already in store
        (let ((existing (when tree-sha256
                          (clpm.store:get-source-path tree-sha256))))
          (if existing
              (push (cons system-id existing) results)
              ;; Need to fetch
              (let ((source-spec (locked-source-to-spec source)))
                (multiple-value-bind (path fetched-tree-sha256)
                    (fetch-artifact source-spec artifact-sha256)
                  (when (and fetched-tree-sha256 (null tree-sha256))
                    (setf (clpm.project:locked-release-tree-sha256 release)
                          fetched-tree-sha256))
                  (push (cons system-id path) results)))))))
    ;; If we backfilled tree hashes, persist them to disk.
    (when lockfile-path
      (let* ((lockfile-path (uiop:ensure-pathname lockfile-path
                                                  :want-file t
                                                  :want-existing nil))
             (tmp-path (merge-pathnames "clpm.lock.tmp"
                                        (uiop:pathname-directory-pathname lockfile-path))))
        (clpm.project:write-lock-file lockfile tmp-path)
        (rename-file tmp-path lockfile-path)))
    (nreverse results)))

(defun locked-source-to-spec (locked-source)
  "Convert a locked-source struct to a source specification plist."
  (let ((kind (clpm.project:locked-source-kind locked-source)))
    (ecase kind
      (:tarball
       (list :tarball
             :url (clpm.project:locked-source-url locked-source)
             :sha256 (clpm.project:locked-source-sha256 locked-source)))
      (:git
       (list :git
             :url (clpm.project:locked-source-url locked-source)
             :commit (clpm.project:locked-source-commit locked-source)))
      (:path
       (list :path (clpm.project:locked-source-path locked-source))))))
