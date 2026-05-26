;;;; fetch.lisp - Artifact fetching and extraction

(in-package #:clpm.fetch)

;;; Retry / timeout configuration
;;;
;;; A `fetch-url` call wraps the underlying downloader in a retry loop with
;;; quadratic backoff. The schedule for attempt N (1-indexed) waits
;;; `*fetch-backoff-base* * (N-1)^2` seconds before the next attempt. With the
;;; default base of 1s and 3 attempts: pauses of 0s, 1s, 4s -- the third
;;; attempt happens 5s after the first.
;;;
;;; All tunables fall through to env vars so users can override without code
;;; changes; CLI flags then bind these specials globally for the run.

(defvar *fetch-retries* nil
  "Number of fetch attempts before giving up. NIL means read CLPM_FETCH_RETRIES
or fall back to 3.")

(defvar *fetch-timeout* nil
  "Per-request timeout in seconds. NIL means read CLPM_FETCH_TIMEOUT or fall
back to 60.")

(defvar *fetch-backoff-base* 1
  "Base seconds for quadratic backoff (0 disables sleeping; tests set this).")

(defvar *fetch-sleep-fn* nil
  "If non-NIL, a function (lambda (seconds)) called instead of sleep. Lets
tests assert on the backoff schedule without actually sleeping.")

(defvar *test-fetcher* nil
  "If non-NIL, a function (lambda (url dest-path)) that replaces the real
downloader. Tests inject this to simulate failures deterministically.")

(defun %positive-int-from-env (name)
  (let ((raw (uiop:getenv name)))
    (when (and raw (plusp (length raw)))
      (let ((n (ignore-errors (parse-integer raw :junk-allowed nil))))
        (and (integerp n) (plusp n) n)))))

(defun %effective-retries ()
  (or *fetch-retries*
      (%positive-int-from-env "CLPM_FETCH_RETRIES")
      3))

(defun %effective-timeout ()
  (or *fetch-timeout*
      (%positive-int-from-env "CLPM_FETCH_TIMEOUT")
      60))

(defun %fetch-sleep (seconds)
  (when (plusp seconds)
    (if *fetch-sleep-fn*
        (funcall *fetch-sleep-fn* seconds)
        (sleep seconds))))

;;; Downloader interface

(defun %fetch-once (url dest-path &key progress timeout)
  "Perform a single download attempt; signal `clpm-fetch-error' on failure."
  (when *test-fetcher*
    (funcall *test-fetcher* url dest-path)
    (return-from %fetch-once))
  (let ((downloader (clpm.platform:find-downloader)))
    (unless downloader
      (error 'clpm.errors:clpm-fetch-error
             :message "No downloader available (need curl, wget, or PowerShell)"
             :url url))
    (ecase downloader
      (:curl (fetch-with-curl url dest-path :progress progress :timeout timeout))
      (:wget (fetch-with-wget url dest-path :progress progress :timeout timeout))
      (:powershell (fetch-with-powershell url dest-path :timeout timeout)))))

(defun call-with-retry (action policy &key (error-type 'error) (sleep-fn #'sleep))
  "Call ACTION (a zero-argument function) repeatedly under POLICY.
POLICY is a function: (lambda (attempt err) -> (values retry-p delay-seconds)).
ERROR-TYPE is the type of error that triggers a retry; other conditions
propagate immediately."
  (labels ((try (attempt)
             (handler-case
                 (funcall action)
               (condition (err)
                 (if (typep err error-type)
                     (multiple-value-bind (retry-p delay)
                         (funcall policy attempt err)
                       (if retry-p
                           (progn
                             (funcall sleep-fn delay)
                             (try (1+ attempt)))
                           (error err)))
                     (error err))))))
    (try 1)))

(defun quadratic-backoff-policy (max-attempts backoff-base)
  "Create a policy that retries up to MAX-ATTEMPTS with quadratic backoff."
  (lambda (attempt err)
    (declare (ignore err))
    (if (< attempt max-attempts)
        (values t (* backoff-base attempt attempt))
        (values nil 0))))

(defun fetch-url (url dest-path &key (progress t))
  "Fetch URL to DEST-PATH using available downloader.

Retries transient failures up to `*fetch-retries*' times with quadratic
backoff (`*fetch-backoff-base*' * (N-1)^2 seconds between attempts). Each
attempt enforces a per-request timeout of `*fetch-timeout*' seconds."
  (let* ((retries (%effective-retries))
         (timeout (%effective-timeout))
         (policy (quadratic-backoff-policy retries *fetch-backoff-base*))
         (sleep-fn (lambda (seconds)
                     (when (plusp seconds)
                       (if *fetch-sleep-fn*
                           (funcall *fetch-sleep-fn* seconds)
                           (sleep seconds))))))
    (call-with-retry
     (lambda ()
       (%fetch-once url dest-path :progress progress :timeout timeout))
     policy
     :error-type 'clpm.errors:clpm-fetch-error
     :sleep-fn sleep-fn)))


(defun fetch-with-curl (url dest-path &key progress timeout)
  "Fetch URL using curl."
  (let ((args (append (list "curl" "-fsSL")
                      (when (and timeout (plusp timeout))
                        (list "--max-time" (princ-to-string timeout)))
                      (when progress (list "-#"))
                      (list "-o" (namestring dest-path) url))))
    (multiple-value-bind (output error-output exit-code)
        (clpm.platform:run-program args)
      (declare (ignore output))
      (unless (zerop exit-code)
        (error 'clpm.errors:clpm-fetch-error
               :message (or error-output "curl failed")
               :url url
               :status exit-code)))))

(defun fetch-with-wget (url dest-path &key progress timeout)
  "Fetch URL using wget."
  (let ((args (append (list "wget")
                      (when (and timeout (plusp timeout))
                        (list (format nil "--timeout=~A" timeout)
                              "--tries=1"))
                      (unless progress (list "-q"))
                      (list "-O" (namestring dest-path) url))))
    (multiple-value-bind (output error-output exit-code)
        (clpm.platform:run-program args)
      (declare (ignore output))
      (unless (zerop exit-code)
        (error 'clpm.errors:clpm-fetch-error
               :message (or error-output "wget failed")
               :url url
               :status exit-code)))))

(defun fetch-with-powershell (url dest-path &key timeout)
  "Fetch URL using PowerShell (Windows)."
  (let* ((timeout-clause
           (if (and timeout (plusp timeout))
               (format nil " -TimeoutSec ~A" timeout)
               ""))
         (cmd (format nil "Invoke-WebRequest -Uri '~A' -OutFile '~A'~A"
                      url (namestring dest-path) timeout-clause)))
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

(defun %hex40-p (s)
  "Return true when S is a 40-hex commit string."
  (and (stringp s)
       (= (length s) 40)
       (every (lambda (c) (find c "0123456789abcdef" :test #'char-equal)) s)))

(defun resolve-git-ref (url ref)
  "Resolve REF in URL to a full 40-hex commit SHA."
  ;; Fast path: already a commit.
  (when (%hex40-p ref)
    (return-from resolve-git-ref ref))

  (let ((git (clpm.platform:find-git)))
    (unless git
      (error 'clpm.errors:clpm-fetch-error
             :message "git not found in PATH"
             :url url))

    (labels ((ls-remote (&optional (use-refs t))
               (let ((args (if use-refs
                               (list git "ls-remote" "--refs")
                               (list git "ls-remote"))))
                 (multiple-value-bind (output error-output exit-code)
                     (clpm.platform:run-program
                      (append args (list url ref (format nil "~A^{}" ref)))
                      :error-output :string)
                   (unless (zerop exit-code)
                     (error 'clpm.errors:clpm-fetch-error
                            :message (or error-output "git ls-remote failed")
                            :url url
                            :status exit-code))
                   (let* ((lines (uiop:split-string output :separator '(#\Newline #\Return)))
                          (lines (remove-if (lambda (l) (zerop (length l))) lines)))
                     (remove nil
                             (mapcar (lambda (line)
                                       (let ((tab (position #\Tab line)))
                                         (when tab
                                           (cons (subseq line 0 tab)
                                                 (subseq line (1+ tab))))))
                                     lines)))))))
      ;; Plan algorithm: ls-remote --refs URL REF REF^{}.
      (let* ((pairs (ls-remote t))
             ;; Compatibility: `--refs` doesn't include HEAD; allow HEAD for local testing.
             (pairs (if (and (null pairs) (string= ref "HEAD"))
                        (ls-remote nil)
                        pairs)))
        (when (null pairs)
          (error 'clpm.errors:clpm-fetch-error
                 :message "git ref not found"
                 :url url))
        (let* ((tag-suffix (format nil "refs/tags/~A^{}" ref))
               (tag (find-if (lambda (p) (and (cdr p)
                                              (ends-with-p (cdr p) tag-suffix)))
                             pairs))
               (sha (string-trim '(#\Space #\Newline #\Return)
                                 (car (or tag (first pairs))))))
          sha)))))

;;; Archive extraction

(defun windows-drive-prefix-p (path)
  "Return true if PATH starts with a Windows drive prefix like C:."
  (and (>= (length path) 2)
       (char= (char path 1) #\:)
       (let ((c (char path 0)))
         (or (and (char>= c #\A) (char<= c #\Z))
             (and (char>= c #\a) (char<= c #\z))))))

(defun unsafe-archive-entry-p (entry)
  "Return true if ENTRY is unsafe to extract."
  (let ((entry (string-trim '(#\Space #\Tab #\Newline #\Return) entry)))
    (cond
      ((zerop (length entry)) nil)
      ;; Absolute paths.
      ((char= (char entry 0) #\/) t)
      ((char= (char entry 0) #\\) t)
      ;; Windows drive prefixes.
      ((windows-drive-prefix-p entry) t)
      ;; Path traversal.
      ((find ".." (uiop:split-string entry :separator '(#\/ #\\))
             :test #'string=)
       t)
      (t nil))))

(defun validate-archive-entries (archive-path entries)
  "Validate archive ENTRIES are safe to extract.
Signals CLPM-FETCH-ERROR if any entry is unsafe."
  (dolist (entry entries)
    (when (unsafe-archive-entry-p entry)
      (error 'clpm.errors:clpm-fetch-error
             :message (format nil "Unsafe archive entry: ~A" entry)
             :url (namestring archive-path)))))

(defun %tar-list-flag (compression)
  "Return the tar listing flag for COMPRESSION (:none :gzip :xz :bz2)."
  (ecase compression
    (:none "-tf")
    (:gzip "-tzf")
    (:xz   "-tJf")
    (:bz2  "-tjf")))

(defun %tar-extract-flag (compression)
  "Return the tar extract flag for COMPRESSION."
  (ecase compression
    (:none "-xf")
    (:gzip "-xzf")
    (:xz   "-xJf")
    (:bz2  "-xjf")))

(defun %compression-helper-tool (compression)
  "Return the helper tool name a user must have installed for COMPRESSION."
  (case compression
    (:xz  "xz")
    (:bz2 "bzip2")
    (t    nil)))

(defun %ensure-compression-tool (compression archive-path)
  "Verify that the helper tool for COMPRESSION is available; signal a
clpm-missing-tool-error with install hints otherwise."
  (let ((helper (%compression-helper-tool compression)))
    (when (and helper (not (clpm.platform:which helper)))
      (error 'clpm.errors:clpm-missing-tool-error
             :tool helper
             :install-hints (clpm.platform:tool-install-hints helper)
             :message (format nil "~A required to extract ~A"
                              helper (namestring archive-path))))))

(defun validate-tar-contents (archive-path &key (compression :none))
  "Validate tar contents (with optional COMPRESSION) before extracting."
  (let ((tar (clpm.platform:find-tar)))
    (unless tar
      (error 'clpm.errors:clpm-missing-tool-error
             :tool "tar"
             :install-hints (clpm.platform:tool-install-hints "tar")))
    (%ensure-compression-tool compression archive-path)
    (let ((args (list tar (%tar-list-flag compression) (namestring archive-path))))
      (multiple-value-bind (lines error-output exit-code)
          (clpm.platform:run-program args :output :lines :error-output :string)
        (declare (ignore error-output))
        (unless (zerop exit-code)
          (error 'clpm.errors:clpm-fetch-error
                 :message "tar listing failed"
                 :url (namestring archive-path)
                 :status exit-code))
        (validate-archive-entries archive-path lines)))))

(defun validate-zip-contents (archive-path)
  "Validate zip contents before extracting."
  (let ((unzip (clpm.platform:which "unzip")))
    (unless unzip
      (error 'clpm.errors:clpm-fetch-error
             :message "unzip not found in PATH"))
    (multiple-value-bind (lines error-output exit-code)
        (clpm.platform:run-program
         (list unzip "-Z1" (namestring archive-path))
         :output :lines
         :error-output :string)
      (declare (ignore error-output))
      (unless (zerop exit-code)
        (error 'clpm.errors:clpm-fetch-error
               :message "unzip listing failed"
               :url (namestring archive-path)
               :status exit-code))
      (validate-archive-entries archive-path lines))))

(defun extract-archive (archive-path dest-dir)
  "Extract archive at ARCHIVE-PATH to DEST-DIR.
Supports .tar.gz/.tgz, .tar.xz/.txz, .tar.bz2/.tbz/.tbz2, .tar, .zip."
  (let ((name (file-namestring archive-path)))
    (ensure-directories-exist dest-dir)
    (cond
      ((or (ends-with-p name ".tar.gz")
           (ends-with-p name ".tgz"))
       (extract-tar-compressed archive-path dest-dir :gzip))
      ((or (ends-with-p name ".tar.xz")
           (ends-with-p name ".txz"))
       (extract-tar-compressed archive-path dest-dir :xz))
      ((or (ends-with-p name ".tar.bz2")
           (ends-with-p name ".tbz2")
           (ends-with-p name ".tbz"))
       (extract-tar-compressed archive-path dest-dir :bz2))
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

(defun extract-tar-compressed (archive-path dest-dir compression)
  "Extract a compressed tar archive. COMPRESSION is :gzip, :xz, or :bz2."
  (let ((tar (clpm.platform:find-tar)))
    (unless tar
      (error 'clpm.errors:clpm-missing-tool-error
             :tool "tar"
             :install-hints (clpm.platform:tool-install-hints "tar")))
    (validate-tar-contents archive-path :compression compression)
    (multiple-value-bind (output error-output exit-code)
        (clpm.platform:run-program
         (list tar (%tar-extract-flag compression)
               (namestring archive-path)
               "-C" (namestring dest-dir))
         :error-output :string)
      (declare (ignore output))
      (unless (zerop exit-code)
        (error 'clpm.errors:clpm-fetch-error
               :message (format nil "tar extraction failed: ~A" error-output)
               :status exit-code)))))

(defun extract-tar (archive-path dest-dir)
  "Extract .tar archive."
  (let ((tar (clpm.platform:find-tar)))
    (unless tar
      (error 'clpm.errors:clpm-missing-tool-error
             :tool "tar"
             :install-hints (clpm.platform:tool-install-hints "tar")))
    (validate-tar-contents archive-path :compression :none)
    (multiple-value-bind (output error-output exit-code)
        (clpm.platform:run-program
         (list tar "-xf" (namestring archive-path)
               "-C" (namestring dest-dir))
         :error-output :string)
      (declare (ignore output))
      (unless (zerop exit-code)
        (error 'clpm.errors:clpm-fetch-error
               :message (format nil "tar extraction failed: ~A" error-output)
               :status exit-code)))))

(defun extract-zip (archive-path dest-dir)
  "Extract .zip archive."
  (let ((unzip (clpm.platform:which "unzip")))
    (if unzip
        (progn
          (validate-zip-contents archive-path)
          (multiple-value-bind (output error-output exit-code)
              (clpm.platform:run-program
               (list unzip "-q" (namestring archive-path)
                     "-d" (namestring dest-dir)))
            (declare (ignore output))
            (unless (zerop exit-code)
              (error 'clpm.errors:clpm-fetch-error
                     :message (format nil "unzip failed: ~A" error-output)
                     :status exit-code))))
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

(defun %content-sha1-hex (root)
  "Return a Quicklisp-style content SHA-1 hex digest for ROOT.

Quicklisp's releases.txt provides a content hash, not an archive hash: it is
the SHA-1 of the concatenated contents of all files in the release, processed
in sorted filename order."
  (let* ((files (clpm.io.fs:walk-files root))
         (paths (mapcar #'cdr files)))
    (clpm.crypto.sha256:bytes-to-hex
     (clpm.crypto.sha1:sha1-files paths))))

(defun fetch-artifact (source expected-sha256 &key (verify t))
  "Fetch artifact according to SOURCE specification.
SOURCE is a plist like (:tarball :url ... :sha256 ... :sha1 ...) or (:git :url ... :commit ...)
Returns (values source-path tree-sha256 artifact-sha256).
If VERIFY is true, verifies the tarball against an expected hash when available."
  (let ((kind (car source)))
    (clpm.store:with-temp-dir (tmp)
      (ecase kind
        (:tarball
	         (let* ((url (getf (cdr source) :url))
	                (expected-sha256 (or expected-sha256 (getf (cdr source) :sha256)))
	                (expected-sha1 (getf (cdr source) :sha1))
	                (artifact-file (merge-pathnames "artifact.tar.gz" tmp)))
	           ;; Download
	           (fetch-url url artifact-file)
	           ;; Verify hashes (if available).
	           ;;
	           ;; - If we have an archive SHA-256, verify it before extraction.
	           ;; - If we only have a SHA-1 (Quicklisp content hash), verify it after extraction.
	           (when verify
	             (unless (or (stringp expected-sha256) (stringp expected-sha1))
	               (error 'clpm.errors:clpm-fetch-error
	                      :message "Missing expected hash for tarball source"
	                      :url url))
	             (when (stringp expected-sha256)
	               (let ((actual-sha256 (clpm.crypto.sha256:bytes-to-hex
	                                     (clpm.crypto.sha256:sha256-file artifact-file))))
	                 (unless (string-equal actual-sha256 expected-sha256)
	                   (error 'clpm.errors:clpm-hash-mismatch-error
	                          :expected expected-sha256
	                          :actual actual-sha256
	                          :artifact url)))))
	           ;; Compute artifact SHA-256 for storage and lockfile backfills.
	           (let ((artifact-sha256 (clpm.crypto.sha256:bytes-to-hex
	                                   (clpm.crypto.sha256:sha256-file artifact-file))))
	             ;; Store artifact (store uses SHA-256 addressing).
	             (clpm.store:store-artifact artifact-file artifact-sha256)
             ;; Extract and store source
             (let ((extract-dir (merge-pathnames "extract/" tmp)))
               (ensure-directories-exist extract-dir)
               (extract-archive artifact-file extract-dir)
               ;; Find extracted directory (often has one top-level dir)
	               (let ((contents (directory (merge-pathnames "*" extract-dir))))
	                 (let ((source-dir (if (and (= (length contents) 1)
	                                            (uiop:directory-pathname-p (first contents)))
	                                       (first contents)
	                                       extract-dir)))
	                   (when (and verify
	                              (stringp expected-sha1)
	                              (not (stringp expected-sha256)))
	                     (let ((actual-sha1 (%content-sha1-hex source-dir)))
	                       (unless (string-equal actual-sha1 expected-sha1)
	                         (error 'clpm.errors:clpm-hash-mismatch-error
	                                :expected expected-sha1
	                                :actual actual-sha1
	                                :artifact url))))
	                   (multiple-value-bind (store-path tree-sha256)
	                       (clpm.store:store-source source-dir nil)
	                     (declare (ignore store-path))
	                     (values (clpm.store:get-source-path tree-sha256)
	                             tree-sha256
                             artifact-sha256))))))))
        (:git
         (let* ((url (getf (cdr source) :url))
                (commit (getf (cdr source) :commit))
                (clone-dir (merge-pathnames "repo/" tmp)))
           (fetch-git url clone-dir :commit commit)
           ;; Store source
           (multiple-value-bind (store-path tree-sha256)
               (clpm.store:store-source clone-dir nil)
             (declare (ignore store-path))
             (values (clpm.store:get-source-path tree-sha256) tree-sha256 nil))))
        (:path
         ;; Local path - hash and store like any other source.
         (let* ((path (or (second source)
                          (getf (cdr source) :path)))
                (expanded (clpm.platform:expand-path path))
                (pn (uiop:ensure-pathname expanded
                                          :defaults (uiop:getcwd)
                                          :want-existing nil))
                (abs (uiop:ensure-directory-pathname pn)))
           (unless (uiop:directory-exists-p abs)
             (error 'clpm.errors:clpm-fetch-error
                    :message (format nil "Path does not exist: ~A" (namestring abs))))
           (let ((tru (uiop:ensure-directory-pathname (truename abs))))
             (multiple-value-bind (store-path tree-sha256)
                 (clpm.store:store-source tru expected-sha256)
               (declare (ignore store-path))
               (values (clpm.store:get-source-path tree-sha256) tree-sha256 nil)))))))))

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

(defun fetch-lockfile-deps (lockfile &key lockfile-path (jobs 1))
  "Fetch all dependencies specified in LOCKFILE.
JOBS controls bounded concurrency for network/path/git fetches.

Returns list of (system-id . source-path) pairs sorted by system-id."
  (let* ((jobs (max 1 (or jobs 1)))
         (existing-results '())
         (tasks '()))
    (dolist (locked (clpm.project:lockfile-resolved lockfile))
      (let* ((system-id (clpm.project:locked-system-id locked))
             (release (clpm.project:locked-system-release locked))
             (source (clpm.project:locked-release-source release))
             (artifact-sha256 (clpm.project:locked-release-artifact-sha256 release))
             (tree-sha256 (clpm.project:locked-release-tree-sha256 release)))
        ;; Check if already in store.
        (let ((existing (when tree-sha256
                          (clpm.store:get-source-path tree-sha256))))
          (if existing
              (push (cons system-id existing) existing-results)
              (let ((source-spec (locked-source-to-spec source))
                    (expected (if (eq (clpm.project:locked-source-kind source) :path)
                                  tree-sha256
                                  artifact-sha256)))
                ;; Task = (system-id release source-spec expected old-tree-sha256)
                (push (list system-id release source-spec expected tree-sha256) tasks))))))

    (let ((fetched-results '())
          (updates '()))
      (labels ((fetch-task (task)
                 (destructuring-bind (system-id release source-spec expected old-tree-sha256)
                     task
                   (multiple-value-bind (path fetched-tree-sha256 fetched-artifact-sha256)
                       (fetch-artifact source-spec expected)
                     (values system-id path release old-tree-sha256
                             fetched-tree-sha256 fetched-artifact-sha256)))))
        (if (or (<= jobs 1) (<= (length tasks) 1))
            ;; Sequential.
            (dolist (task tasks)
              (multiple-value-bind (system-id path release old-tree-sha256
                                    fetched-tree-sha256 fetched-artifact-sha256)
                  (fetch-task task)
                (push (cons system-id path) fetched-results)
                (when (or fetched-tree-sha256 fetched-artifact-sha256)
                  (push (list release old-tree-sha256
                              fetched-tree-sha256 fetched-artifact-sha256)
                        updates))))
            ;; Parallel.
            (let ((queue tasks)
                  (queue-mutex (sb-thread:make-mutex :name "clpm.fetch.queue"))
                  (result-mutex (sb-thread:make-mutex :name "clpm.fetch.results"))
                  (err nil))
              (labels ((pop-task ()
                         (sb-thread:with-mutex (queue-mutex)
                           (when (and (null err) queue)
                             (pop queue))))
                       (record-error (c)
                         (sb-thread:with-mutex (queue-mutex)
                           (unless err
                             (setf err c)))))
                (let ((threads
                        (loop repeat jobs
                              collect
                              (sb-thread:make-thread
                               (lambda ()
                                 (loop for task = (pop-task) while task do
                                   (handler-case
                                       (multiple-value-bind (system-id path release old-tree-sha256
                                                             fetched-tree-sha256 fetched-artifact-sha256)
                                           (fetch-task task)
                                         (sb-thread:with-mutex (result-mutex)
                                           (push (cons system-id path) fetched-results)
                                           (when (or fetched-tree-sha256 fetched-artifact-sha256)
                                             (push (list release old-tree-sha256
                                                         fetched-tree-sha256 fetched-artifact-sha256)
                                                   updates))))
                                     (error (c)
                                       (record-error c)
                                       (return)))))
                               :name "clpm.fetch.worker"))))
                  (dolist (th threads)
                    (sb-thread:join-thread th))
                  (when err
                    (error err))))))

      ;; Apply backfills (and verify consistency) to the lockfile struct.
      (dolist (u updates)
        (destructuring-bind (release old-tree-sha256 fetched-tree-sha256 fetched-artifact-sha256)
            u
          ;; tree-sha256
          (when (stringp fetched-tree-sha256)
            (cond
              ((null old-tree-sha256)
               (setf (clpm.project:locked-release-tree-sha256 release) fetched-tree-sha256))
              ((not (string= old-tree-sha256 fetched-tree-sha256))
               (error 'clpm.errors:clpm-hash-mismatch-error
                      :expected old-tree-sha256
                      :actual fetched-tree-sha256
                      :artifact "source tree"))))
          ;; artifact-sha256 (for tarballs)
          (when (stringp fetched-artifact-sha256)
            (let ((existing (clpm.project:locked-release-artifact-sha256 release)))
              (cond
                ((null existing)
                 (setf (clpm.project:locked-release-artifact-sha256 release)
                       fetched-artifact-sha256))
                ((not (string= existing fetched-artifact-sha256))
                 (error 'clpm.errors:clpm-hash-mismatch-error
                        :expected existing
                        :actual fetched-artifact-sha256
                        :artifact "artifact"))))
            ;; Also backfill locked-source :sha256 for :tarball sources.
            (let ((src (clpm.project:locked-release-source release)))
              (when (and src (eq (clpm.project:locked-source-kind src) :tarball))
                (let ((src-sha256 (clpm.project:locked-source-sha256 src)))
                  (cond
                    ((null src-sha256)
                     (setf (clpm.project:locked-source-sha256 src) fetched-artifact-sha256))
                    ((not (string= src-sha256 fetched-artifact-sha256))
                     (error 'clpm.errors:clpm-hash-mismatch-error
                            :expected src-sha256
                            :actual fetched-artifact-sha256
                            :artifact "tarball sha256")))))))))

      ;; Persist lockfile (may include tree hash backfills).
      (when lockfile-path
        (let* ((lockfile-path (uiop:ensure-pathname lockfile-path
                                                    :want-file t
                                                    :want-existing nil))
               (tmp-path (merge-pathnames "clpm.lock.tmp"
                                          (uiop:pathname-directory-pathname lockfile-path))))
          (clpm.project:write-lock-file lockfile tmp-path)
          (rename-file tmp-path lockfile-path)))

      ;; Deterministic result ordering.
      (sort (append existing-results fetched-results) #'string< :key #'car)))))

(defun locked-source-to-spec (locked-source)
  "Convert a locked-source struct to a source specification plist."
  (let ((kind (clpm.project:locked-source-kind locked-source)))
    (ecase kind
      (:tarball
       (list :tarball
             :url (clpm.project:locked-source-url locked-source)
             :sha256 (clpm.project:locked-source-sha256 locked-source)
             :sha1 (clpm.project:locked-source-sha1 locked-source)))
      (:git
       (list :git
             :url (clpm.project:locked-source-url locked-source)
             :commit (clpm.project:locked-source-commit locked-source)))
      (:path
       (list :path (clpm.project:locked-source-path locked-source))))))
