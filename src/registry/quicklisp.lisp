;;;; registry/quicklisp.lisp - Quicklisp dist registry handling

(in-package #:clpm.registry)

(defun %starts-with-p (string prefix)
  (let ((slen (length string))
        (plen (length prefix)))
    (and (>= slen plen)
         (string= prefix string :end2 plen))))

(defun %upgrade-quicklisp-url (url)
  "Upgrade well-known Quicklisp HTTP URLs to HTTPS."
  (cond
    ((and (stringp url)
          (%starts-with-p url "http://beta.quicklisp.org/"))
     (concatenate 'string "https://beta.quicklisp.org/" (subseq url (length "http://beta.quicklisp.org/"))))
    ((and (stringp url)
          (%starts-with-p url "http://www.quicklisp.org/"))
     (concatenate 'string "https://www.quicklisp.org/" (subseq url (length "http://www.quicklisp.org/"))))
    ((and (stringp url)
          (%starts-with-p url "http://quicklisp.org/"))
     (concatenate 'string "https://quicklisp.org/" (subseq url (length "http://quicklisp.org/"))))
    (t url)))

(defun %quicklisp-distinfo-path (registry)
  (merge-pathnames "distinfo.txt"
                   (registry-local-path (registry-name registry))))

(defun %quicklisp-systems-path (registry)
  (merge-pathnames "systems.txt"
                   (registry-local-path (registry-name registry))))

(defun %quicklisp-releases-path (registry)
  (merge-pathnames "releases.txt"
                   (registry-local-path (registry-name registry))))

(defun %read-lines (path)
  (with-open-file (s path :direction :input :external-format :utf-8)
    (loop for line = (read-line s nil nil)
          while line
          collect line)))

(defun %parse-distinfo (lines)
  "Parse distinfo.txt style lines into a hash table (downcased keys)."
  (let ((tbl (make-hash-table :test 'equal)))
    (dolist (line lines)
      (let ((line (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
        (when (plusp (length line))
          (let ((pos (position #\: line)))
            (when pos
              (let ((k (string-downcase (string-trim '(#\Space #\Tab)
                                                     (subseq line 0 pos))))
                    (v (string-trim '(#\Space #\Tab)
                                    (subseq line (1+ pos)))))
                (setf (gethash k tbl) v)))))))
    tbl))

(defun %find-yyyymmdd (s)
  "Return the first 8-digit YYYYMMDD substring in S, or NIL."
  (when (stringp s)
    (loop for i from 0 to (- (length s) 8) do
      (let ((sub (subseq s i (+ i 8))))
        (when (every #'digit-char-p sub)
          (return sub))))))

(defun %quicklisp-version-from-prefix (prefix)
  "Return a semver-ish version string for PREFIX.

Quicklisp typically uses prefixes like \"proj-20250622-git\"."
  (let ((digits (%find-yyyymmdd prefix)))
    (if digits
        (format nil "0.0.~A" digits)
        "0.0.0")))

(defun %quicklisp-builtin-system-p (system-id)
  "Return true if SYSTEM-ID should be treated as built-in."
  (or (string-equal system-id "asdf")
      (string-equal system-id "uiop")
      (and (>= (length system-id) 3)
           (string-equal (subseq system-id 0 3) "sb-"))))

(defun %split-fields (line)
  (remove-if (lambda (s) (zerop (length s)))
             (uiop:split-string line :separator '(#\Space #\Tab))))

(defun %parse-quicklisp-systems (path)
  "Parse Quicklisp systems.txt at PATH.

Returns (values systems-by-project deps-by-system)."
  (let ((systems-by-project (make-hash-table :test 'equal))
        (deps-by-system (make-hash-table :test 'equal)))
    (dolist (line (%read-lines path))
      (let ((line (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
        (when (and (plusp (length line))
                   (not (char= (char line 0) #\#)))
          (let* ((parts (%split-fields line)))
            (when (<= 3 (length parts))
              (let ((project (first parts))
                    (system (third parts))
                    (deps (cdddr parts)))
                (push system (gethash project systems-by-project))
                (setf (gethash system deps-by-system) deps)))))))
    ;; Normalize
    (maphash (lambda (k v)
               (setf (gethash k systems-by-project)
                     (sort (remove-duplicates v :test #'string=) #'string<)))
             systems-by-project)
    (values systems-by-project deps-by-system)))

(defun %parse-quicklisp-releases (path)
  "Parse Quicklisp releases.txt at PATH.

Returns a hash table mapping project -> plist(:url :sha1 :version :prefix)."
  (let ((tbl (make-hash-table :test 'equal)))
    (dolist (line (%read-lines path))
      (let ((line (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
        (when (and (plusp (length line))
                   (not (char= (char line 0) #\#)))
          (let* ((parts (%split-fields line)))
            (when (<= 6 (length parts))
              (let* ((project (first parts))
                     (url (%upgrade-quicklisp-url (second parts)))
                     ;; fields: project url size file-md5 content-sha1 prefix
                     (sha1 (fifth parts))
                     (prefix (sixth parts))
                     (version (%quicklisp-version-from-prefix prefix)))
                (setf (gethash project tbl)
                      (list :url url :sha1 sha1 :version version :prefix prefix))))))))
    tbl))

(defun clone-quicklisp-registry (registry)
  "Ensure quicklisp registry metadata exists locally and load it."
  (let ((distinfo (%quicklisp-distinfo-path registry))
        (systems (%quicklisp-systems-path registry))
        (releases (%quicklisp-releases-path registry)))
    (ensure-directories-exist distinfo)
    (if (and (uiop:file-exists-p distinfo)
             (uiop:file-exists-p systems)
             (uiop:file-exists-p releases))
        (load-quicklisp-registry-snapshot registry)
        (update-quicklisp-registry registry))
    registry))

(defun update-quicklisp-registry (registry)
  "Download quicklisp dist metadata and load it."
  (let* ((url (registry-url registry))
         (distinfo (%quicklisp-distinfo-path registry))
         (systems (%quicklisp-systems-path registry))
         (releases (%quicklisp-releases-path registry)))
    (unless (and (stringp url) (plusp (length url)))
      (error 'clpm.errors:clpm-fetch-error
             :message "Missing Quicklisp dist URL"))
    ;; Download distinfo + indexes.
    (clpm.fetch::fetch-url (%upgrade-quicklisp-url url) distinfo :progress nil)
    (let* ((tbl (%parse-distinfo (%read-lines distinfo)))
           (systems-url (%upgrade-quicklisp-url (gethash "system-index-url" tbl)))
           (releases-url (%upgrade-quicklisp-url (gethash "release-index-url" tbl))))
      (unless (and (stringp systems-url) (plusp (length systems-url)))
        (error 'clpm.errors:clpm-fetch-error
               :message "Quicklisp distinfo missing system-index-url"
               :url url))
      (unless (and (stringp releases-url) (plusp (length releases-url)))
        (error 'clpm.errors:clpm-fetch-error
               :message "Quicklisp distinfo missing release-index-url"
               :url url))
      (clpm.fetch::fetch-url systems-url systems :progress nil)
      (clpm.fetch::fetch-url releases-url releases :progress nil))
    (load-quicklisp-registry-snapshot registry)
    registry))

(defun load-quicklisp-registry-snapshot (registry)
  "Load snapshot + release metadata for a quicklisp registry."
  (let* ((systems-path (%quicklisp-systems-path registry))
         (releases-path (%quicklisp-releases-path registry)))
    (unless (and (uiop:file-exists-p systems-path)
                 (uiop:file-exists-p releases-path))
      (error 'clpm.errors:clpm-parse-error
             :message "Quicklisp registry metadata missing"
             :file (if (uiop:file-exists-p systems-path) releases-path systems-path)))

    (multiple-value-bind (systems-by-project deps-by-system)
        (%parse-quicklisp-systems systems-path)
      (let* ((rel-info (%parse-quicklisp-releases releases-path))
             (release-table (make-hash-table :test 'equal))
             (releases '())
             (provides '()))
        ;; Build release metadata table.
        (maphash
         (lambda (project info)
           (let* ((url (getf info :url))
                  (sha1 (getf info :sha1))
                  (version (getf info :version))
                  (release-ref (format nil "~A@~A" project version))
                  (systems (gethash project systems-by-project))
                  (system-deps
                    (mapcar
                     (lambda (sys)
                       (let* ((deps (or (gethash sys deps-by-system) '()))
                              (deps (remove-if #'%quicklisp-builtin-system-p deps))
                              (dep-pairs (mapcar (lambda (d) (cons d nil)) deps)))
                         (cons sys dep-pairs)))
                     (or systems '()))))
             (push release-ref releases)
             (dolist (sys (or systems '()))
               (push (cons sys release-ref) provides))
             (setf (gethash release-ref release-table)
                   (make-release-metadata
                    :name project
                    :version version
                    :source (list :tarball :url url :sha1 sha1)
                    :artifact-sha256 nil
                    :systems systems
                    :system-deps system-deps))))
         rel-info)

        ;; Publish snapshot + table (deterministic ordering).
        (setf (registry-release-table registry) release-table)
        (setf (registry-snapshot-sig-sha256 registry) nil)
        (setf (registry-snapshot registry)
              (make-snapshot
               :format 1
               :generated-at nil
               :releases (sort releases #'string<)
               :provides (sort provides #'string<
                              :key (lambda (p)
                                     (format nil "~A~C~A" (car p) #\Null (cdr p)))))))
      registry)))
