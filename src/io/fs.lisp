;;;; io/fs.lisp - Filesystem helpers (SBCL-focused, no external deps)

(in-package #:clpm.io.fs)

(defun %collect-directory (dir)
  "Return a list of pathnames for entries in DIR.

This attempts to include extensionless files and dotfiles across platforms by
combining a few DIRECTORY patterns and deduplicating results."
  (let* ((dir (uiop:ensure-directory-pathname dir))
         (patterns (list "*.*" "*" ".*"))
         (seen (make-hash-table :test 'equal))
         (entries '()))
    (dolist (pat patterns)
      (dolist (p (ignore-errors (directory (merge-pathnames pat dir))))
        (let ((key (namestring p)))
          (unless (gethash key seen)
            (setf (gethash key seen) t)
            (push p entries)))))
    entries))

(defun list-directory-entries (dir &key (exclude nil))
  "List immediate entries in DIR as pathnames, including dotfiles.

EXCLUDE is a list of entry names (strings) to ignore (directories or files)."
  (let ((entries (%collect-directory dir))
        (result '()))
    (dolist (p entries)
      (let ((name (if (uiop:directory-pathname-p p)
                      (car (last (pathname-directory (uiop:ensure-directory-pathname p))))
                      (file-namestring p))))
        (unless (and exclude (member name exclude :test #'string=))
          (push p result))))
    (sort result #'string< :key #'namestring)))

(defun %rel-unix-path (root file)
  "Return FILE's path relative to ROOT as a slash-separated string."
  (let* ((root (uiop:ensure-directory-pathname root))
         (rel (uiop:enough-pathname file root))
         (dir (pathname-directory rel))
         (dir-parts (cond
                      ((null dir) '())
                      ((and (consp dir) (keywordp (first dir))) (rest dir))
                      ((consp dir) dir)
                      (t '())))
         (name (pathname-name rel))
         (type (pathname-type rel)))
    (with-output-to-string (s)
      (dolist (part dir-parts)
        (when part
          (write-string part s)
          (write-char #\/ s)))
      (when name
        (write-string name s))
      (when type
        (write-char #\. s)
        (write-string type s)))))

(defun walk-files (root &key (exclude '(".git" ".hg" ".svn" ".clpm")))
  "Walk ROOT and return (rel-path . absolute-path) for all files.

REL-PATH is a deterministic, slash-separated string, sorted lexicographically."
  (let ((root (uiop:ensure-directory-pathname
               (truename (uiop:ensure-directory-pathname root))))
        (files '()))
    (labels ((walk (dir)
               (dolist (entry (list-directory-entries dir :exclude exclude))
                 (cond
                   ((uiop:directory-pathname-p entry)
                    (walk entry))
                   (t
                    (push (cons (%rel-unix-path root entry) entry) files))))))
      (walk root))
    (sort files #'string< :key #'car)))
