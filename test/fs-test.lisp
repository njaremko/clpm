;;;; test/fs-test.lisp - Filesystem walking and tree hashing tests

(require :asdf)

;; Add repo root directory to ASDF load path.
(let* ((this-file (or *load-truename* *load-pathname*))
       (test-dir (uiop:pathname-directory-pathname this-file))
       (repo-root (uiop:pathname-parent-directory-pathname test-dir)))
  (push repo-root asdf:*central-registry*))

;; Load CLPM
(format t "Loading CLPM...~%")
(handler-case
    (asdf:load-system :clpm)
  (error (c)
    (format *error-output* "Failed to load CLPM: ~A~%" c)
    (sb-ext:exit :code 1)))

(format t "CLPM loaded successfully.~%~%")

(defun fail (fmt &rest args)
  (apply #'format *error-output* (concatenate 'string fmt "~%") args)
  (sb-ext:exit :code 1))

(defun assert-true (x fmt &rest args)
  (unless x
    (apply #'fail fmt args)))

(defun write-file (path contents)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (write-string contents s)))

(defun read-file (path)
  (with-open-file (s path :direction :input :external-format :utf-8)
    (let ((buf (make-string (file-length s))))
      (read-sequence buf s)
      buf)))

(format t "Testing filesystem walker + tree hash...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((root (merge-pathnames "root/" tmp))
         (nested (merge-pathnames "dir/" root))
         (dest (merge-pathnames "dest/" tmp))
         (file1 (merge-pathnames "file" root))
         (hidden (merge-pathnames ".hidden" root))
         (inner (merge-pathnames ".inner" nested)))
    (ensure-directories-exist nested)
    (write-file file1 "a")
    (write-file hidden "b")
    (write-file inner "c")

    ;; walk-files includes extensionless and dotfiles
    (let ((paths (mapcar #'car (clpm.io.fs:walk-files root :exclude nil))))
      (assert-true (member ".hidden" paths :test #'string=)
                   "walk-files missing .hidden: ~S" paths)
      (assert-true (member "file" paths :test #'string=)
                   "walk-files missing extensionless file: ~S" paths)
      (assert-true (member "dir/.inner" paths :test #'string=)
                   "walk-files missing nested dotfile: ~S" paths))

    ;; sha256-tree changes when a dotfile changes
    (let* ((h1 (clpm.crypto.sha256:bytes-to-hex
                (clpm.crypto.sha256:sha256-tree root :exclude nil))))
      (write-file hidden "b2")
      (let ((h2 (clpm.crypto.sha256:bytes-to-hex
                 (clpm.crypto.sha256:sha256-tree root :exclude nil))))
        (assert-true (not (string= h1 h2))
                     "sha256-tree did not change after modifying .hidden")))

    ;; copy-directory-tree includes dotfiles and extensionless files
    (clpm.store::copy-directory-tree root dest)
    (let ((dest-file1 (merge-pathnames "file" dest))
          (dest-hidden (merge-pathnames ".hidden" dest))
          (dest-inner (merge-pathnames "dir/.inner" dest)))
      (assert-true (uiop:file-exists-p dest-file1) "copied file missing")
      (assert-true (uiop:file-exists-p dest-hidden) "copied .hidden missing")
      (assert-true (uiop:file-exists-p dest-inner) "copied dir/.inner missing")
      (assert-true (string= (read-file dest-file1) "a") "copied file contents mismatch")
      (assert-true (string= (read-file dest-hidden) "b2") "copied .hidden contents mismatch")
      (assert-true (string= (read-file dest-inner) "c") "copied inner contents mismatch"))))

(format t "  Filesystem walker + tree hash PASSED~%")

(format t "~%FS tests PASSED!~%")
(sb-ext:exit :code 0)

