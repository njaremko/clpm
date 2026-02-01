;;;; test/extract-safety-test.lisp - Archive extraction safety tests

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

(defun write-text (path text)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (write-string text s)))

(format t "Testing archive extraction safety...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((tar (clpm.platform:find-tar))
         (work (merge-pathnames "work/" tmp))
         (dest (merge-pathnames "dest/" tmp))
         (outside (merge-pathnames "evil-outside.txt" tmp))
         (archive (merge-pathnames "evil.tar" tmp)))
    (unless tar
      (fail "tar not found in PATH"))
    (ensure-directories-exist work)
    (ensure-directories-exist dest)
    (write-text outside "pwned\n")

    ;; Create a tarball containing a path traversal entry.
    (multiple-value-bind (o e rc)
        (clpm.platform:run-program
         (list tar "-cf" (namestring archive) "../evil-outside.txt")
         :directory work
         :error-output :string)
      (declare (ignore o e))
      (assert-true (zerop rc) "tar failed to create malicious archive"))

    ;; Delete the outside file: extraction must not recreate it.
    (delete-file outside)
    (assert-true (not (uiop:file-exists-p outside))
                 "Expected outside file to be deleted before extraction")

    ;; Extraction must fail before writing anything.
    (let ((failed nil))
      (handler-case
          (progn
            (clpm.fetch:extract-archive archive dest)
            (setf failed nil))
        (clpm.errors:clpm-fetch-error ()
          (setf failed t)))
      (assert-true failed "Expected extraction to fail for unsafe tar entry"))

    (assert-true (not (uiop:file-exists-p outside))
                 "Extraction wrote outside destination: ~A" (namestring outside))))

(format t "  Archive extraction safety PASSED~%")
(format t "~%Extract safety tests PASSED!~%")
(sb-ext:exit :code 0)

