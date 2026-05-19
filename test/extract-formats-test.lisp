;;;; test/extract-formats-test.lisp - xz/bz2 archive extraction

(require :asdf)

(let* ((this-file (or *load-truename* *load-pathname*))
       (test-dir (uiop:pathname-directory-pathname this-file))
       (repo-root (uiop:pathname-parent-directory-pathname test-dir)))
  (push repo-root asdf:*central-registry*))

(format t "Loading CLPM...~%")
(handler-case
    (asdf:load-system :clpm :verbose nil)
  (error (c)
    (format *error-output* "Failed to load CLPM: ~A~%" c)
    (sb-ext:exit :code 1)))
(format t "CLPM loaded.~%")

(defun fail (fmt &rest args)
  (apply #'format *error-output* (concatenate 'string "FAIL: " fmt "~%") args)
  (sb-ext:exit :code 1))

(defun assert-true (x fmt &rest args)
  (unless x (apply #'fail fmt args)))

(defun assert-eql (expected actual)
  (unless (eql expected actual)
    (fail "expected ~S, got ~S" expected actual)))

(defun write-text (path text)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (write-string text s)))

(defun has-tool (name)
  (clpm.platform:which name))

(defun build-archive (tar src-dir archive flags)
  "Run `tar FLAGS -f ARCHIVE -C SRC-DIR .`. Returns t on success."
  (multiple-value-bind (o e rc)
      (clpm.platform:run-program
       (append (list tar)
               (uiop:split-string flags)
               (list "-f" (namestring archive)
                     "-C" (namestring src-dir) "."))
       :error-output :string)
    (declare (ignore o))
    (when (not (zerop rc))
      (format *error-output* "tar ~A failed (rc=~D, err=~A)~%" flags rc e))
    (zerop rc)))

(format t "Testing xz/bz2 extraction round-trip...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((tar (clpm.platform:find-tar))
         (src (merge-pathnames "src/" tmp))
         (sub (merge-pathnames "sub/" src))
         (greeting (merge-pathnames "greet.txt" src))
         (deep (merge-pathnames "data.txt" sub)))
    (unless tar (fail "tar not found"))
    (ensure-directories-exist src)
    (ensure-directories-exist sub)
    (write-text greeting "hello-from-tarball")
    (write-text deep "deeper")

    (dolist (case '(("xz"     ".tar.xz"  "-cJ")
                    ("bzip2"  ".tar.bz2" "-cj")))
      (destructuring-bind (tool ext flags) case
        (cond
          ((not (has-tool tool))
           (format t "  skipping ~A (~A not installed)~%" ext tool))
          (t
           (let* ((archive (merge-pathnames (concatenate 'string "pkg" ext) tmp))
                  (dest (merge-pathnames (concatenate 'string "out" ext "/") tmp)))
             (unless (build-archive tar src archive flags)
               (fail "could not build ~A archive" ext))
             (ensure-directories-exist dest)
             (clpm.fetch:extract-archive archive dest)
             (let ((g (merge-pathnames "greet.txt" dest))
                   (d (merge-pathnames "sub/data.txt" dest)))
               (assert-true (uiop:file-exists-p g)
                            "Expected ~A to extract greet.txt" ext)
               (assert-true (uiop:file-exists-p d)
                            "Expected ~A to extract sub/data.txt" ext)
               (assert-true (string= "hello-from-tarball"
                                     (uiop:read-file-string g))
                            "Extracted greet.txt has wrong contents")
               (assert-true (string= "deeper" (uiop:read-file-string d))
                            "Extracted sub/data.txt has wrong contents"))
             (format t "  ~A round-trip OK~%" ext))))))))

(format t "Testing path-traversal safety for xz/bz2...~%")
(clpm.store:with-temp-dir (tmp)
  (let* ((tar (clpm.platform:find-tar))
         (work (merge-pathnames "work/" tmp))
         (dest (merge-pathnames "dest/" tmp))
         (outside (merge-pathnames "evil-outside.txt" tmp)))
    (unless tar (fail "tar not found"))
    (ensure-directories-exist work)
    (ensure-directories-exist dest)
    (write-text outside "pwned")

    (dolist (case '((".tar.xz"  "-cJ" "xz")
                    (".tar.bz2" "-cj" "bzip2")))
      (destructuring-bind (ext flags tool) case
        (cond
          ((not (has-tool tool))
           (format t "  skipping ~A (~A not installed)~%" ext tool))
          (t
           (let ((archive (merge-pathnames (concatenate 'string "evil" ext) tmp)))
             (multiple-value-bind (o e rc)
                 (clpm.platform:run-program
                  (append (list tar)
                          (uiop:split-string flags)
                          (list "-f" (namestring archive) "../evil-outside.txt"))
                  :directory work
                  :error-output :string)
               (declare (ignore o e))
               (assert-true (zerop rc) "could not create unsafe ~A archive" ext))
             (delete-file outside)
             (let ((failed nil))
               (handler-case
                   (clpm.fetch:extract-archive archive dest)
                 (clpm.errors:clpm-fetch-error () (setf failed t)))
               (assert-true failed
                            "Unsafe ~A entry was not rejected" ext))
             (assert-true (not (uiop:file-exists-p outside))
                          "~A extraction wrote outside its dir" ext)
             (write-text outside "pwned")
             (format t "  ~A path-traversal rejected~%" ext))))))))

(format t "Testing unsupported format still rejected...~%")
(clpm.store:with-temp-dir (tmp)
  (let ((archive (merge-pathnames "blob.7z" tmp))
        (dest (merge-pathnames "out/" tmp)))
    (write-text archive "not a real archive")
    (let ((failed nil))
      (handler-case
          (clpm.fetch:extract-archive archive dest)
        (clpm.errors:clpm-fetch-error () (setf failed t)))
      (assert-true failed "Expected .7z to be rejected as unsupported"))))
(format t "  unsupported format still rejected~%")

(format t "~%Extract formats tests PASSED!~%")
(sb-ext:exit :code 0)
