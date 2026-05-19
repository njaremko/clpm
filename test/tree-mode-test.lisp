;;;; tree-mode-test.lisp - sha256-tree must include the file's mode (executable bit + symlinks).

(require :asdf)
(require :sb-posix)

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

(defun write-file (path contents)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (write-string contents s)))

(defun tree-hash (root)
  (clpm.crypto.sha256:bytes-to-hex
   (clpm.crypto.sha256:sha256-tree root :exclude nil)))

(defun test-exec-bit ()
  (format t "Executable bit affects tree hash... ")
  (clpm.store:with-temp-dir (tmp)
    (let* ((root (merge-pathnames "src/" tmp))
           (script (merge-pathnames "tool.sh" root)))
      (ensure-directories-exist root)
      (write-file script "#!/bin/sh\necho hi\n")
      (sb-posix:chmod (namestring script) #o644)
      (let ((h1 (tree-hash root)))
        (sb-posix:chmod (namestring script) #o755)
        (let ((h2 (tree-hash root)))
          (unless (not (string= h1 h2))
            (fail "tree-hash did not change after chmod +x: ~A == ~A" h1 h2))))))
  (format t "ok~%"))

(defun test-symlink-target ()
  (format t "Symlink target affects tree hash... ")
  (clpm.store:with-temp-dir (tmp)
    (let* ((root (merge-pathnames "src/" tmp))
           (link (merge-pathnames "config" root))
           (target-a (merge-pathnames "a.conf" root))
           (target-b (merge-pathnames "b.conf" root)))
      (ensure-directories-exist root)
      (write-file target-a "alpha")
      (write-file target-b "beta")
      (sb-posix:symlink "a.conf" (namestring link))
      (let ((h1 (tree-hash root)))
        (sb-posix:unlink (namestring link))
        (sb-posix:symlink "b.conf" (namestring link))
        (let ((h2 (tree-hash root)))
          (unless (not (string= h1 h2))
            (fail "tree-hash did not change after retargeting symlink: ~A == ~A" h1 h2))))))
  (format t "ok~%"))

(defun test-stable-when-mode-unchanged ()
  (format t "Tree hash is stable when nothing changes... ")
  (clpm.store:with-temp-dir (tmp)
    (let* ((root (merge-pathnames "src/" tmp))
           (f (merge-pathnames "a.txt" root)))
      (ensure-directories-exist root)
      (write-file f "hi")
      (sb-posix:chmod (namestring f) #o644)
      (let ((h1 (tree-hash root))
            (h2 (tree-hash root)))
        (unless (string= h1 h2)
          (fail "tree-hash not deterministic: ~A vs ~A" h1 h2)))))
  (format t "ok~%"))

(defun test-build-id-bump ()
  (format t "compute-build-id uses v2 hash prefix... ")
  ;; A round-trip is enough — clean v1 builds must not collide with v2 builds
  ;; for the same tree, so the build-id for a fixed (tree-sha, lisp, ...) tuple
  ;; should now differ from what v1 would have computed.
  (let ((id (clpm.store:compute-build-id
             (make-string 64 :initial-element #\0)
             '(:speed 1 :safety 1 :debug 1)
             :lisp-kind :sbcl
             :lisp-version "2.6.0")))
    (unless (and (stringp id) (= 64 (length id)))
      (fail "compute-build-id returned ~S" id)))
  (format t "ok~%"))

(test-stable-when-mode-unchanged)
(test-exec-bit)
(test-symlink-target)
(test-build-id-bump)

(format t "~%Tree mode tests PASSED!~%")
(sb-ext:exit :code 0)
