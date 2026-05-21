;;;; test/store-streaming-test.lisp - Store streaming hashing/copy tests

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

(defun assert-user-error (thunk fmt &rest args)
  (handler-case
      (progn
        (funcall thunk)
        (apply #'fail fmt args))
    (clpm.errors:clpm-user-error ()
      t)))

(format t "Testing streaming store-artifact...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((big (merge-pathnames "big.bin" tmp))
         (chunk-size 8192)
         (chunks (/ (* 16 1024 1024) chunk-size))
         (buf (make-array chunk-size :element-type '(unsigned-byte 8))))
    ;; Deterministic contents.
    (dotimes (i chunk-size)
      (setf (aref buf i) (mod i 256)))
    (with-open-file (s big :direction :output
                           :element-type '(unsigned-byte 8)
                           :if-exists :supersede)
      (dotimes (i chunks)
        (declare (ignore i))
        (write-sequence buf s)))

    (let ((expected (clpm.crypto.sha256:bytes-to-hex
                     (clpm.crypto.sha256:sha256-file big))))
      (let ((stored (clpm.store:store-artifact big expected)))
        (assert-true (uiop:file-exists-p stored)
                     "Expected stored artifact to exist: ~A" (namestring stored))
        (let ((actual (clpm.crypto.sha256:bytes-to-hex
                       (clpm.crypto.sha256:sha256-file stored))))
          (assert-true (string= actual expected)
                       "Stored artifact hash mismatch"))
        ;; Avoid polluting the global cache store with a large file.
        (delete-file stored)))))

(format t "  Store streaming PASSED~%")

(format t "Testing store identities are digest-only...~%")

(clpm.store:with-temp-dir (tmp)
  (declare (ignore tmp))
  (let* ((store-parent
           (uiop:pathname-parent-directory-pathname
            (uiop:ensure-directory-pathname (clpm.platform:store-dir))))
         (escaped-root (merge-pathnames "escaped-source/" store-parent))
         (escaped-src (merge-pathnames "src/" escaped-root)))
    (ensure-directories-exist escaped-src)
    (unwind-protect
         (assert-user-error
          (lambda ()
            (clpm.store:get-source-path "../../../escaped-source"))
          "Expected invalid source identity to be rejected")
         (assert-user-error
          (lambda ()
            (clpm.store:get-artifact-path "../../../escaped-artifact"))
          "Expected invalid artifact identity to be rejected")
         (assert-user-error
          (lambda ()
            (clpm.store:get-build-path "../../../escaped-build"))
          "Expected invalid build identity to be rejected")
         (assert-user-error
          (lambda ()
            (clpm.store:compute-build-id "../../../escaped-source" nil))
          "Expected invalid source identity to be rejected before build-id derivation")
      (ignore-errors
       (uiop:delete-directory-tree escaped-root
                                   :validate t
                                   :if-does-not-exist :ignore)))))

(format t "  Store identity validation PASSED~%")
(format t "~%Store streaming tests PASSED!~%")
(sb-ext:exit :code 0)
