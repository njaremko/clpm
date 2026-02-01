;;;; test/fetch-parallel-test.lisp - Parallel fetch determinism tests

(require :asdf)
(require :sb-posix)

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

(defun assert-equal (expected actual)
  (unless (equal expected actual)
    (fail "Assertion failed: expected ~S, got ~S" expected actual)))

(defun write-text (path text)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (write-string text s)))

(defun make-path-lock (system-id dep-root tree-sha256)
  (let* ((src (clpm.project:make-locked-source
               :kind :path
               :path (namestring dep-root)
               :sha256 tree-sha256))
         (rel (clpm.project:make-locked-release
               :name system-id
               :version "0.0.0"
               :source src
               :artifact-sha256 nil
               :tree-sha256 tree-sha256))
         (sys (clpm.project:make-locked-system
               :id system-id
               :release rel
               :deps nil)))
    sys))

(defun run-fetch-with-jobs (clpm-home lockfile jobs)
  (let ((old-home (sb-posix:getenv "CLPM_HOME")))
    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)
           (let ((results (clpm.fetch:fetch-lockfile-deps lockfile :jobs jobs)))
             (mapcar #'car results)))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "Testing parallel fetch determinism...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((workspace (merge-pathnames "ws/" tmp))
         (dep-a (merge-pathnames "dep-a/" workspace))
         (dep-b (merge-pathnames "dep-b/" workspace))
         (dep-c (merge-pathnames "dep-c/" workspace))
         (home-1 (merge-pathnames "home-1/" workspace))
         (home-4 (merge-pathnames "home-4/" workspace)))
    (ensure-directories-exist workspace)
    (ensure-directories-exist dep-a)
    (ensure-directories-exist dep-b)
    (ensure-directories-exist dep-c)
    (ensure-directories-exist home-1)
    (ensure-directories-exist home-4)

    (write-text (merge-pathnames "a.txt" dep-a) "a")
    (write-text (merge-pathnames "b.txt" dep-b) "b")
    (write-text (merge-pathnames "c.txt" dep-c) "c")

    (let* ((tree-a (clpm.crypto.sha256:bytes-to-hex
                    (clpm.crypto.sha256:sha256-tree dep-a)))
           (tree-b (clpm.crypto.sha256:bytes-to-hex
                    (clpm.crypto.sha256:sha256-tree dep-b)))
           (tree-c (clpm.crypto.sha256:bytes-to-hex
                    (clpm.crypto.sha256:sha256-tree dep-c)))
           (lock (clpm.project:make-lockfile
                  :format 1
                  :generated-at (clpm.project:rfc3339-timestamp)
                  :project-name "proj"
                  :clpm-version "0.1.0"
                  :registries nil
                  :resolved (list (make-path-lock "a" dep-a tree-a)
                                  (make-path-lock "b" dep-b tree-b)
                                  (make-path-lock "c" dep-c tree-c)))))
      (let ((ids-1 (run-fetch-with-jobs home-1 lock 1))
            (ids-4 (run-fetch-with-jobs home-4 lock 4)))
        (assert-equal '("a" "b" "c") ids-1)
        (assert-equal ids-1 ids-4)))))

(format t "  Parallel fetch determinism PASSED~%")
(format t "~%Fetch parallel tests PASSED!~%")
(sb-ext:exit :code 0)
