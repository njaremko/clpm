;;;; test/build-parallel-test.lisp - Parallel build scheduling tests

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

(defun assert-eql (expected actual)
  (unless (eql expected actual)
    (fail "Assertion failed: expected ~S, got ~S" expected actual)))

(defun write-text (path text)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (write-string text s)))

(defun write-simple-system (root name)
  (let* ((asd (merge-pathnames (format nil "~A.asd" name) root))
         (src-dir (merge-pathnames "src/" root))
         (src (merge-pathnames (format nil "src/~A.lisp" name) root)))
    (ensure-directories-exist src-dir)
    (write-text
     asd
     (with-output-to-string (s)
       (format s ";;;; ~A.asd~%~%" name)
       (format s "(asdf:defsystem ~S~%  :version ~S~%  :serial t~%  :pathname ~S~%  :components ((:file ~S)))~%"
               name "0.1.0" "src" name)))
    (write-text
     src
     (with-output-to-string (s)
       (format s ";;;; ~A.lisp~%~%" name)
       (format s "(defpackage #:~A (:use #:cl) (:export #:ping))~%~%" name)
       (format s "(in-package #:~A)~%~%" name)
       (format s "(defun ping () :ok)~%")))))

(defun lock-entry-for (name tree-sha256)
  (let* ((src (clpm.project:make-locked-source
               :kind :path
               :path (format nil "/dev/null/~A" name)
               :sha256 tree-sha256))
         (rel (clpm.project:make-locked-release
               :name name
               :version "0.1.0"
               :source src
               :artifact-sha256 nil
               :tree-sha256 tree-sha256)))
    (clpm.project:make-locked-system
     :id name
     :release rel
     :deps nil)))

(format t "Testing parallel build scheduling...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (workspace (merge-pathnames "ws/" tmp))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist workspace)
    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)

           ;; Create three independent local systems.
           (let* ((a-root (merge-pathnames "a/" workspace))
                  (b-root (merge-pathnames "b/" workspace))
                  (c-root (merge-pathnames "c/" workspace)))
             (ensure-directories-exist a-root)
             (ensure-directories-exist b-root)
             (ensure-directories-exist c-root)
             (write-simple-system a-root "a")
             (write-simple-system b-root "b")
             (write-simple-system c-root "c")

             ;; Store sources and build lockfile/source-paths.
             (multiple-value-bind (a-store a-tree)
                 (clpm.store:store-source a-root nil)
               (declare (ignore a-store))
               (multiple-value-bind (b-store b-tree)
                   (clpm.store:store-source b-root nil)
                 (declare (ignore b-store))
                 (multiple-value-bind (c-store c-tree)
                     (clpm.store:store-source c-root nil)
                   (declare (ignore c-store))
                   (let* ((lock (clpm.project:make-lockfile
                                :format 1
                                :generated-at (clpm.project:rfc3339-timestamp)
                                :project-name "proj"
                                :clpm-version "0.1.0"
                                :registries nil
                                :resolved (list (lock-entry-for "a" a-tree)
                                                (lock-entry-for "b" b-tree)
                                                (lock-entry-for "c" c-tree))))
                          (source-paths (list (cons "a" (clpm.store:get-source-path a-tree))
                                              (cons "b" (clpm.store:get-source-path b-tree))
                                              (cons "c" (clpm.store:get-source-path c-tree))))
                          (resolution
                            (clpm.solver::make-resolution
                             :systems (list (list :system "a" :name "a" :version "0.1.0" :release-ref "a@0.1.0")
                                            (list :system "b" :name "b" :version "0.1.0" :release-ref "b@0.1.0")
                                            (list :system "c" :name "c" :version "0.1.0" :release-ref "c@0.1.0"))
                             :graph (list (cons "a" nil)
                                          (cons "b" nil)
                                          (cons "c" nil)))))
                     (let ((results (clpm.build:build-all resolution lock source-paths
                                                          :jobs 4
                                                          :compile-options nil)))
                       (assert-eql 3 (length results))
                       (dolist (pair results)
                         (assert-true (clpm.store:build-exists-p (cdr pair))
                                      "Expected build to exist for ~A" (car pair))))))))))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "  Parallel build scheduling PASSED~%")
(format t "~%Build parallel tests PASSED!~%")
(sb-ext:exit :code 0)

