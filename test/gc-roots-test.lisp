;;;; test/gc-roots-test.lisp - GC roots tests (projects index + lock/env scanning)

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

(defun store-sample-source (tmp name)
  (let ((src (merge-pathnames (format nil "~A-src/" name) tmp)))
    (ensure-directories-exist src)
    (write-text (merge-pathnames "main.lisp" src)
                (format nil ";;;; ~A~%~%(defun ~A () :ok)~%" name name))
    (multiple-value-bind (stored tree-sha256)
        (clpm.store:store-source src nil)
      (declare (ignore stored))
      tree-sha256)))

(defun store-sample-artifact (text expected-sha256)
  (let ((bytes (map '(vector (unsigned-byte 8)) #'char-code text)))
    (clpm.store:store-artifact bytes expected-sha256)
    expected-sha256))

(defun store-sample-build (tmp tree-sha256)
  (let* ((fasl-dir (merge-pathnames (format nil "fasl-~A/" tree-sha256) tmp))
         (build-id (clpm.store:compute-build-id tree-sha256 nil)))
    (ensure-directories-exist fasl-dir)
    (write-text (merge-pathnames "dummy.fasl" fasl-dir) "not a real fasl")
    (clpm.store:store-build build-id fasl-dir fasl-dir
                            (list :systems (list "dummy")
                                  :compile-options nil))
    build-id))

(defun make-lockfile-for (project-name system-id tree-sha256 artifact-sha256)
  (let* ((src (clpm.project:make-locked-source
               :kind :tarball
               :url (format nil "file://~A" system-id)
               :sha256 artifact-sha256))
         (rel (clpm.project:make-locked-release
               :name system-id
               :version "1.0.0"
               :source src
               :artifact-sha256 artifact-sha256
               :tree-sha256 tree-sha256))
         (sys (clpm.project:make-locked-system
               :id system-id
               :release rel
               :deps nil)))
    (clpm.project:make-lockfile
     :format 1
     :generated-at (clpm.project:rfc3339-timestamp)
     :project-name project-name
     :clpm-version "0.1.0"
     :registries nil
     :resolved (list sys))))

(format t "Testing GC roots...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (workspace (merge-pathnames "ws/" tmp))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist workspace)

    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)

           ;; Store 3 sets of entries; only A and B are referenced by roots.
           (let* ((tree-a (store-sample-source tmp "a"))
                  (tree-b (store-sample-source tmp "b"))
                  (tree-c (store-sample-source tmp "c"))
                  (artifact-a (clpm.crypto.sha256:bytes-to-hex
                              (clpm.crypto.sha256:sha256 "artifact-a")))
                  (artifact-b (clpm.crypto.sha256:bytes-to-hex
                              (clpm.crypto.sha256:sha256 "artifact-b")))
                  (artifact-c (clpm.crypto.sha256:bytes-to-hex
                              (clpm.crypto.sha256:sha256 "artifact-c")))
                  (build-a (store-sample-build tmp tree-a))
                  (build-b (store-sample-build tmp tree-b))
                  (build-c (store-sample-build tmp tree-c)))
             (store-sample-artifact "artifact-a" artifact-a)
             (store-sample-artifact "artifact-b" artifact-b)
             (store-sample-artifact "artifact-c" artifact-c)

             (let* ((proj-a (merge-pathnames "proj-a/" workspace))
                    (proj-b (merge-pathnames "proj-b/" workspace))
                    (lock-a (make-lockfile-for "proj-a" "a" tree-a artifact-a))
                    (lock-b (make-lockfile-for "proj-b" "b" tree-b artifact-b)))
               (ensure-directories-exist proj-a)
               (ensure-directories-exist proj-b)
               (clpm.project:write-lock-file lock-a (merge-pathnames "clpm.lock" proj-a))
               (clpm.project:write-lock-file lock-b (merge-pathnames "clpm.lock" proj-b))

               ;; Activation writes .clpm/env.sexp and upserts projects.sxp.
               (clpm.build:activate-project proj-a lock-a :compile-options nil)
               (clpm.build:activate-project proj-b lock-b :compile-options nil)

               (let ((index-path (merge-pathnames "projects.sxp" (clpm.platform:data-dir))))
                 (assert-true (uiop:file-exists-p index-path)
                              "Expected projects index to exist"))

               ;; Sanity: everything exists before GC.
               (assert-true (clpm.store:source-exists-p tree-a) "Expected source A to exist pre-GC")
               (assert-true (clpm.store:source-exists-p tree-b) "Expected source B to exist pre-GC")
               (assert-true (clpm.store:source-exists-p tree-c) "Expected source C to exist pre-GC")
               (assert-true (clpm.store:artifact-exists-p artifact-a) "Expected artifact A to exist pre-GC")
               (assert-true (clpm.store:artifact-exists-p artifact-b) "Expected artifact B to exist pre-GC")
               (assert-true (clpm.store:artifact-exists-p artifact-c) "Expected artifact C to exist pre-GC")
               (assert-true (clpm.store:build-exists-p build-a) "Expected build A to exist pre-GC")
               (assert-true (clpm.store:build-exists-p build-b) "Expected build B to exist pre-GC")
               (assert-true (clpm.store:build-exists-p build-c) "Expected build C to exist pre-GC")

               ;; GC should keep referenced entries (A, B) and remove unreferenced (C).
               (assert-eql 0 (clpm:run-cli '("gc")))

               (assert-true (clpm.store:source-exists-p tree-a) "Expected source A to be kept")
               (assert-true (clpm.store:source-exists-p tree-b) "Expected source B to be kept")
               (assert-true (not (clpm.store:source-exists-p tree-c)) "Expected source C to be removed")
               (assert-true (clpm.store:artifact-exists-p artifact-a) "Expected artifact A to be kept")
               (assert-true (clpm.store:artifact-exists-p artifact-b) "Expected artifact B to be kept")
               (assert-true (not (clpm.store:artifact-exists-p artifact-c)) "Expected artifact C to be removed")
               (assert-true (clpm.store:build-exists-p build-a) "Expected build A to be kept")
               (assert-true (clpm.store:build-exists-p build-b) "Expected build B to be kept")
               (assert-true (not (clpm.store:build-exists-p build-c)) "Expected build C to be removed"))))

      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "  GC roots PASSED~%")
(format t "~%GC roots tests PASSED!~%")
(sb-ext:exit :code 0)
