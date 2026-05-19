;;;; native-deps-test.lisp - End-to-end test for native-requires plumbing

(require :asdf)
(push (truename "./") asdf:*central-registry*)
(handler-case
    (asdf:load-system :clpm :verbose nil)
  (error (c)
    (format *error-output* "Load failed: ~A~%" c)
    (sb-ext:exit :code 1)))

(format t "Loading CLPM... ok~%")

(defun fail (fmt &rest args)
  (apply #'format *error-output* (concatenate 'string "FAIL: " fmt "~%") args)
  (sb-ext:exit :code 1))

;;; Test 1: native-requires round-trip through lockfile serialization.

(defun test-roundtrip ()
  (format t "Native-requires lockfile round-trip... ")
  (let* ((tmp (uiop:ensure-directory-pathname
               (format nil "/tmp/clpm-native-test-~A/" (random (expt 2 32))))))
    (unwind-protect
         (progn
           (ensure-directories-exist tmp)
           (let* ((lock-path (merge-pathnames "clpm.lock" tmp))
                  (release (clpm.project:make-locked-release
                            :name "demo"
                            :version "0.1.0"
                            :source (clpm.project:make-locked-source
                                     :kind :tarball
                                     :url "https://example.invalid/demo.tar.gz"
                                     :sha256 (make-string 64 :initial-element #\a))
                            :artifact-sha256 (make-string 64 :initial-element #\a)
                            :tree-sha256 (make-string 64 :initial-element #\b)
                            :native-requires '((:pkg-config "libssl")
                                               (:brew "openssl"))))
                  (sys (clpm.project:make-locked-system
                        :id "demo"
                        :release release
                        :deps '()))
                  (lock (clpm.project:make-lockfile
                         :format 1
                         :generated-at "2026-01-01T00:00:00Z"
                         :project-name "x"
                         :clpm-version "0.1.0"
                         :resolved (list sys))))
             (clpm.project:write-lock-file lock lock-path)
             (let* ((back (clpm.project:read-lock-file lock-path))
                    (back-sys (first (clpm.project:lockfile-resolved back)))
                    (back-rel (clpm.project:locked-system-release back-sys))
                    (back-natives (clpm.project:locked-release-native-requires back-rel)))
               (unless (equal back-natives '((:pkg-config "libssl") (:brew "openssl")))
                 (fail "round-trip mismatch: ~S" back-natives)))))
      (ignore-errors (uiop:delete-directory-tree tmp :validate t)))
    (format t "ok~%")))

;;; Test 2: check-native-deps signals missing-native-dep-error for a fake lib.

(defun test-missing-native ()
  (format t "check-native-deps signals on missing... ")
  (let* ((release (clpm.project:make-locked-release
                   :name "demo"
                   :version "0.1.0"
                   :source (clpm.project:make-locked-source :kind :tarball)
                   :artifact-sha256 (make-string 64 :initial-element #\a)
                   :tree-sha256 (make-string 64 :initial-element #\b)
                   :native-requires
                   '((:pkg-config "clpm-nonexistent-library-9999xyz"))))
         (sys (clpm.project:make-locked-system :id "demo" :release release))
         (lock (clpm.project:make-lockfile :resolved (list sys))))
    (handler-case
        (progn
          (clpm.build:check-native-deps lock)
          (fail "expected clpm-missing-native-dep-error"))
      (clpm.errors:clpm-missing-native-dep-error ()
        nil)))
  (format t "ok~%"))

;;; Test 3: check-native-deps signals on malformed entries.

(defun test-malformed-native ()
  (format t "check-native-deps rejects malformed entries... ")
  (let* ((release (clpm.project:make-locked-release
                   :name "demo"
                   :version "0.1.0"
                   :source (clpm.project:make-locked-source :kind :tarball)
                   :artifact-sha256 (make-string 64 :initial-element #\a)
                   :tree-sha256 (make-string 64 :initial-element #\b)
                   :native-requires '("just a string")))
         (sys (clpm.project:make-locked-system :id "demo" :release release))
         (lock (clpm.project:make-lockfile :resolved (list sys))))
    (handler-case
        (progn
          (clpm.build:check-native-deps lock)
          (fail "expected clpm-missing-native-dep-error for malformed entry"))
      (clpm.errors:clpm-missing-native-dep-error ()
        nil)))
  (format t "ok~%"))

;;; Test 4: empty / nil native-requires is a no-op (does not error).

(defun test-empty-native ()
  (format t "check-native-deps no-op with empty list... ")
  (let* ((release (clpm.project:make-locked-release
                   :name "demo"
                   :version "0.1.0"
                   :source (clpm.project:make-locked-source :kind :tarball)
                   :artifact-sha256 (make-string 64 :initial-element #\a)
                   :tree-sha256 (make-string 64 :initial-element #\b)
                   :native-requires nil))
         (sys (clpm.project:make-locked-system :id "demo" :release release))
         (lock (clpm.project:make-lockfile :resolved (list sys))))
    ;; Should NOT signal.
    (clpm.build:check-native-deps lock))
  (format t "ok~%"))

(test-roundtrip)
(test-empty-native)
(test-missing-native)
(test-malformed-native)
(format t "~%Native deps tests PASSED!~%")
