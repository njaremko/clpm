;;;; test/audit-command-test.lisp - `clpm audit` command tests

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

(defun assert-eql (expected actual)
  (unless (eql expected actual)
    (fail "Assertion failed: expected ~S, got ~S" expected actual)))

(defun assert-true (x fmt &rest args)
  (unless x
    (apply #'fail fmt args)))

(defun assert-contains (haystack needle)
  (assert-true (and (stringp haystack)
                    (search needle haystack :test #'char-equal))
               "Expected output to contain ~S, got:~%~A"
               needle
               haystack))

(defun run-cli-captured (args)
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((*standard-output* out)
          (*error-output* err))
      (let ((code (clpm:run-cli args)))
        (values code
                (get-output-stream-string out)
                (get-output-stream-string err))))))

(format t "Testing `clpm audit`...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (proj-root (merge-pathnames "proj/" tmp))
         (manifest (merge-pathnames "clpm.project" proj-root))
         (lock-path (merge-pathnames "clpm.lock" proj-root))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist proj-root)
    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)

           ;; Write a minimal project and lockfile to audit.
           (clpm.project:write-project-file
            (clpm.project:make-project
             :name "proj"
             :version "1.2.3"
             :systems (list "proj")
             :depends '()
             :dev-depends '()
             :test-depends '()
             :registries '())
            manifest)

           (clpm.project:write-lock-file
            (clpm.project:make-lockfile
             :format 1
             :generated-at "2026-02-01T00:00:00Z"
             :project-name "proj"
             :clpm-version "0.1.0"
             :registries
             (list
              (clpm.project:make-locked-registry
               :name "main"
               :kind :git
               :url "file:///tmp/registry.git"
               :commit "deadbeef"
               :trust "ed25519:test"
               :signature "abcdef")
              (clpm.project:make-locked-registry
               :name "quicklisp"
               :kind :quicklisp
               :url "https://beta.quicklisp.org/dist/quicklisp.txt"
               :trust nil))
             :resolved
             (list
              ;; tarball
              (clpm.project:make-locked-system
               :id "lib-t"
               :release (clpm.project:make-locked-release
                         :name "lib-t"
                         :version "1.0.0"
                         :source (clpm.project:make-locked-source
                                  :kind :tarball
                                  :url "https://example.invalid/lib-t.tgz"
                                  :sha256 "00"
                                  :sha1 "11")
                         :artifact-sha256 "00"
                         :tree-sha256 nil)
               :deps '())
              ;; git without commit pin (warn)
              (clpm.project:make-locked-system
               :id "lib-g"
               :release (clpm.project:make-locked-release
                         :name "lib-g"
                         :version "2.0.0"
                         :source (clpm.project:make-locked-source
                                  :kind :git
                                  :url "https://example.invalid/lib-g.git"
                                  :commit nil)
                         :artifact-sha256 nil
                         :tree-sha256 nil)
               :deps '())
              ;; path (warn)
              (clpm.project:make-locked-system
               :id "lib-p"
               :release (clpm.project:make-locked-release
                         :name "lib-p"
                         :version "0.0.0"
                         :source (clpm.project:make-locked-source
                                  :kind :path
                                  :path "../lib-p/"
                                  :sha256 "22")
                         :artifact-sha256 nil
                         :tree-sha256 "22")
               :deps '())))
            lock-path)

           (uiop:with-current-directory (proj-root)
             (multiple-value-bind (code stdout stderr)
                 (run-cli-captured '("audit"))
               (assert-eql 0 code)
               (assert-true (string= "" stderr) "Expected empty stderr, got:~%~A" stderr)
               (assert-contains stdout "Project: proj 1.2.3")
               (assert-contains stdout "Lockfile: generated-at 2026-02-01T00:00:00Z")
               (assert-contains stdout "Registries:")
               (assert-contains stdout
                                (format nil "main~Cgit~Cfile:///tmp/registry.git~Ctrust: ed25519:test~Ccommit: deadbeef~Csnapshot-sig: abcdef~Cverified: yes"
                                        #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab))
               (assert-contains stdout
                                (format nil "quicklisp~Cquicklisp~Chttps://beta.quicklisp.org/dist/quicklisp.txt~Ctrust: -~Ccommit: -~Csnapshot-sig: -~Cverified: -"
                                        #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab))
               (assert-contains stdout "Quicklisp distinfo pins:")
               (assert-contains stdout (format nil "quicklisp~C-" #\Tab))
               (assert-contains stdout "Sources:")
               (assert-contains stdout "tarball: 1")
               (assert-contains stdout "git: 1")
               (assert-contains stdout "path: 1")
               (assert-contains stdout "Warnings:")
               (assert-contains stdout "path dependencies present: lib-p")
               (assert-contains stdout "git dependencies missing commit pin: lib-g")
               (assert-contains stdout "quicklisp trust not configured: quicklisp"))))

           ;; JSON mode is deterministic and includes expected keys.
           (uiop:with-current-directory (proj-root)
             (multiple-value-bind (code stdout stderr)
                 (run-cli-captured '("audit" "--json"))
               (assert-eql 0 code)
               (assert-true (string= "" stderr) "Expected empty stderr, got:~%~A" stderr)
               (assert-contains stdout "\"project\"")
               (assert-contains stdout "\"registries\"")
               (assert-contains stdout "\"warnings\""))))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME"))))

(format t "~%Audit command tests PASSED!~%")
(sb-ext:exit :code 0)
