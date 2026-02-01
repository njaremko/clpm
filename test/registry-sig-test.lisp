;;;; test/registry-sig-test.lisp - Registry snapshot signature enforcement tests

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

(defun write-text (path text)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (write-string text s)))

(format t "Testing registry snapshot signature enforcement...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (remote (merge-pathnames "remote-registry/" tmp))
         (project-root (merge-pathnames "proj/" tmp))
         (old-home (sb-posix:getenv "CLPM_HOME"))
         (git (clpm.platform:find-git)))
    (unless git
      (fail "git not found in PATH"))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist remote)
    (ensure-directories-exist project-root)

    (unwind-protect
         (progn
           ;; Isolate registry dir under temp CLPM_HOME.
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)

           ;; Create a minimal registry repo with an intentionally invalid signature.
           (ensure-directories-exist (merge-pathnames "registry/keys/" remote))
           (clpm.io.sexp:write-canonical-sexp-to-file
            '(:snapshot :format 1 :generated-at "2026-02-01T00:00:00Z"
              :releases () :provides ())
            (merge-pathnames "registry/snapshot.sxp" remote))
           ;; Invalid signature (64 bytes of zero, hex encoded).
           (write-text (merge-pathnames "registry/snapshot.sig" remote)
                       (make-string 128 :initial-element #\0))
           ;; Valid key (RFC 8032 test vector 1 public key), hex encoded.
           (write-text (merge-pathnames "registry/keys/test.pub" remote)
                       "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a")

           ;; Init git repo + commit (so clone-registry can clone it).
           (multiple-value-bind (o e rc)
               (clpm.platform:run-program (list git "init")
                                          :directory remote
                                          :error-output :string)
             (declare (ignore o e))
             (assert-true (zerop rc) "git init failed"))
           (dolist (cfg '(("user.email" "test@example.com")
                          ("user.name" "CLPM Test")))
             (multiple-value-bind (o e rc)
                 (clpm.platform:run-program (list git "config" (first cfg) (second cfg))
                                            :directory remote
                                            :error-output :string)
               (declare (ignore o e))
               (assert-true (zerop rc) "git config failed")))
           (multiple-value-bind (o e rc)
               (clpm.platform:run-program (list git "add" ".")
                                          :directory remote
                                          :error-output :string)
             (declare (ignore o e))
             (assert-true (zerop rc) "git add failed"))
           (multiple-value-bind (o e rc)
               (clpm.platform:run-program (list git "commit" "-m" "snapshot")
                                          :directory remote
                                          :error-output :string)
             (declare (ignore o e))
             (assert-true (zerop rc) "git commit failed"))

           ;; Project that trusts this registry.
           (let ((url (format nil "file://~A" (namestring remote))))
             (clpm.io.sexp:write-canonical-sexp-to-file
              `(:project
                :name "proj"
                :version "0.1.0"
                :systems ("proj")
                :depends ()
                :registries ((:git :url ,url :name "main" :trust "ed25519:test")))
              (merge-pathnames "clpm.project" project-root)))

           ;; Resolve should fail without --insecure.
           (uiop:with-current-directory (project-root)
             (let ((rc (clpm:run-cli '("resolve"))))
               (assert-true (not (zerop rc))
                            "Expected resolve to fail without --insecure")))

           ;; Resolve should succeed with --insecure.
           (uiop:with-current-directory (project-root)
             (let ((rc (clpm:run-cli '("--insecure" "resolve"))))
             (assert-true (zerop rc)
                            "Expected resolve to succeed with --insecure"))
             (assert-true (uiop:file-exists-p (merge-pathnames "clpm.lock" project-root))
                          "Expected clpm.lock to be written")))
      ;; Restore env.
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "  Registry snapshot signature enforcement PASSED~%")
(format t "~%Registry signature tests PASSED!~%")
(sb-ext:exit :code 0)
