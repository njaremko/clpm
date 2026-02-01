;;;; test/release-sig-test.lisp - Release signature verification tests

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

(defun read-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((data (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence data s)
      data)))

(defun ed25519-sign (message-bytes seed-bytes)
  "Sign MESSAGE-BYTES using Ed25519 SEED-BYTES (32-byte private key seed).
Returns 64-byte signature vector."
  (unless (= (length seed-bytes) 32)
    (error "Seed must be 32 bytes"))
  (let* ((h (clpm.crypto.sha512:sha512 seed-bytes))
         (a-bytes (subseq h 0 32))
         (prefix (subseq h 32 64)))
    ;; Clamp scalar bytes per RFC 8032.
    (setf (aref a-bytes 0) (logand (aref a-bytes 0) #xf8))
    (setf (aref a-bytes 31) (logand (aref a-bytes 31) #x3f))
    (setf (aref a-bytes 31) (logior (aref a-bytes 31) #x40))
    (let* ((a (clpm.crypto.ed25519::le-bytes-to-int a-bytes))
           (a-point (clpm.crypto.ed25519::scalar-mult a (clpm.crypto.ed25519::basepoint)))
           (a-enc (clpm.crypto.ed25519::encode-point a-point))
           (r-input (make-array (+ 32 (length message-bytes))
                                :element-type '(unsigned-byte 8)))
           (_ (replace r-input prefix :start1 0))
           (_ (replace r-input message-bytes :start1 32))
           (r-digest (clpm.crypto.sha512:sha512 r-input))
           (r (clpm.crypto.ed25519::reduce-scalar
               (clpm.crypto.ed25519::le-bytes-to-int r-digest)))
           (r-point (clpm.crypto.ed25519::scalar-mult r (clpm.crypto.ed25519::basepoint)))
           (r-enc (clpm.crypto.ed25519::encode-point r-point))
           (k-input (make-array (+ 64 (length message-bytes))
                                :element-type '(unsigned-byte 8)))
           (_ (replace k-input r-enc :start1 0))
           (_ (replace k-input a-enc :start1 32))
           (_ (replace k-input message-bytes :start1 64))
           (k-digest (clpm.crypto.sha512:sha512 k-input))
           (k (clpm.crypto.ed25519::reduce-scalar
               (clpm.crypto.ed25519::le-bytes-to-int k-digest)))
           (s (mod (+ r (* k a)) clpm.crypto.ed25519::+l+))
           (s-enc (clpm.crypto.ed25519::int-to-le-bytes s 32))
           (sig (make-array 64 :element-type '(unsigned-byte 8))))
      (declare (ignore _))
      (replace sig r-enc :start1 0)
      (replace sig s-enc :start1 32)
      sig)))

(defun write-registry (root &key (valid-release-sig-p t))
  "Create a minimal registry repo under ROOT.
Returns snapshot signature SHA-256 hex."
  (let* ((seed (clpm.crypto.sha256:hex-to-bytes
                "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"))
         (pub (clpm.crypto.sha256:hex-to-bytes
               "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"))
         (pub-hex (clpm.crypto.sha256:bytes-to-hex pub))
         (empty-sha256 "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
    (ensure-directories-exist (merge-pathnames "registry/keys/" root))
    (ensure-directories-exist (merge-pathnames "registry/packages/pkg/1.0.0/" root))
    ;; Public key embedded in registry.
    (write-text (merge-pathnames "registry/keys/test.pub" root) pub-hex)
    ;; Snapshot + release metadata.
    (clpm.io.sexp:write-canonical-sexp-to-file
     '(:snapshot :format 1 :generated-at "2026-02-01T00:00:00Z"
       :releases ("pkg@1.0.0")
       :provides (("pkg" . "pkg@1.0.0")))
     (merge-pathnames "registry/snapshot.sxp" root))
    (clpm.io.sexp:write-canonical-sexp-to-file
     `(:release :format 1
       :name "pkg"
       :version "1.0.0"
       :source (:tarball :url "https://example.invalid/pkg-1.0.0.tgz"
                :sha256 ,empty-sha256)
       :artifact-sha256 ,empty-sha256
       :systems ("pkg"))
     (merge-pathnames "registry/packages/pkg/1.0.0/release.sxp" root))
    ;; Sign snapshot.
    (let* ((snapshot-path (merge-pathnames "registry/snapshot.sxp" root))
           (snapshot-bytes (read-bytes snapshot-path))
           (snapshot-sig (ed25519-sign snapshot-bytes seed))
           (snapshot-sig-hex (clpm.crypto.sha256:bytes-to-hex snapshot-sig))
           (snapshot-sig-sha256 (clpm.crypto.sha256:bytes-to-hex
                                 (clpm.crypto.sha256:sha256 snapshot-sig))))
      (write-text (merge-pathnames "registry/snapshot.sig" root) snapshot-sig-hex)
      ;; Write release signature (valid or intentionally invalid).
      (if valid-release-sig-p
          (let* ((release-path (merge-pathnames "registry/packages/pkg/1.0.0/release.sxp" root))
                 (release-bytes (read-bytes release-path))
                 (release-sig (ed25519-sign release-bytes seed))
                 (release-sig-hex (clpm.crypto.sha256:bytes-to-hex release-sig)))
            (write-text (merge-pathnames "registry/packages/pkg/1.0.0/release.sig" root)
                        release-sig-hex))
          (write-text (merge-pathnames "registry/packages/pkg/1.0.0/release.sig" root)
                      (make-string 128 :initial-element #\0)))
      snapshot-sig-sha256)))

(format t "Testing release metadata signature verification...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (remote-good (merge-pathnames "remote-registry-good/" tmp))
         (remote-bad (merge-pathnames "remote-registry-bad/" tmp))
         (project-good (merge-pathnames "proj-good/" tmp))
         (project-bad (merge-pathnames "proj-bad/" tmp))
         (old-home (sb-posix:getenv "CLPM_HOME"))
         (git (clpm.platform:find-git)))
    (unless git
      (fail "git not found in PATH"))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist remote-good)
    (ensure-directories-exist remote-bad)
    (ensure-directories-exist project-good)
    (ensure-directories-exist project-bad)

    (unwind-protect
         (progn
           ;; Isolate registry dir under temp CLPM_HOME.
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)

           ;; Create registries.
           (let ((good-snapshot-sig-sha256 (write-registry remote-good :valid-release-sig-p t))
                 (bad-snapshot-sig-sha256 (write-registry remote-bad :valid-release-sig-p nil)))
             (declare (ignore bad-snapshot-sig-sha256))

             ;; Init git repos + commit (so clone-registry can clone them).
             (dolist (repo (list remote-good remote-bad))
               (multiple-value-bind (o e rc)
                   (clpm.platform:run-program (list git "init")
                                              :directory repo
                                              :error-output :string)
                 (declare (ignore o e))
                 (assert-true (zerop rc) "git init failed"))
               (dolist (cfg '(("user.email" "test@example.com")
                              ("user.name" "CLPM Test")))
                 (multiple-value-bind (o e rc)
                     (clpm.platform:run-program (list git "config" (first cfg) (second cfg))
                                                :directory repo
                                                :error-output :string)
                   (declare (ignore o e))
                   (assert-true (zerop rc) "git config failed")))
               (multiple-value-bind (o e rc)
                   (clpm.platform:run-program (list git "add" ".")
                                              :directory repo
                                              :error-output :string)
                 (declare (ignore o e))
                 (assert-true (zerop rc) "git add failed"))
               (multiple-value-bind (o e rc)
                   (clpm.platform:run-program (list git "commit" "-m" "registry")
                                              :directory repo
                                              :error-output :string)
                 (declare (ignore o e))
                 (assert-true (zerop rc) "git commit failed")))

             ;; Project that uses good registry (should succeed without --insecure).
             (let ((url (format nil "file://~A" (namestring remote-good))))
               (clpm.io.sexp:write-canonical-sexp-to-file
                `(:project
                  :name "proj-good"
                  :version "0.1.0"
                  :systems ("proj-good")
                  :depends ()
                  :registries ((:git :url ,url :name "good" :trust "ed25519:test")))
                (merge-pathnames "clpm.project" project-good)))
             (uiop:with-current-directory (project-good)
               (assert-true (zerop (clpm:run-cli '("resolve")))
                            "Expected resolve to succeed with valid release signature"))
             ;; Lockfile should record trust and snapshot signature hash.
             (let* ((lock (clpm.project:read-lock-file (merge-pathnames "clpm.lock" project-good)))
                    (regs (clpm.project:lockfile-registries lock))
                    (reg (find "good" regs
                               :key #'clpm.project:locked-registry-name
                               :test #'string=)))
               (assert-true reg "Expected registry 'good' in lockfile")
               (assert-true (string= (clpm.project:locked-registry-trust reg) "ed25519:test")
                            "Expected lockfile to record trust key")
               (assert-true (string= (clpm.project:locked-registry-signature reg)
                                     good-snapshot-sig-sha256)
                            "Expected lockfile to record snapshot signature sha256"))

             ;; Project that uses bad registry (invalid release.sig).
             (let ((url (format nil "file://~A" (namestring remote-bad))))
               (clpm.io.sexp:write-canonical-sexp-to-file
                `(:project
                  :name "proj-bad"
                  :version "0.1.0"
                  :systems ("proj-bad")
                  :depends ()
                  :registries ((:git :url ,url :name "bad" :trust "ed25519:test")))
                (merge-pathnames "clpm.project" project-bad)))

             ;; Resolve should fail without --insecure.
             (uiop:with-current-directory (project-bad)
               (assert-true (not (zerop (clpm:run-cli '("resolve"))))
                            "Expected resolve to fail with invalid release signature"))

             ;; Resolve should succeed with --insecure.
             (uiop:with-current-directory (project-bad)
               (assert-true (zerop (clpm:run-cli '("--insecure" "resolve")))
                            "Expected resolve to succeed with --insecure")
               (assert-true (uiop:file-exists-p (merge-pathnames "clpm.lock" project-bad))
                            "Expected clpm.lock to be written"))))
      ;; Restore env.
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "  Release signature verification PASSED~%")
(format t "~%Release signature tests PASSED!~%")
(sb-ext:exit :code 0)

