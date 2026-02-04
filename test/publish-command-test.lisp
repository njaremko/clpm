;;;; test/publish-command-test.lisp - `clpm publish` command tests

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
    (write-string text s)
    (terpri s)))

(defun run-cli-captured (args)
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((*standard-output* out)
          (*error-output* err))
      (let ((code (clpm:run-cli args)))
        (values code
                (get-output-stream-string out)
                (get-output-stream-string err))))))

(format t "Testing `clpm publish`...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (keys-dir (merge-pathnames "keys/" tmp))
         (reg-root (merge-pathnames "reg/" tmp))
         (tarballs-dir (merge-pathnames "tarballs/" tmp))
         (old-home (sb-posix:getenv "CLPM_HOME"))
         (key-id "testkey")
         ;; RFC8032 vector 1 seed/public key (deterministic).
         (seed-hex "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60")
         (pub-hex "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a")
         (priv-path (merge-pathnames (format nil "~A.key" key-id) keys-dir))
         (pub-path (merge-pathnames (format nil "~A.pub" key-id) keys-dir))
         (project-name "mylib")
         (project-root (merge-pathnames (format nil "~A/" project-name) tmp)))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist keys-dir)
    (ensure-directories-exist reg-root)
    (ensure-directories-exist tarballs-dir)
    (write-text priv-path seed-hex)
    (write-text pub-path pub-hex)

    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)

           ;; Initialize registry.
           (assert-eql
            0
            (clpm:run-cli (list "registry" "init"
                                "--dir" (namestring reg-root)
                                "--key-id" key-id
                                "--keys-dir" (namestring keys-dir))))

           ;; Create a small local project.
           (assert-eql
            0
            (clpm:run-cli (list "new" project-name "--lib" "--dir" (namestring tmp))))

           ;; Publish into the local registry.
           (multiple-value-bind (code stdout _stderr)
               (run-cli-captured (list "publish"
                                       "--registry" (namestring reg-root)
                                       "--key-id" key-id
                                       "--keys-dir" (namestring keys-dir)
                                       "--project" (namestring project-root)
                                       "--tarball-url" "https://example.invalid/mylib-0.1.0.tar.gz"
                                       "--tarball-out" (namestring tarballs-dir)))
             (declare (ignore _stderr))
             (assert-eql 0 code)
             (assert-true (search "Wrote tarball:" stdout :test #'char-equal)
                          "Expected publish output to mention tarball write, got:~%~A"
                          stdout))

           (let ((tarball (merge-pathnames "mylib-0.1.0.tar.gz" tarballs-dir)))
             (assert-true (uiop:file-exists-p tarball)
                          "Expected tarball to exist: ~A"
                          (namestring tarball)))

           (let* ((release-dir (merge-pathnames "registry/packages/mylib/0.1.0/" reg-root))
                  (release (merge-pathnames "release.sxp" release-dir))
                  (release-sig (merge-pathnames "release.sig" release-dir))
                  (snapshot (merge-pathnames "registry/snapshot.sxp" reg-root))
                  (snapshot-sig (merge-pathnames "registry/snapshot.sig" reg-root)))
             (assert-true (uiop:file-exists-p release) "Missing release: ~A" (namestring release))
             (assert-true (uiop:file-exists-p release-sig) "Missing release signature: ~A" (namestring release-sig))
             (assert-true (uiop:file-exists-p snapshot) "Missing snapshot: ~A" (namestring snapshot))
             (assert-true (uiop:file-exists-p snapshot-sig) "Missing snapshot signature: ~A" (namestring snapshot-sig))

             ;; Snapshot updated.
             (let* ((snap-form (clpm.io.sexp:read-registry-snapshot snapshot))
                    (plist (cdr snap-form))
                    (releases (getf plist :releases))
                    (provides (getf plist :provides)))
               (assert-true (member "mylib@0.1.0" releases :test #'string=)
                            "Expected snapshot to contain release mylib@0.1.0, got ~S" releases)
               (assert-true (member (cons "mylib" "mylib@0.1.0") provides
                                    :test (lambda (a b)
                                            (and (string= (car a) (car b))
                                                 (string= (cdr a) (cdr b)))))
                            "Expected provides to map mylib -> mylib@0.1.0, got ~S" provides))

             ;; Signature verification via registry verifier.
             (let ((trust (format nil "ed25519:~A" key-id)))
               (assert-true (stringp (clpm.registry::verify-snapshot-signature reg-root trust))
                            "Snapshot signature did not verify")
               (assert-true (clpm.registry::verify-release-metadata-signature reg-root "mylib" "0.1.0" trust)
                            "Release signature did not verify"))))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "~%Publish command tests PASSED!~%")
(sb-ext:exit :code 0)
