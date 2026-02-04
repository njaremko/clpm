;;;; test/registry-init-test.lisp - `clpm registry init` tests

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

(format t "Testing `clpm registry init`...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (keys-dir (merge-pathnames "keys/" tmp))
         (reg-root (merge-pathnames "reg/" tmp))
         (old-home (sb-posix:getenv "CLPM_HOME"))
         (key-id "testkey")
         ;; RFC8032 vector 1 seed/public key (deterministic).
         (seed-hex "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60")
         (pub-hex "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a")
         (priv-path (merge-pathnames (format nil "~A.key" key-id) keys-dir))
         (pub-path (merge-pathnames (format nil "~A.pub" key-id) keys-dir)))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist keys-dir)
    (ensure-directories-exist reg-root)
    (write-text priv-path seed-hex)
    (write-text pub-path pub-hex)

    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)
           (assert-eql
            0
            (clpm:run-cli (list "registry" "init"
                                "--dir" (namestring reg-root)
                                "--key-id" key-id
                                "--keys-dir" (namestring keys-dir))))

           (let* ((snapshot (merge-pathnames "registry/snapshot.sxp" reg-root))
                  (sig (merge-pathnames "registry/snapshot.sig" reg-root))
                  (embedded (merge-pathnames (format nil "registry/keys/~A.pub" key-id) reg-root))
                  (packages (merge-pathnames "registry/packages/" reg-root)))
             (assert-true (uiop:file-exists-p snapshot) "Missing snapshot: ~A" (namestring snapshot))
             (assert-true (uiop:file-exists-p sig) "Missing signature: ~A" (namestring sig))
             (assert-true (uiop:file-exists-p embedded) "Missing embedded pubkey: ~A" (namestring embedded))
             (assert-true (uiop:directory-exists-p packages) "Missing packages dir: ~A" (namestring packages))

             ;; Verify via existing registry verification path, using embedded key.
             (let ((digest (clpm.registry::verify-snapshot-signature reg-root
                                                                    (format nil "ed25519:~A" key-id))))
               (assert-true (and (stringp digest) (= (length digest) 64))
                            "Expected 64-char signature digest, got ~S" digest))))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "~%Registry init tests PASSED!~%")
(sb-ext:exit :code 0)
