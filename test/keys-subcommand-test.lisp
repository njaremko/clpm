;;;; keys-subcommand-test.lisp - `clpm registry key list/import/verify`.

(require :asdf)
(require :sb-posix)

(let* ((this-file (or *load-truename* *load-pathname*))
       (test-dir (uiop:pathname-directory-pathname this-file))
       (repo-root (uiop:pathname-parent-directory-pathname test-dir)))
  (push repo-root asdf:*central-registry*))

(format t "Loading CLPM...~%")
(handler-case
    (asdf:load-system :clpm :verbose nil)
  (error (c)
    (format *error-output* "Failed to load CLPM: ~A~%" c)
    (sb-ext:exit :code 1)))
(format t "CLPM loaded.~%")

(defun fail (fmt &rest args)
  (apply #'format *error-output* (concatenate 'string "FAIL: " fmt "~%") args)
  (sb-ext:exit :code 1))

(defun assert-true (x fmt &rest args)
  (unless x (apply #'fail fmt args)))

(defun assert-eql (expected actual)
  (unless (eql expected actual)
    (fail "expected ~S, got ~S" expected actual)))

(defun assert-contains (haystack needle)
  (assert-true (and (stringp haystack)
                    (search needle haystack :test #'char-equal))
               "expected ~S in:~%~A"
               needle
               haystack))

(defun run-cli-captured (args)
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((*standard-output* out)
          (*error-output* err))
      (let ((code (clpm:run-cli args)))
        (values code (get-output-stream-string out)
                (get-output-stream-string err))))))

(defun assert-duplicate-option (args option)
  (multiple-value-bind (code stdout stderr)
      (run-cli-captured args)
    (declare (ignore stdout))
    (assert-eql 1 code)
    (assert-contains stderr (format nil "Duplicate option: ~A" option))))

(defun read-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((data (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence data s)
      data)))

(defun write-bytes (path bytes)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (write-sequence bytes s)))

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (gen-dir (merge-pathnames "gen/" tmp))
         (import-src (merge-pathnames "import/key2.pub" tmp))
         (msg (merge-pathnames "msg.bin" tmp))
         (sig-path (merge-pathnames "msg.sig" tmp))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (ensure-directories-exist gen-dir)
    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)

           ;; Singleton value options must reject duplicates before touching key files.
           (assert-duplicate-option
            (list "registry" "key" "generate"
                  "--out" (namestring gen-dir)
                  "--out" (namestring (merge-pathnames "other-gen/" tmp))
                  "--id" "duplicate")
            "--out")
           (assert-duplicate-option
            (list "registry" "key" "generate"
                  "--out" (namestring gen-dir)
                  "--id" "duplicate"
                  "--id" "other")
            "--id")
           (assert-duplicate-option
            (list "registry" "key" "list"
                  "--keys-dir" (namestring gen-dir)
                  "--keys-dir" (namestring (merge-pathnames "other-keys/" tmp)))
            "--keys-dir")
           (assert-duplicate-option
            (list "registry" "key" "import"
                  "--pub" (namestring (merge-pathnames "missing-a.pub" tmp))
                  "--pub" (namestring (merge-pathnames "missing-b.pub" tmp))
                  "--id" "duplicate")
            "--pub")
           (assert-duplicate-option
            (list "registry" "key" "import"
                  "--pub" (namestring (merge-pathnames "missing.pub" tmp))
                  "--id" "duplicate"
                  "--id" "other")
            "--id")
           (assert-duplicate-option
            (list "registry" "key" "import"
                  "--pub" (namestring (merge-pathnames "missing.pub" tmp))
                  "--keys-dir" (namestring gen-dir)
                  "--keys-dir" (namestring (merge-pathnames "other-keys/" tmp)))
            "--keys-dir")
           (assert-duplicate-option
            (list "registry" "key" "verify"
                  "--pub" (namestring (merge-pathnames "missing-a.pub" tmp))
                  "--pub" (namestring (merge-pathnames "missing-b.pub" tmp))
                  "--file" (namestring msg)
                  "--sig" (namestring sig-path))
            "--pub")
           (assert-duplicate-option
            (list "registry" "key" "verify"
                  "--pub" (namestring (merge-pathnames "missing.pub" tmp))
                  "--file" (namestring msg)
                  "--file" (namestring (merge-pathnames "other-msg.bin" tmp))
                  "--sig" (namestring sig-path))
            "--file")
           (assert-duplicate-option
            (list "registry" "key" "verify"
                  "--pub" (namestring (merge-pathnames "missing.pub" tmp))
                  "--file" (namestring msg)
                  "--sig" (namestring sig-path)
                  "--sig" (namestring (merge-pathnames "other.sig" tmp)))
            "--sig")

           ;; --- generate a key into a private dir.
           (assert-eql 0 (clpm:run-cli (list "registry" "key" "generate"
                                             "--out" (namestring gen-dir)
                                             "--id" "primary")))

           ;; --- import that pub into the default keys dir.
           (let ((source-pub (merge-pathnames "primary.pub" gen-dir)))
             (assert-eql 0 (clpm:run-cli (list "registry" "key" "import"
                                               "--pub" (namestring source-pub)
                                               "--id" "primary"))))

           ;; --- generate a second key elsewhere and import it under a new id.
           (assert-eql 0 (clpm:run-cli (list "registry" "key" "generate"
                                             "--out" (namestring (uiop:pathname-directory-pathname import-src))
                                             "--id" "key2")))
           (assert-eql 0 (clpm:run-cli (list "registry" "key" "import"
                                             "--pub" (namestring
                                                      (merge-pathnames "key2.pub"
                                                                       (uiop:pathname-directory-pathname import-src)))
                                             "--id" "second")))

           ;; --- list shows both keys with fingerprints.
           (multiple-value-bind (code stdout stderr)
               (run-cli-captured '("registry" "key" "list"))
             (declare (ignore stderr))
             (assert-eql 0 code)
             (assert-true (search "primary" stdout)
                          "expected 'primary' in list output: ~A" stdout)
             (assert-true (search "second" stdout)
                          "expected 'second' in list output: ~A" stdout)
             (assert-true (search "fingerprint:" stdout)
                          "expected fingerprint label: ~A" stdout))

           ;; --- import refuses to overwrite an existing key.
           (let ((source-pub (merge-pathnames "primary.pub" gen-dir)))
             (multiple-value-bind (code _ err)
                 (run-cli-captured (list "registry" "key" "import"
                                         "--pub" (namestring source-pub)
                                         "--id" "primary"))
               (declare (ignore _))
               (assert-true (not (zerop code))
                            "import must refuse to overwrite (rc=~S)" code)
               (assert-true (search "already exists" err)
                            "expected 'already exists' in err: ~A" err)))

           ;; --- verify: round-trip sign/verify using primary's private key.
           (let* ((seed-hex
                    (with-open-file (s (merge-pathnames "primary.key" gen-dir)
                                       :external-format :utf-8)
                      (string-trim '(#\Space #\Tab #\Return #\Newline) (read-line s))))
                  (seed (clpm.crypto.sha256:hex-to-bytes seed-hex))
                  (message (make-array 10 :element-type '(unsigned-byte 8)
                                          :initial-contents '(1 2 3 4 5 6 7 8 9 10))))
             (write-bytes msg message)
             (let* ((sig-bytes (clpm.crypto.ed25519:sign message seed))
                    (sig-hex (clpm.crypto.sha256:bytes-to-hex sig-bytes)))
               (with-open-file (s sig-path :direction :output
                                           :if-exists :supersede
                                           :external-format :utf-8)
                 (write-string sig-hex s)
                 (terpri s)))
             (let ((pub (merge-pathnames "primary.pub" gen-dir)))
               ;; Good signature -> rc 0.
               (assert-eql 0 (clpm:run-cli (list "registry" "key" "verify"
                                                 "--pub" (namestring pub)
                                                 "--file" (namestring msg)
                                                 "--sig" (namestring sig-path))))
               ;; Tamper with the file -> rc != 0.
               (write-bytes msg (make-array 3 :element-type '(unsigned-byte 8)
                                              :initial-contents '(99 99 99)))
               (multiple-value-bind (code _ err)
                   (run-cli-captured (list "registry" "key" "verify"
                                           "--pub" (namestring pub)
                                           "--file" (namestring msg)
                                           "--sig" (namestring sig-path)))
                 (declare (ignore _))
                 (assert-true (not (zerop code))
                              "tampered verify must fail (rc=~S, err=~A)" code err)))))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "~%Keys subcommand tests PASSED!~%")
(sb-ext:exit :code 0)
