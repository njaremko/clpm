;;;; test/quicklisp-pin-on-load-test.lisp - Quicklisp TOFU pinning on load (no explicit update)

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

(defun write-sexp (path form)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (let ((*print-case* :downcase))
      (prin1 form s)
      (terpri s))))

(format t "Testing Quicklisp TOFU pinning during registry load...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (old-home (sb-posix:getenv "CLPM_HOME")))
    (ensure-directories-exist clpm-home)
    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)
           (let* ((ql-dir (merge-pathnames "quicklisp/" (clpm.platform:registry-dir)))
                  (distinfo (merge-pathnames "distinfo.txt" ql-dir))
                  (systems (merge-pathnames "systems.txt" ql-dir))
                  (releases (merge-pathnames "releases.txt" ql-dir)))
             (ensure-directories-exist ql-dir)

             ;; Pre-populate local Quicklisp metadata so `clone-quicklisp-registry`
             ;; loads without running a network update.
             (write-text distinfo
                         (with-output-to-string (s)
                           (format s "system-index-url: file:///unused~%")
                           (format s "release-index-url: file:///unused~%")))
             (write-text systems "ql-quick ql-quick.asd ql-quick\n")
             (write-text releases "ql-quick file:///unused 0 0 0000000000000000000000000000000000000000 ql-quick-20260201-git ql-quick.asd\n")

             ;; Configure Quicklisp in TOFU mode.
             (write-sexp
              (merge-pathnames "config/config.sxp" clpm-home)
              `(:config :format 1
                :registries ((:quicklisp :url "https://beta.quicklisp.org/dist/quicklisp.txt"
                              :name "quicklisp"
                              :trust "tofu"))
                :defaults nil))

             ;; Loading a registry (via search) should compute the distinfo SHA-256
             ;; and persist a pinned sha256:... trust string to config.
             (assert-true (zerop (clpm:run-cli '("search" "ql-quick")))
                          "Expected search to succeed")
             (let* ((cfg (clpm.config:read-config))
                    (refs (clpm.config:config-registries cfg))
                    (ref (find "quicklisp" refs
                               :key #'clpm.project:registry-ref-name
                               :test #'string=)))
               (assert-true ref "Missing quicklisp registry ref after search")
               (let ((trust (clpm.project:registry-ref-trust ref)))
                 (assert-true (and (stringp trust)
                                   (<= (length "sha256:") (length trust))
                                   (string= "sha256:" trust :end2 (length "sha256:")))
                              "Expected pinned trust string sha256:..., got ~S"
                              trust)))))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME")))))

(format t "  Quicklisp TOFU pin-on-load PASSED~%")
(format t "~%Quicklisp pin-on-load tests PASSED!~%")
(sb-ext:exit :code 0)
