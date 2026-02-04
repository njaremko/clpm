;;;; test/info-command-test.lisp - Info command tests

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

(defun assert-equal (expected actual)
  (unless (equal expected actual)
    (fail "Assertion failed: expected ~S, got ~S" expected actual)))

(defun write-sexp (path form)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (let ((*print-case* :downcase))
      (prin1 form s)
      (terpri s))))

(defun run-cli-captured (args)
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((*standard-output* out)
          (*error-output* err))
      (let ((code (clpm:run-cli args)))
        (values code
                (get-output-stream-string out)
                (get-output-stream-string err))))))

(defun split-lines (s)
  (let ((lines (uiop:split-string s :separator '(#\Newline))))
    (if (and lines (string= (car (last lines)) ""))
        (butlast lines)
        lines)))

(defun index-of (needle haystack)
  (position needle haystack :test #'string=))

(format t "Testing `clpm info`...~%")

(clpm.store:with-temp-dir (tmp)
  (let* ((clpm-home (merge-pathnames "clpm-home/" tmp))
         (old-home (sb-posix:getenv "CLPM_HOME"))
         (empty-sha256 "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
         (reg-root (merge-pathnames "registries/main/" clpm-home)))
    (ensure-directories-exist clpm-home)
    (unwind-protect
         (progn
           (sb-posix:setenv "CLPM_HOME" (namestring clpm-home) 1)

           ;; Local registry files.
           (write-sexp
            (merge-pathnames "registry/snapshot.sxp" reg-root)
            `(:snapshot :format 1 :generated-at "2026-02-01T00:00:00Z"
              :releases ("foo@1.0.0" "foo@2.0.0")
              :provides (("foo" . "foo@1.0.0")
                         ("foo" . "foo@2.0.0"))))

           (dolist (ver '("1.0.0" "2.0.0"))
             (write-sexp
              (merge-pathnames (format nil "registry/packages/foo/~A/release.sxp" ver) reg-root)
              `(:release :format 1
                :name "foo"
                :version ,ver
                :source (:tarball :url ,(format nil "https://example.invalid/foo-~A.tgz" ver)
                         :sha256 ,empty-sha256)
                :artifact-sha256 ,empty-sha256
                :systems ("foo")
                :system-deps ()
                ,@(when (string= ver "2.0.0")
                    (list :license "MIT")))))

           ;; Config: single git registry with no trust (skip signature checks for test).
           (write-sexp
            (merge-pathnames "config/config.sxp" clpm-home)
            '(:config :format 1
              :registries ((:git :url "file:///unused" :name "main" :trust nil))
              :defaults nil))

           (multiple-value-bind (code stdout stderr)
               (run-cli-captured '("info" "foo"))
             (unless (eql code 0)
               (fail "clpm info failed: ~D~%stdout:~A~%stderr:~A" code stdout stderr))
             (let* ((lines (split-lines stdout))
                    (i-sel (index-of "Selected:" lines))
                    (i-cand (index-of "Candidates:" lines)))
               (assert-true (and lines (string= (first lines) "System: foo"))
                            "Expected first line to be `System: foo`, got: ~S"
                            (first lines))
               (assert-true (and (integerp i-sel) (< (1+ i-sel) (length lines)))
                            "Missing Selected section:~%~A" stdout)
               (assert-equal (format nil "  main~Cfoo@2.0.0" #\Tab)
                             (nth (1+ i-sel) lines))
               (assert-true (and (integerp i-cand) (<= (+ i-cand 2) (1- (length lines))))
                            "Missing Candidates section:~%~A" stdout)
               (assert-equal (format nil "  main~Cfoo@2.0.0" #\Tab)
                             (nth (1+ i-cand) lines))
               (assert-equal (format nil "  main~Cfoo@1.0.0" #\Tab)
                             (nth (+ i-cand 2) lines))))))
      (if old-home
          (sb-posix:setenv "CLPM_HOME" old-home 1)
          (sb-posix:unsetenv "CLPM_HOME"))))

(format t "  Info command tests PASSED~%")
(format t "~%Info command tests PASSED!~%")
(sb-ext:exit :code 0)
