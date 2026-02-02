;;;; test/basic-test.lisp - Basic tests for CLPM

(require :asdf)

;; Add repo root directory to ASDF load path.
;; This script may be run from any working directory.
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

;;; Test SHA256

(format t "Testing SHA256...~%")

(let* ((test-input "hello world")
       (expected "b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9")
       (result (clpm.crypto.sha256:bytes-to-hex
                (clpm.crypto.sha256:sha256 test-input))))
  (if (string= result expected)
      (format t "  SHA256 test PASSED~%")
      (progn
        (format *error-output* "  SHA256 test FAILED~%")
        (format *error-output* "    Expected: ~A~%" expected)
        (format *error-output* "    Got:      ~A~%" result)
        (sb-ext:exit :code 1))))

;;; Test SHA1

(format t "Testing SHA1...~%")

(let* ((test-input "abc")
       (expected "a9993e364706816aba3e25717850c26c9cd0d89d")
       (result (clpm.crypto.sha256:bytes-to-hex
                (clpm.crypto.sha1:sha1 test-input))))
  (if (string= result expected)
      (format t "  SHA1 test PASSED~%")
      (progn
        (format *error-output* "  SHA1 test FAILED~%")
        (format *error-output* "    Expected: ~A~%" expected)
        (format *error-output* "    Got:      ~A~%" result)
        (sb-ext:exit :code 1))))

;; Regression: sha1-files must match sha1(concat) even when file boundaries
;; exactly fill an internal block.
(clpm.store:with-temp-dir (tmp)
  (let* ((a (merge-pathnames "a.bin" tmp))
         (b (merge-pathnames "b.bin" tmp))
         (bytes-a (make-array 10 :element-type '(unsigned-byte 8) :initial-element #x41))
         (bytes-b (make-array 54 :element-type '(unsigned-byte 8) :initial-element #x42))
         (expected (clpm.crypto.sha256:bytes-to-hex
                    (clpm.crypto.sha1:sha1 (concatenate '(vector (unsigned-byte 8))
                                                        bytes-a
                                                        bytes-b)))))
    (with-open-file (s a :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-sequence bytes-a s))
    (with-open-file (s b :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-sequence bytes-b s))
    (let ((result (clpm.crypto.sha256:bytes-to-hex
                   (clpm.crypto.sha1:sha1-files (list a b)))))
      (unless (string= result expected)
        (format *error-output* "  SHA1 sha1-files regression FAILED~%")
        (format *error-output* "    Expected: ~A~%" expected)
        (format *error-output* "    Got:      ~A~%" result)
        (sb-ext:exit :code 1)))))

;;; Test version parsing

(format t "Testing version parsing...~%")

(let ((v (clpm.solver.version:parse-version "1.2.3-alpha+build")))
  (assert (= (clpm.solver.version:version-major v) 1))
  (assert (= (clpm.solver.version:version-minor v) 2))
  (assert (= (clpm.solver.version:version-patch v) 3))
  (assert (string= (clpm.solver.version:version-prerelease v) "alpha"))
  (assert (string= (clpm.solver.version:version-build v) "build"))
  (format t "  Version parsing PASSED~%"))

;;; Test version comparison

(format t "Testing version comparison...~%")

(assert (clpm.solver.version:version< "1.0.0" "2.0.0"))
(assert (clpm.solver.version:version< "1.0.0" "1.1.0"))
(assert (clpm.solver.version:version< "1.0.0" "1.0.1"))
(assert (clpm.solver.version:version< "1.0.0-alpha" "1.0.0"))
(assert (not (clpm.solver.version:version< "2.0.0" "1.0.0")))
(assert (clpm.solver.version:version= "1.0.0" "1.0.0"))
(format t "  Version comparison PASSED~%")

;;; Test constraint parsing

(format t "Testing constraint parsing...~%")

;; Caret constraint
(let ((c (clpm.solver.constraint:parse-constraint '(:semver "^1.2.3"))))
  (assert (clpm.solver.constraint:constraint-satisfies-p c "1.2.3"))
  (assert (clpm.solver.constraint:constraint-satisfies-p c "1.9.9"))
  (assert (not (clpm.solver.constraint:constraint-satisfies-p c "2.0.0")))
  (assert (not (clpm.solver.constraint:constraint-satisfies-p c "1.2.2"))))
(format t "  Caret constraint PASSED~%")

;; Tilde constraint
(let ((c (clpm.solver.constraint:parse-constraint '(:semver "~1.2.3"))))
  (assert (clpm.solver.constraint:constraint-satisfies-p c "1.2.3"))
  (assert (clpm.solver.constraint:constraint-satisfies-p c "1.2.9"))
  (assert (not (clpm.solver.constraint:constraint-satisfies-p c "1.3.0"))))
(format t "  Tilde constraint PASSED~%")

;; Exact constraint
(let ((c (clpm.solver.constraint:parse-constraint '(:exact "1.2.3"))))
  (assert (clpm.solver.constraint:constraint-satisfies-p c "1.2.3"))
  (assert (not (clpm.solver.constraint:constraint-satisfies-p c "1.2.4"))))
(format t "  Exact constraint PASSED~%")

;;; Test safe S-expression reader

(format t "Testing S-expression reader...~%")

(let ((form (clpm.io.sexp:read-safe-sexp-from-string
             "(:project :name \"test\" :version \"1.0\")")))
  (assert (eq (car form) :project))
  (assert (string= (getf (cdr form) :name) "test"))
  (assert (string= (getf (cdr form) :version) "1.0")))
(format t "  S-expression reader PASSED~%")

;;; Test canonical writer

(format t "Testing canonical writer...~%")

(let ((output (clpm.io.sexp:write-canonical-sexp-to-string
               '(:test :key "value" :number 42))))
  (assert (search ":test" output))
  (assert (search ":key" output))
  (assert (search "\"value\"" output))
  (assert (search "42" output)))
(format t "  Canonical writer PASSED~%")

;;; Test platform functions

(format t "Testing platform functions...~%")

(assert (stringp (clpm.platform:platform-triple)))
(assert (stringp (clpm.platform:sbcl-version)))
(assert (stringp (clpm.platform:asdf-version)))
(format t "  Platform functions PASSED~%")
(format t "    Platform: ~A~%" (clpm.platform:platform-triple))
(format t "    SBCL: ~A~%" (clpm.platform:sbcl-version))

;;; Test CLI runner

(format t "Testing CLI runner...~%")

(let ((exit-code (clpm:run-cli '("--version"))))
  (assert (eql exit-code 0)))
(format t "  CLI runner PASSED~%")

;;; Done

(format t "~%All tests PASSED!~%")
(sb-ext:exit :code 0)
