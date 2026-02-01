;;;; test/cli-test.lisp - CLI behavior tests

(require :asdf)

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

(format t "Testing --help...~%")
(assert-eql 0 (clpm:run-cli '("--help")))
(format t "  --help PASSED~%")

(format t "Testing unknown command...~%")
(assert-eql 1 (clpm:run-cli '("unknown-command")))
(format t "  Unknown command PASSED~%")

(format t "Testing run-program :timeout keyword...~%")
(multiple-value-bind (output error-output exit-code)
    (clpm.platform:run-program (list "sh" "-c" "exit 0") :timeout 1)
  (declare (ignore output error-output))
  (assert-eql 0 exit-code))
(format t "  run-program :timeout PASSED~%")

(format t "~%CLI tests PASSED!~%")
(sb-ext:exit :code 0)
