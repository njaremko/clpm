;;;; test/readme-docs-test.lisp - README command surface checks.

(require :asdf)

(defparameter *repo-root*
  (let* ((this-file (or *load-truename* *load-pathname*))
         (test-dir (uiop:pathname-directory-pathname this-file)))
    (uiop:pathname-parent-directory-pathname test-dir)))

(defun fail (fmt &rest args)
  (apply #'format *error-output* (concatenate 'string "FAIL: " fmt "~%") args)
  (sb-ext:exit :code 1))

(defun assert-contains (haystack needle)
  (unless (and (stringp haystack)
               (search needle haystack :test #'char-equal))
    (fail "expected README to contain ~S" needle)))

(defun assert-not-contains (haystack needle)
  (when (and (stringp haystack)
             (search needle haystack :test #'char-equal))
    (fail "README still contains stale command text ~S" needle)))

(defun read-file-string (path)
  (with-open-file (s path :direction :input :external-format :utf-8)
    (let ((text (make-string (file-length s))))
      (read-sequence text s)
      text)))

(let ((readme (read-file-string (merge-pathnames "README.md" *repo-root*))))
  (format t "Testing README REPL command docs...~%")
  (assert-contains readme "call find-definition")
  (assert-contains readme "call xref")
  (assert-not-contains readme "find-definitions")
  (assert-not-contains readme "who-calls")
  (assert-not-contains readme "help --method eval")
  (assert-contains readme "Scoped options must appear before the command token.")
  (format t "  README REPL docs PASSED~%"))

(format t "~%README docs tests PASSED!~%")
(sb-ext:exit :code 0)
