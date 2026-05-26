;;;; test/sexpr-edit-test.lisp - SexprEdit source lenses.

(require :asdf)
(require :sb-bsd-sockets)

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

(defun assert-equal (expected actual fmt &rest args)
  (unless (equal expected actual)
    (apply #'fail
           (concatenate 'string fmt " Expected ~S, got ~S")
           (append args (list expected actual)))))

(defun lookup (object key)
  (when (and (consp object) (eq (car object) :object))
    (cdr (assoc key (cadr object) :test #'string=))))

(defun array-items (a)
  (when (and (consp a) (eq (car a) :array))
    (cadr a)))

(defun json-object (&rest pairs)
  (list :object
        (loop for (k v) on pairs by #'cddr
              collect (cons k v))))

(defun write-file (path contents)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string contents s)))

(defun read-file-string (path)
  (with-open-file (s path :direction :input :external-format :utf-8)
    (let ((out (make-string-output-stream)))
      (loop for ch = (read-char s nil nil)
            while ch
            do (write-char ch out))
      (get-output-stream-string out))))

(defun source-text ()
  ";;; section header
#| block comment |#
(in-package #:cl-user)

(defun alpha (x)
  (+ x 1))

#;(defun skipped () :bad)

(defun beta ()
  (alpha 4))

(defun alpha (y)
  (- y 1))

#+sbcl
(defparameter *feature-form* :sbcl)
")

(defun make-source-file ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "/tmp/clpm-sexpr-edit-~A/"
                       (random (expt 2 32)))))
         (path (merge-pathnames "sample.lisp" dir)))
    (write-file path (source-text))
    path))

(defun with-daemon (fn)
  (let* ((sock (format nil "/tmp/clpm-sexpr-edit-~A.sock"
                      (random (expt 2 32))))
         (thread (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (clpm.repl:start-server :socket-path sock)
                      (error (c) (format *error-output* "daemon: ~A~%" c))))
                  :name "test-sexpr-edit-daemon")))
    (unwind-protect
         (progn
           (loop for i from 0 below 50
                 while (not (probe-file sock))
                 do (sleep 0.05))
           (assert-true (probe-file sock) "daemon never started")
           (funcall fn sock))
      (handler-case (clpm.repl:send-request sock "shutdown")
        (error () nil))
      (loop for i from 0 below 30
            while (sb-thread:thread-alive-p thread)
            do (sleep 0.05))
      (when (sb-thread:thread-alive-p thread)
        (ignore-errors (sb-thread:terminate-thread thread)))
      (ignore-errors (delete-file sock)))))

(format t "Test: source index preserves exact top-level form text~%")
(let* ((path (make-source-file))
       (document (clpm.sexpr-edit:read-source-document (namestring path)))
       (forms (clpm.sexpr-edit:source-document-forms document)))
  (assert-equal 5 (length forms) "wrong number of indexed forms")
  (assert-true (null (clpm.sexpr-edit:source-document-diagnostics document))
               "unexpected diagnostics: ~S"
               (clpm.sexpr-edit:source-document-diagnostics document))
  (let ((first-defun (second forms)))
    (assert-equal "defun" (clpm.sexpr-edit:source-form-kind first-defun)
                  "wrong form kind")
    (assert-equal "ALPHA" (clpm.sexpr-edit:source-form-name first-defun)
                  "wrong definition name")
    (assert-equal "CL-USER" (clpm.sexpr-edit:source-form-package first-defun)
                  "wrong package context")
    (assert-true (string= "(defun alpha (x)
  (+ x 1))"
                          (clpm.sexpr-edit:source-form-text first-defun))
                 "first defun text was not exact: ~S"
                 (clpm.sexpr-edit:source-form-text first-defun))
    (assert-true (not (search "section header"
                              (clpm.sexpr-edit:source-form-text first-defun)))
                 "leading comment leaked into form text")
    (assert-true (not (search "block comment"
                              (clpm.sexpr-edit:source-form-text first-defun)))
                 "block comment leaked into form text"))
  (let ((matches (clpm.sexpr-edit:find-source-forms document
                                                    :kind "defun"
                                                    :name "alpha")))
    (assert-equal 2 (length matches) "duplicate selector should find both forms")))
(format t "  source index OK~%")

(format t "Test: SexprEdit methods are discoverable and return source lenses~%")
(with-daemon
  (lambda (sock)
    (let* ((path (make-source-file))
           (file (namestring path))
           (methods-resp (clpm.repl:send-request sock "methods"))
           (method-names
             (loop for method in
                   (array-items (lookup (lookup methods-resp "result")
                                        "methods"))
                   collect (lookup method "name"))))
      (assert-true (member "sexpr-list-top-level-forms" method-names
                           :test #'string=)
                   "sexpr-list-top-level-forms missing from methods")
      (assert-true (member "sexpr-show-form" method-names :test #'string=)
                   "sexpr-show-form missing from methods")
      (assert-true (member "sexpr-apply-edit" method-names :test #'string=)
                   "sexpr-apply-edit missing from methods")
      (let* ((list-resp
               (clpm.repl:send-request
                sock "sexpr-list-top-level-forms"
                :params (json-object "file" file)))
             (list-result (lookup list-resp "result"))
             (forms (array-items (lookup list-result "forms"))))
        (assert-true list-result "list method failed: ~S" list-resp)
        (assert-equal 5 (length forms) "wrong RPC form count")
        (assert-equal "in-package" (lookup (first forms) "kind")
                      "wrong first kind")
        (let* ((show-resp
                 (clpm.repl:send-request
                  sock "sexpr-show-form"
                  :params (json-object
                           "path" (json-object "file" file
                                               "top_level" 1))))
               (show-result (lookup show-resp "result"))
               (form (lookup show-result "form")))
          (assert-equal "ok" (lookup show-result "status")
                        "top-level show should be unique")
          (assert-true (search "(+ x 1)" (lookup form "text"))
                       "show did not return selected form: ~S" show-resp))
        (let* ((ambiguous-resp
                 (clpm.repl:send-request
                  sock "sexpr-show-form"
                  :params (json-object
                           "path" (json-object "file" file
                                               "kind" "defun"
                                               "name" "alpha"))))
               (ambiguous-result (lookup ambiguous-resp "result"))
               (candidates (array-items (lookup ambiguous-result
                                                "candidates"))))
          (assert-equal "ambiguous" (lookup ambiguous-result "status")
                        "duplicate selector should be ambiguous")
          (assert-equal 2 (length candidates)
                        "ambiguity should return two candidates"))
        (let* ((before (read-file-string path))
               (bad-resp
                 (clpm.repl:send-request
                  sock "sexpr-apply-edit"
                  :params (json-object
                           "operation" "replace"
                           "path" (json-object "file" file
                                               "top_level" 2)
                           "text" "(defun broken")))
               (bad-error (lookup bad-resp "error")))
          (assert-true bad-error
                       "malformed replacement should fail: ~S" bad-resp)
          (assert-equal before (read-file-string path)
                        "malformed replacement changed the file"))
        (let* ((replace-resp
                 (clpm.repl:send-request
                  sock "sexpr-apply-edit"
                  :params (json-object
                           "operation" "replace"
                           "path" (json-object "file" file
                                               "top_level" 2)
                           "text" "(defun beta () :changed)")))
               (replace-result (lookup replace-resp "result")))
          (assert-true replace-result "valid replacement failed: ~S"
                       replace-resp)
          (assert-equal "replace" (lookup replace-result "operation")
                        "wrong edit operation")
          (assert-true (search "(defun beta () :changed)"
                               (read-file-string path))
                       "replacement text not found in edited file")
          (let* ((show-resp
                   (clpm.repl:send-request
                    sock "sexpr-show-form"
                    :params (json-object
                             "path" (json-object "file" file
                                                 "top_level" 2))))
                 (show-result (lookup show-resp "result"))
                 (form (lookup show-result "form")))
            (assert-equal "ok" (lookup show-result "status")
                          "replaced form should still resolve")
            (assert-true (search ":changed" (lookup form "text"))
                         "replaced form text is stale: ~S" show-resp)))))))
(format t "  RPC source lenses OK~%")

(format t "~%SexprEdit tests PASSED!~%")
(sb-ext:exit :code 0)
