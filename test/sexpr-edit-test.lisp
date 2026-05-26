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

(defun string-suffix-p (suffix string)
  (let ((suffix-length (length suffix))
        (string-length (length string)))
    (and (<= suffix-length string-length)
         (string= suffix string
                  :start2 (- string-length suffix-length)))))

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

(defun count-substrings (needle haystack)
  (let ((count 0)
        (start 0))
    (loop for position = (search needle haystack :start2 start)
          while position
          do (incf count)
             (setf start (+ position (length needle))))
    count))

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

(defun make-diff-source-file ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "/tmp/clpm-sexpr-edit-diff-~A/"
                       (random (expt 2 32)))))
         (path (merge-pathnames "diff.lisp" dir)))
    (write-file path "(in-package #:cl-user)

(foo x)
")
    path))

(defun make-validation-source-file (contents)
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "/tmp/clpm-sexpr-edit-validate-~A/"
                       (random (expt 2 32)))))
         (path (merge-pathnames "validate.lisp" dir)))
    (write-file path contents)
    path))

(defun make-macro-source-file ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "/tmp/clpm-sexpr-edit-macro-~A/"
                       (random (expt 2 32)))))
         (path (merge-pathnames "macro.lisp" dir)))
    (write-file path "(in-package #:cl-user)
(when t (list :ok))
")
    path))

(defun make-scope-source-file ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "/tmp/clpm-sexpr-edit-scope-~A/"
                       (random (expt 2 32)))))
         (path (merge-pathnames "scope.lisp" dir)))
    (write-file path "(in-package #:cl-user)
(defun scoped (cell env)
  (let ((value (car cell)))
    (symbol-macrolet ((current (cdr cell)))
      (flet ((resolve () value))
        (list current (resolve) env)))))
")
    path))

(defun make-defpackage-source-file ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "/tmp/clpm-sexpr-edit-package-~A/"
                       (random (expt 2 32)))))
         (path (merge-pathnames "packages.lisp" dir)))
    (write-file path "(in-package #:cl-user)

(defpackage #:demo.engine
  (:use #:cl)
  (:export #:evaluate-workbook))
")
    path))

(defun make-package-diagnostic-source-file (package-name)
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "/tmp/clpm-sexpr-edit-package-diag-~A/"
                       (random (expt 2 32)))))
         (path (merge-pathnames "package-diag.lisp" dir)))
    (write-file path
                (format nil "(in-package #:cl-user)

(defun uses-internal-symbol ()
  ~A::hidden)
"
                        (string-downcase package-name)))
    path))

(defun make-call-graph-source-file ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "/tmp/clpm-sexpr-edit-call-graph-~A/"
                       (random (expt 2 32)))))
         (path (merge-pathnames "call-graph.lisp" dir)))
    (write-file path "(in-package #:cl-user)

(defgeneric graph-evaluate (thing env))

(defun graph-direct (x)
  (graph-helper x))

(defun graph-possible (thing env)
  (graph-evaluate thing env))

(defun graph-dynamic (evaluator thing env)
  (funcall evaluator thing env))
")
    path))

(defun make-add-method-source-file ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "/tmp/clpm-sexpr-edit-add-method-~A/"
                       (random (expt 2 32)))))
         (path (merge-pathnames "methods.lisp" dir)))
    (write-file path "(in-package #:cl-user)

(defgeneric add-demo (thing env))
")
    path))

(defun make-sync-source-file (function-name value)
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "/tmp/clpm-sexpr-edit-sync-~A/"
                       (random (expt 2 32)))))
         (path (merge-pathnames "sync.lisp" dir)))
    (write-file path
                (format nil "(in-package #:cl-user)

(defun ~A ()
  ~S)
"
                        (string-downcase function-name)
                        value))
    path))

(defun make-lint-source-file ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "/tmp/clpm-sexpr-edit-lint-~A/"
                       (random (expt 2 32)))))
         (path (merge-pathnames "lint.lisp" dir)))
    (write-file path "(in-package #:cl-user)

(defun lint-demo (x)
  (setf (car '(1 2)) 3)
  (list (eq x 1)
        (eval '(+ 1 2))))
")
    path))

(defun make-macro-shape-source-file ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "/tmp/clpm-sexpr-edit-macro-shape-~A/"
                       (random (expt 2 32)))))
         (path (merge-pathnames "macro-shape.lisp" dir)))
    (write-file path "(in-package #:cl-user)

(defmacro with-foo ((foo arg) &body body)
  `(let ((,foo ,arg)) ,@body))

(defmacro define-thing (name args &body body)
  `(defun ,name ,args ,@body))

(defmacro odd-macro (x)
  x)
")
    path))

(defun make-macro-contract-source-file ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "/tmp/clpm-sexpr-edit-contract-~A/"
                       (random (expt 2 32)))))
         (path (merge-pathnames "contracts.lisp" dir)))
    (write-file path "(in-package #:cl-user)

(define-editing-contract with-bound
  :kind :binding-macro
  :lambda-list ((resource value) &body body)
  :introduced-bindings ((resource :lexical))
  :body-position :body
  :safe-to-wrap t)

(defmacro with-bound ((resource value) &body body)
  `(let ((,resource ,value))
     ,@body))

(defun contract-scope (input)
  (with-bound (resource input)
    resource))

(define-editing-contract define-widget
  :kind :definition
  :name-position 0
  :lambda-list-position 1
  :body-position :body)

(define-widget sample-widget (x)
  x)

(define-editing-contract malformed-contract
  :kind :binding-macro
  :introduced-bindings (not-a-binding))
")
    path))

(defun make-effect-source-file ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "/tmp/clpm-sexpr-edit-effect-~A/"
                       (random (expt 2 32)))))
         (path (merge-pathnames "effect.lisp" dir)))
    (write-file path "(in-package #:cl-user)

(defun effect-demo (x z)
  (setf (slot-value x 'y) z)
  (unknown-effect x)
  42
  x)
")
    path))

(defun make-rewrite-source-file ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "/tmp/clpm-sexpr-edit-rewrite-~A/"
                       (random (expt 2 32)))))
         (path (merge-pathnames "rewrite.lisp" dir)))
    (write-file path "(in-package #:cl-user)

(defun rewrite-progn (x)
  (progn (list x) x))

(defun rewrite-inline (y)
  (let ((x (unknown-effect y)))
    (+ x x)))

(defun rewrite-special (y)
  (let ((x y))
    (declare (special x))
    x))
")
    path))

(defun make-introduce-source-file ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "/tmp/clpm-sexpr-edit-introduce-~A/"
                       (random (expt 2 32)))))
         (path (merge-pathnames "introduce.lisp" dir)))
    (write-file path "(in-package #:cl-user)

(defun introduce-demo (cell)
  (+ (cell-value cell) 1))
")
    path))

(defun make-bind-repeated-source-file ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "/tmp/clpm-sexpr-edit-bind-repeated-~A/"
                       (random (expt 2 32)))))
         (path (merge-pathnames "bind-repeated.lisp" dir)))
    (write-file path "(in-package #:cl-user)

(defun repeated-demo (cell)
  (+ (car cell) (car cell)))

(defun repeated-effect (cell)
  (+ (unknown-effect cell) (unknown-effect cell)))
")
    path))

(defun make-extract-source-file ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "/tmp/clpm-sexpr-edit-extract-~A/"
                       (random (expt 2 32)))))
         (path (merge-pathnames "extract.lisp" dir)))
    (write-file path "(in-package #:cl-user)

(defun extract-demo (x y)
  (+ (* x y) 1))
")
    path))

(defun make-lambda-refactor-source-file ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "/tmp/clpm-sexpr-edit-lambda-~A/"
                       (random (expt 2 32)))))
         (path (merge-pathnames "lambda.lisp" dir)))
    (write-file path "(in-package #:cl-user)

(defun optional-target (x)
  x)

(defun optional-caller ()
  (optional-target 1))

(defun api-target (cell formula env)
  (list cell formula env))

(defun api-caller (cell formula env)
  (api-target cell formula env))

(defun api-dynamic (args)
  (apply #'api-target args))
")
    path))

(defun make-structural-move-source-file ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "/tmp/clpm-sexpr-edit-structural-~A/"
                       (random (expt 2 32)))))
         (path (merge-pathnames "structural.lisp" dir)))
    (write-file path "(in-package #:cl-user)

(defun splice-demo ()
  (progn
    (alpha)
    (beta))
  (gamma))

(defun raise-demo (x)
  (outer (inner x) :done))

(defun transpose-demo ()
  (first)
  (second)
  (third))
")
    path))

(defun make-repair-source-file ()
  (let* ((dir (uiop:ensure-directory-pathname
               (format nil "/tmp/clpm-sexpr-edit-repair-~A/"
                       (random (expt 2 32)))))
         (path (merge-pathnames "repair.lisp" dir)))
    (write-file path "(in-package #:cl-user)

(defun repair-known-helper (cell)
  cell)

(defun repair-unbound (cell env)
  (list store cell env))

(defun repair-undefined (cell)
  (repair-known-helpr cell))
")
    path))

(defun entry-names (array)
  (loop for entry in (array-items array)
        collect (lookup entry "name")))

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
      (assert-true (member "sexpr-search-forms" method-names :test #'string=)
                   "sexpr-search-forms missing from methods")
      (assert-true (member "sexpr-plan-edit" method-names :test #'string=)
                   "sexpr-plan-edit missing from methods")
      (assert-true (member "sexpr-apply-edit" method-names :test #'string=)
                   "sexpr-apply-edit missing from methods")
      (assert-true (member "sexpr-update-defpackage" method-names
                           :test #'string=)
                   "sexpr-update-defpackage missing from methods")
      (assert-true (member "sexpr-add-method" method-names :test #'string=)
                   "sexpr-add-method missing from methods")
      (assert-true (member "sexpr-package-diagnostics" method-names
                           :test #'string=)
                   "sexpr-package-diagnostics missing from methods")
      (assert-true (member "sexpr-macroexpand-at" method-names :test #'string=)
                   "sexpr-macroexpand-at missing from methods")
      (assert-true (member "sexpr-expansion-of" method-names :test #'string=)
                   "sexpr-expansion-of missing from methods")
      (assert-true (member "sexpr-source-origin" method-names :test #'string=)
                   "sexpr-source-origin missing from methods")
      (assert-true (member "sexpr-call-graph" method-names :test #'string=)
                   "sexpr-call-graph missing from methods")
      (assert-true (member "sexpr-compare-image-source" method-names
                           :test #'string=)
                   "sexpr-compare-image-source missing from methods")
      (assert-true (member "sexpr-lint" method-names :test #'string=)
                   "sexpr-lint missing from methods")
      (assert-true (member "sexpr-macro-shape" method-names :test #'string=)
                   "sexpr-macro-shape missing from methods")
      (assert-true (member "sexpr-macro-contracts" method-names
                            :test #'string=)
                   "sexpr-macro-contracts missing from methods")
      (assert-true (member "sexpr-effect-summary" method-names :test #'string=)
                   "sexpr-effect-summary missing from methods")
      (assert-true (member "sexpr-classify-rewrite" method-names
                           :test #'string=)
                   "sexpr-classify-rewrite missing from methods")
      (assert-true (member "sexpr-introduce-let" method-names :test #'string=)
                   "sexpr-introduce-let missing from methods")
      (assert-true (member "sexpr-extract-function" method-names
                            :test #'string=)
                   "sexpr-extract-function missing from methods")
      (assert-true (member "sexpr-suggest-edit-candidates" method-names
                            :test #'string=)
                   "sexpr-suggest-edit-candidates missing from methods")
      (assert-true (member "sexpr-change-lambda-list" method-names
                            :test #'string=)
                   "sexpr-change-lambda-list missing from methods")
      (assert-true (member "sexpr-add-keyword-arg" method-names
                            :test #'string=)
                   "sexpr-add-keyword-arg missing from methods")
      (assert-true (member "sexpr-convert-to-keyword-argument" method-names
                            :test #'string=)
                   "sexpr-convert-to-keyword-argument missing from methods")
      (assert-true (member "sexpr-splice-form" method-names :test #'string=)
                   "sexpr-splice-form missing from methods")
      (assert-true (member "sexpr-raise-form" method-names :test #'string=)
                   "sexpr-raise-form missing from methods")
      (assert-true (member "sexpr-transpose-forms" method-names
                            :test #'string=)
                   "sexpr-transpose-forms missing from methods")
      (assert-true (member "sexpr-kill-form" method-names :test #'string=)
                   "sexpr-kill-form missing from methods")
      (assert-true (member "sexpr-copy-form" method-names :test #'string=)
                   "sexpr-copy-form missing from methods")
      (assert-true (member "sexpr-move-form" method-names :test #'string=)
                   "sexpr-move-form missing from methods")
      (assert-true (member "sexpr-slurp-forward" method-names :test #'string=)
                   "sexpr-slurp-forward missing from methods")
      (assert-true (member "sexpr-barf-forward" method-names :test #'string=)
                   "sexpr-barf-forward missing from methods")
      (assert-true (member "sexpr-bind-repeated-expression" method-names
                            :test #'string=)
                   "sexpr-bind-repeated-expression missing from methods")
      (assert-true (member "sexpr-bindings-at" method-names :test #'string=)
                   "sexpr-bindings-at missing from methods")
      (assert-true (member "sexpr-symbol-info" method-names :test #'string=)
                   "sexpr-symbol-info missing from methods")
      (assert-true (member "sexpr-class-info" method-names :test #'string=)
                   "sexpr-class-info missing from methods")
      (assert-true (member "sexpr-generic-info" method-names :test #'string=)
                   "sexpr-generic-info missing from methods")
      (assert-true (member "sexpr-system-graph" method-names :test #'string=)
                   "sexpr-system-graph missing from methods")
      (assert-true (member "sexpr-affected-files" method-names :test #'string=)
                   "sexpr-affected-files missing from methods")
      (assert-true (member "sexpr-validate-edit" method-names :test #'string=)
                   "sexpr-validate-edit missing from methods")
      (assert-true (member "sexpr-generate-test" method-names
                            :test #'string=)
                   "sexpr-generate-test missing from methods")
      (assert-true (member "sexpr-repair-suggestions" method-names
                            :test #'string=)
                   "sexpr-repair-suggestions missing from methods")
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
        (let* ((search-resp
                 (clpm.repl:send-request
                  sock "sexpr-search-forms"
                  :params (json-object
                           "file" file
                           "pattern" "(alpha ?value)")))
               (search-result (lookup search-resp "result"))
               (matches (array-items (lookup search-result "matches")))
               (match (first matches))
               (bindings (array-items (lookup match "bindings")))
               (binding (first bindings)))
          (assert-true search-result "search failed: ~S" search-resp)
          (assert-equal 1 (lookup search-result "match_count")
                        "pattern should match one call")
          (assert-equal "4" (lookup binding "value")
                        "single-form variable binding should be printed")
          (assert-equal "?VALUE" (lookup binding "name")
                        "binding name should preserve pattern variable"))
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
               (plan-resp
                 (clpm.repl:send-request
                  sock "sexpr-plan-edit"
                  :params (json-object
                           "operation" "replace"
                           "path" (json-object "file" file
                                               "top_level" 2)
                           "text" "(defun beta () :planned)")))
               (plan-result (lookup plan-resp "result"))
               (plan-diff (array-items (lookup plan-result
                                                "structural_diff")))
               (plan-provenance (lookup plan-result "provenance")))
          (assert-true plan-result "plan edit failed: ~S" plan-resp)
          (assert-true (lookup plan-result "dry_run")
                       "plan result should be marked dry-run")
          (assert-true plan-diff
                       "plan result should include structural diff")
          (assert-true plan-provenance
                       "plan result should include provenance")
          (assert-equal "sexpr-edit"
                        (lookup plan-provenance "created_by")
                        "wrong provenance creator")
          (assert-equal :false
                        (lookup plan-provenance "source_comments_inserted")
                        "provenance should not be written as comments")
          (assert-true (array-items
                        (lookup plan-provenance "changed_forms"))
                       "provenance should carry changed form identities")
          (assert-equal before (read-file-string path)
                        "dry-run plan changed the file"))
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
                         "replaced form text is stale: ~S" show-resp)))
        (let* ((diff-path (make-diff-source-file))
               (diff-file (namestring diff-path))
               (call-resp
                 (clpm.repl:send-request
                  sock "sexpr-apply-edit"
                  :params (json-object
                           "operation" "replace"
                           "path" (json-object "file" diff-file
                                               "top_level" 1)
                           "text" "(foo x y)")))
               (call-result (lookup call-resp "result"))
               (call-changes (array-items (lookup call-result
                                                  "structural_diff")))
               (changed-call (find "changed_call" call-changes
                                   :key (lambda (change)
                                          (lookup change "kind"))
                                   :test #'string=))
               (added-args (and changed-call
                                (array-items
                                 (lookup changed-call
                                         "added_arguments")))))
          (assert-true call-result "changed call edit failed: ~S"
                       call-resp)
          (assert-true changed-call
                       "missing changed_call structural diff: ~S"
                       call-result)
          (assert-equal "foo" (lookup changed-call "operator")
                        "wrong changed call operator")
          (assert-equal '("y") added-args
                        "wrong added call arguments"))
        (let* ((diff-path (make-diff-source-file))
               (diff-file (namestring diff-path))
               (insert-resp
                 (clpm.repl:send-request
                  sock "sexpr-apply-edit"
                  :params (json-object
                           "operation" "insert-after"
                           "path" (json-object "file" diff-file
                                               "top_level" 1)
                           "text" "(defun created () :ok)")))
               (insert-result (lookup insert-resp "result"))
               (insert-changes (array-items (lookup insert-result
                                                    "structural_diff")))
               (inserted (find "inserted_top_level" insert-changes
                               :key (lambda (change)
                                      (lookup change "kind"))
                               :test #'string=))
               (after-forms (and inserted
                                 (array-items
                                  (lookup inserted "after_forms"))))
               (inserted-form (first after-forms)))
          (assert-true insert-result "insert edit failed: ~S"
                       insert-resp)
          (assert-true inserted
                       "missing inserted_top_level structural diff: ~S"
                       insert-result)
          (assert-equal "defun" (lookup inserted-form "kind")
                        "inserted form kind should be defun")
          (assert-equal "CREATED" (lookup inserted-form "name")
                        "inserted form name should be recorded"))
        (let* ((package-path (make-defpackage-source-file))
               (package-file (namestring package-path))
               (before (read-file-string package-path))
               (plan-resp
                 (clpm.repl:send-request
                  sock "sexpr-update-defpackage"
                  :params (json-object
                           "file" package-file
                           "package" "demo.engine"
                           "operation" "export"
                           "symbol" "evaluate-cell"
                           "dry_run" t)))
               (plan-result (lookup plan-resp "result")))
          (assert-true plan-result "defpackage dry-run failed: ~S"
                       plan-resp)
          (assert-true (lookup plan-result "dry_run")
                       "defpackage plan should be marked dry-run")
          (assert-true (lookup plan-result "provenance")
                       "defpackage plan should include provenance")
          (assert-true (lookup plan-result "changed")
                       "new export should be a change")
          (assert-true (search "#:evaluate-cell"
                               (lookup plan-result "after_text"))
                       "planned export was not inserted: ~S"
                       plan-result)
          (assert-equal before (read-file-string package-path)
                        "defpackage dry-run changed the file")
          (let* ((apply-resp
                   (clpm.repl:send-request
                    sock "sexpr-update-defpackage"
                    :params (json-object
                             "file" package-file
                             "package" "demo.engine"
                             "operation" "export"
                             "symbol" "evaluate-cell")))
                 (apply-result (lookup apply-resp "result"))
                 (after-apply (read-file-string package-path)))
            (assert-true apply-result "defpackage apply failed: ~S"
                         apply-resp)
            (assert-true (lookup apply-result "changed")
                         "defpackage apply should report changed")
            (assert-true (search "#:evaluate-workbook
           #:evaluate-cell)"
                                 after-apply)
                         "export insertion should preserve clause layout: ~S"
                         after-apply)
            (let* ((duplicate-resp
                     (clpm.repl:send-request
                      sock "sexpr-update-defpackage"
                      :params (json-object
                               "file" package-file
                               "package" "demo.engine"
                               "operation" "export"
                               "symbol" "evaluate-cell")))
                   (duplicate-result (lookup duplicate-resp "result")))
              (assert-true duplicate-result
                           "duplicate export failed: ~S" duplicate-resp)
              (assert-true (lookup duplicate-result "duplicate")
                           "duplicate export should be detected")
              (assert-true (not (lookup duplicate-result "changed"))
                           "duplicate export should not change file")
              (assert-equal 1
                            (count-substrings "#:evaluate-cell"
                                              (read-file-string
                                               package-path))
                            "duplicate export was inserted twice"))))
        (let* ((suffix (random (expt 2 32)))
               (conflict-a (format nil "SEXPR-CONFLICT-A-~A" suffix))
               (conflict-b (format nil "SEXPR-CONFLICT-B-~A" suffix))
               (setup-resp
                 (clpm.repl:send-request
                  sock "eval"
                  :params
                  (json-object
                   "form"
                   (format nil "(let ((a (make-package ~S :use nil))
                                      (b (make-package ~S :use nil)))
                                  (export (intern ~S a) a)
                                  (export (intern ~S b) b)
                                  :ok)"
                           conflict-a conflict-b "COLLIDE" "COLLIDE"))))
               (diag-resp
                 (clpm.repl:send-request
                  sock "sexpr-package-diagnostics"
                  :params
                  (json-object
                   "use_packages" (list :array
                                        (list conflict-a conflict-b)))))
               (diag-result (lookup diag-resp "result"))
               (diagnostics (array-items (lookup diag-result "diagnostics")))
               (conflict (find "package_conflict" diagnostics
                               :key (lambda (diagnostic)
                                      (lookup diagnostic "kind"))
                               :test #'string=))
               (packages (and conflict
                              (array-items (lookup conflict "packages")))))
          (assert-true (lookup setup-resp "result")
                       "package conflict setup failed: ~S" setup-resp)
          (assert-true diag-result
                       "package diagnostics failed: ~S" diag-resp)
          (assert-true conflict
                       "missing package conflict diagnostic: ~S"
                       diag-result)
          (assert-equal "COLLIDE" (lookup conflict "symbol")
                        "wrong conflict symbol")
          (assert-true (member conflict-a packages :test #'string=)
                       "conflict missing first package: ~S" conflict)
          (assert-true (member conflict-b packages :test #'string=)
                       "conflict missing second package: ~S" conflict))
        (let* ((internal-package
                 (format nil "SEXPR-INTERNAL-SOURCE-~A"
                         (random (expt 2 32))))
               (setup-resp
                 (clpm.repl:send-request
                  sock "eval"
                  :params (json-object
                           "form"
                           (format nil "(make-package ~S :use nil)"
                                   internal-package))))
               (diag-path
                 (make-package-diagnostic-source-file internal-package))
               (diag-resp
                 (clpm.repl:send-request
                  sock "sexpr-package-diagnostics"
                  :params (json-object "file" (namestring diag-path))))
               (diag-result (lookup diag-resp "result"))
               (diagnostics (array-items (lookup diag-result "diagnostics")))
               (internal (find "internal_symbol_reference" diagnostics
                               :key (lambda (diagnostic)
                                      (lookup diagnostic "kind"))
                               :test #'string=)))
          (assert-true (lookup setup-resp "result")
                       "internal package setup failed: ~S" setup-resp)
          (assert-true diag-result
                       "source package diagnostics failed: ~S"
                       diag-resp)
          (assert-true internal
                       "missing internal symbol diagnostic: ~S"
                       diag-result)
          (assert-equal "HIDDEN" (lookup internal "symbol")
                        "wrong internal symbol")
          (assert-equal internal-package (lookup internal "package")
                        "wrong internal symbol package"))
        (let* ((bad-path (make-validation-source-file
                          "(defun broken ()"))
               (bad-resp
                 (clpm.repl:send-request
                  sock "sexpr-validate-edit"
                  :params (json-object
                           "file" (namestring bad-path)
                           "steps" (list :array
                                         (list "read" "compile")))))
               (bad-result (lookup bad-resp "result"))
               (bad-steps (array-items (lookup bad-result "steps")))
               (bad-provenance (lookup bad-result "provenance"))
               (bad-read (first bad-steps)))
          (assert-true bad-result "read validation failed to return: ~S"
                       bad-resp)
          (assert-true bad-provenance
                       "validation should include provenance")
          (assert-equal '("read" "compile-file")
                        (array-items
                         (lookup bad-provenance "validation_steps"))
                        "validation provenance should preserve requested steps")
          (assert-equal :false (lookup bad-result "success")
                        "malformed file should fail validation")
          (assert-equal "read" (lookup bad-read "name")
                        "first step should be read")
          (assert-equal :false (lookup bad-read "success")
                        "malformed file should fail read step")
          (assert-equal 1 (length bad-steps)
                        "validation should stop after read failure"))
        (let* ((ok-path (make-validation-source-file
                         "(in-package #:cl-user)

(defun sexpr-validate-ok () 42)
"))
               (ok-resp
                 (clpm.repl:send-request
                  sock "sexpr-validate-edit"
                  :params (json-object
                           "file" (namestring ok-path)
                           "steps" (list :array
                                         (list "read" "compile-file")))))
               (ok-result (lookup ok-resp "result"))
               (ok-steps (array-items (lookup ok-result "steps"))))
          (unwind-protect
               (progn
                 (assert-true ok-result
                              "valid validation failed: ~S" ok-resp)
                 (assert-true (lookup ok-result "success")
                              "valid validation should pass: ~S"
                              ok-result)
                 (assert-equal '("read" "compile-file")
                               (mapcar (lambda (step)
                                         (lookup step "name"))
                                       ok-steps)
                               "wrong validation step transcript"))
            (ignore-errors
             (let ((fasl (make-pathname :type "fasl"
                                        :defaults ok-path)))
               (when (probe-file fasl) (delete-file fasl))))))
        (let* ((dir (uiop:ensure-directory-pathname
                     (format nil "/tmp/clpm-sexpr-edit-generated-test-~A/"
                             (random (expt 2 32)))))
               (example-output (merge-pathnames "example-test.lisp" dir))
               (example-resp
                 (clpm.repl:send-request
                  sock "sexpr-generate-test"
                  :params (json-object
                           "form" "(+ 1 2)"
                           "output_file" (namestring example-output)
                           "test_name" "captured arithmetic")))
               (example-result (lookup example-resp "result"))
               (example-validation
                 (lookup example-result "validation"))
               (definition-source
                 (make-validation-source-file
                  "(in-package #:cl-user)

(defun generated-target () 42)
"))
               (definition-output
                 (merge-pathnames "definition-test.lisp" dir))
               (definition-resp
                 (clpm.repl:send-request
                  sock "sexpr-generate-test"
                  :params (json-object
                           "path" (json-object
                                   "file" (namestring definition-source)
                                   "kind" "defun"
                                   "name" "generated-target")
                           "output_file" (namestring definition-output)
                           "test_name" "generated definition")))
               (definition-result (lookup definition-resp "result"))
               (definition-validation
                 (lookup definition-result "validation")))
          (assert-true example-result
                       "REPL example test generation failed: ~S"
                       example-resp)
          (assert-equal "lightweight_sbcl_script"
                        (lookup example-result "style")
                        "generated test should use SBCL script style")
          (assert-true (probe-file example-output)
                       "REPL example test file was not written")
          (assert-true (lookup example-validation "success")
                       "generated REPL example test should validate: ~S"
                       example-validation)
          (assert-true (search "(multiple-value-list (+ 1 2))"
                               (read-file-string example-output))
                       "generated example should capture action form")
          (assert-true definition-result
                       "definition test generation failed: ~S"
                       definition-resp)
          (assert-true (probe-file definition-output)
                       "definition test file was not written")
          (assert-true (lookup definition-validation "success")
                       "generated definition test should validate: ~S"
                       definition-validation)
          (assert-true (search "(fboundp symbol)"
                               (read-file-string definition-output))
                       "definition smoke test should assert fboundp"))
        (let* ((repair-path (make-repair-source-file))
               (repair-file (namestring repair-path))
               (repair-resp
                 (clpm.repl:send-request
                  sock "sexpr-repair-suggestions"
                  :params (json-object
                           "file" repair-file
                           "path" (json-object
                                   "file" repair-file
                                   "kind" "defun"
                                   "name" "repair-unbound"
                                   "child_path"
                                   (list :array (list 3))))))
               (repair-result (lookup repair-resp "result"))
               (conditions
                 (array-items (lookup repair-result "conditions")))
               (suggestions
                 (array-items (lookup repair-result "suggestions"))))
          (unwind-protect
               (progn
                 (assert-true repair-result
                              "repair suggestions failed: ~S" repair-resp)
                 (let* ((unbound
                          (find "unbound_variable" conditions
                                :key (lambda (condition)
                                       (lookup condition "classification"))
                                :test #'string=))
                        (undefined
                          (find "undefined_function" conditions
                                :key (lambda (condition)
                                       (lookup condition "classification"))
                                :test #'string=))
                        (visible
                          (and unbound
                               (array-items
                                (lookup unbound "visible_bindings"))))
                        (visible-names
                          (mapcar (lambda (binding)
                                    (lookup binding "name"))
                                  visible))
                        (add-parameter
                          (find "add_parameter" suggestions
                                :key (lambda (suggestion)
                                       (lookup suggestion "kind"))
                                :test #'string=))
                        (replace-symbol
                          (find "replace_symbol" suggestions
                                :key (lambda (suggestion)
                                       (lookup suggestion "kind"))
                                :test #'string=))
                        (define-function
                          (find "define_function" suggestions
                                :key (lambda (suggestion)
                                       (lookup suggestion "kind"))
                                :test #'string=))
                        (rename-helper
                          (find-if
                           (lambda (suggestion)
                             (and (string= "rename_symbol"
                                           (lookup suggestion "kind"))
                                  (string= "REPAIR-KNOWN-HELPER"
                                           (lookup suggestion "to"))))
                           suggestions)))
                   (assert-true unbound
                                "missing unbound-variable repair condition: ~S"
                                conditions)
                   (assert-true undefined
                                "missing undefined-function repair condition: ~S"
                                conditions)
                   (assert-true (member "CELL" visible-names :test #'string=)
                                "repair should include visible CELL binding: ~S"
                                visible)
                   (assert-true (member "ENV" visible-names :test #'string=)
                                "repair should include visible ENV binding: ~S"
                                visible)
                   (assert-true add-parameter
                                "missing add-parameter repair: ~S"
                                suggestions)
                   (assert-true replace-symbol
                                "missing replace-symbol repair: ~S"
                                suggestions)
                   (assert-true define-function
                                "missing define-function repair: ~S"
                                suggestions)
                   (assert-true rename-helper
                                "missing rename candidate repair: ~S"
                                suggestions)
                   (assert-true
                    (plusp
                     (length
                      (array-items
                       (lookup (or unbound undefined)
                               "available_restarts"))))
                    "repair conditions should include available restarts")))
            (ignore-errors
             (let ((fasl (make-pathname :type "fasl"
                                        :defaults repair-path)))
               (when (probe-file fasl) (delete-file fasl))))))
        (let* ((macro-path (make-macro-source-file))
               (macro-file (namestring macro-path))
               (macro-resp
                 (clpm.repl:send-request
                  sock "sexpr-macroexpand-at"
                  :params (json-object
                           "path" (json-object "file" macro-file
                                               "top_level" 1))))
               (macro-result (lookup macro-resp "result")))
          (assert-true macro-result "macroexpand-at failed: ~S" macro-resp)
          (assert-equal "ok" (lookup macro-result "status")
                        "macroexpand-at should resolve uniquely")
          (assert-true (lookup macro-result "expanded_p")
                       "WHEN should expand: ~S" macro-result)
          (assert-true (search "IF" (lookup macro-result "expansion"))
                       "WHEN expansion should contain IF: ~S"
                       macro-result))
        (let* ((macro-path (make-macro-source-file))
               (macro-file (namestring macro-path))
               (expansion-resp
                 (clpm.repl:send-request
                  sock "sexpr-expansion-of"
                  :params (json-object
                           "path" (json-object "file" macro-file
                                               "top_level" 1))))
               (expansion-result (lookup expansion-resp "result"))
               (nodes (array-items (lookup expansion-result "nodes")))
               (root-origin (lookup (lookup (first nodes) "origin")
                                    "kind"))
               (source-node
                 (find-if
                  (lambda (node)
                    (let ((origin (lookup node "origin")))
                      (and (string= "source" (lookup origin "kind"))
                           (search "list" (lookup node "form")))))
                  nodes))
               (source-origin (and source-node
                                   (lookup source-node "origin")))
               (source-path (and source-origin
                                 (lookup source-origin "path")))
               (source-child-path
                 (and source-path
                      (array-items (lookup source-path "child_path")))))
          (assert-true expansion-result
                       "expansion-of failed: ~S" expansion-resp)
          (assert-equal "ok" (lookup expansion-result "status")
                        "expansion-of should resolve uniquely")
          (assert-equal "generated" root-origin
                        "expansion root should be generated")
          (assert-true source-node
                       "body form should retain source origin: ~S"
                       expansion-result)
          (assert-equal '(2) source-child-path
                        "body origin should point at WHEN body")
          (let* ((origin-resp
                   (clpm.repl:send-request
                    sock "sexpr-source-origin"
                    :params (json-object
                             "path" (json-object
                                     "file" macro-file
                                     "top_level" 1
                                     "expansion_path"
                                     (lookup source-node
                                             "expansion_path")))))
                 (origin-result (lookup origin-resp "result"))
                 (origin (lookup origin-result "origin"))
                 (origin-path (lookup origin "path")))
            (assert-true origin-result
                         "source-origin failed: ~S" origin-resp)
            (assert-equal "source" (lookup origin "kind")
                          "source-origin should return body source")
            (assert-equal '(2)
                          (array-items (lookup origin-path
                                               "child_path"))
                          "source-origin returned wrong child path"))
          (let* ((unknown-resp
                   (clpm.repl:send-request
                    sock "sexpr-source-origin"
                    :params (json-object
                             "path" (json-object
                                     "file" macro-file
                                     "top_level" 1
                                     "expansion_path"
                                     (list :array (list 99))))))
                 (unknown-result (lookup unknown-resp "result"))
                 (unknown-origin (lookup unknown-result "origin")))
            (assert-true unknown-result
                         "unknown source-origin failed: ~S"
                         unknown-resp)
            (assert-equal "unknown" (lookup unknown-origin "kind")
                          "invalid expansion path should be explicit")))
        (let* ((graph-path (make-call-graph-source-file))
               (graph-file (namestring graph-path))
               (graph-resp
                 (clpm.repl:send-request
                  sock "sexpr-call-graph"
                  :params (json-object "file" graph-file)))
               (graph-result (lookup graph-resp "result"))
               (certain-calls (array-items
                               (lookup graph-result "certain_calls")))
               (possible-calls (array-items
                                (lookup graph-result "possible_calls")))
               (dynamic-calls (array-items
                               (lookup graph-result "dynamic_calls")))
               (direct-call
                 (find "GRAPH-HELPER" certain-calls
                       :key (lambda (call)
                              (lookup (lookup call "callee") "name"))
                       :test #'string=))
               (generic-call
                 (find "GRAPH-EVALUATE" possible-calls
                       :key (lambda (call)
                              (lookup (lookup call "callee") "name"))
                       :test #'string=))
               (dynamic-call
                 (find "funcall" dynamic-calls
                       :key (lambda (call) (lookup call "operator"))
                       :test #'string=)))
          (assert-true graph-result "call graph failed: ~S" graph-resp)
          (assert-true direct-call
                       "direct call missing from certain_calls: ~S"
                       graph-result)
          (assert-true generic-call
                       "generic call missing from possible_calls: ~S"
                       graph-result)
          (assert-equal "generic_function" (lookup generic-call "reason")
                        "generic call should be dispatch-sensitive")
          (assert-true dynamic-call
                       "funcall missing from dynamic_calls: ~S"
                       graph-result)
          (assert-equal "function_value" (lookup dynamic-call "reason")
                        "funcall should be classified as dynamic"))
        (let* ((suffix (random (expt 2 32)))
               (sync-name (format nil "SEXPR-SYNC-~A" suffix))
               (image-only-name
                 (format nil "SEXPR-IMAGE-ONLY-~A" suffix))
               (sync-path (make-sync-source-file sync-name :loaded))
               (load-resp
                 (clpm.repl:send-request
                  sock "eval"
                  :params (json-object
                           "form"
                           (format nil "(load ~S :verbose nil :print nil)"
                                   (namestring sync-path))))))
          (assert-true (lookup load-resp "result")
                       "sync source load failed: ~S" load-resp)
          (sleep 1)
          (write-file sync-path
                      (format nil "(in-package #:cl-user)

(defun ~A ()
  :edited)
"
                              (string-downcase sync-name)))
          (let* ((image-resp
                   (clpm.repl:send-request
                    sock "eval"
                    :params (json-object
                             "form"
                             (format nil "(defun ~A () :image)"
                                     (string-downcase
                                      image-only-name)))))
                 (compare-resp
                   (clpm.repl:send-request
                    sock "sexpr-compare-image-source"
                    :params (json-object
                             "file" (namestring sync-path)
                             "package" "CL-USER")))
                 (compare-result (lookup compare-resp "result"))
                 (source-newer
                   (array-items
                    (lookup compare-result
                            "source_newer_than_image")))
                 (image-only
                   (array-items (lookup compare-result "image_only"))))
            (assert-true (lookup image-resp "result")
                         "image-only setup failed: ~S" image-resp)
            (assert-true compare-result
                         "source/image compare failed: ~S"
                         compare-resp)
            (assert-true (find sync-name source-newer
                               :key (lambda (entry)
                                      (lookup entry "name"))
                               :test #'string=)
                         "edited loaded function should be source-newer: ~S"
                         compare-result)
            (assert-true (find image-only-name image-only
                               :key (lambda (entry)
                                      (lookup entry "name"))
                               :test #'string=)
                         "eval-defined function should be image-only: ~S"
                         compare-result)))
        (let* ((lint-path (make-lint-source-file))
               (lint-resp
                 (clpm.repl:send-request
                  sock "sexpr-lint"
                  :params (json-object "file" (namestring lint-path))))
               (lint-result (lookup lint-resp "result"))
               (lints (array-items (lookup lint-result "lints")))
               (kinds (mapcar (lambda (lint)
                                 (lookup lint "kind"))
                               lints)))
          (assert-true lint-result "sexpr-lint failed: ~S" lint-resp)
          (dolist (kind '("eq_literal"
                          "mutating_quoted_constant"
                          "eval_where_macroexpand_may_suffice"))
            (assert-true (member kind kinds :test #'string=)
                         "missing lint kind ~A in ~S" kind lint-result))
          (dolist (lint lints)
            (assert-true (lookup lint "path")
                         "lint missing source path: ~S" lint)
            (assert-true (lookup lint "suggestion")
                         "lint missing suggestion: ~S" lint)
            (assert-true (lookup lint "certainty")
                         "lint missing certainty: ~S" lint)))
        (let* ((shape-path (make-macro-shape-source-file))
               (shape-file (namestring shape-path))
               (with-resp
                 (clpm.repl:send-request
                  sock "sexpr-macro-shape"
                  :params (json-object "file" shape-file
                                       "name" "with-foo")))
               (with-result (lookup with-resp "result"))
               (bindings (array-items
                          (lookup with-result "introduced_bindings")))
               (define-resp
                 (clpm.repl:send-request
                  sock "sexpr-macro-shape"
                  :params (json-object "file" shape-file
                                       "name" "define-thing")))
               (define-result (lookup define-resp "result"))
               (odd-resp
                 (clpm.repl:send-request
                  sock "sexpr-macro-shape"
                  :params (json-object "file" shape-file
                                       "name" "odd-macro")))
               (odd-result (lookup odd-resp "result")))
          (assert-true with-result "with macro shape failed: ~S"
                       with-resp)
          (assert-equal "binding_macro" (lookup with-result "kind")
                        "with macro should infer binding shape")
          (assert-equal "BODY" (lookup with-result "body_variable")
                        "with macro should expose body variable")
          (assert-true (find "FOO" bindings
                             :key (lambda (binding)
                                    (lookup binding "name"))
                             :test #'string=)
                       "with macro should infer introduced binding: ~S"
                       with-result)
          (assert-true (lookup with-result "uncertain")
                       "inferred macro shape should expose uncertainty")
          (assert-true define-result
                       "define macro shape failed: ~S" define-resp)
          (assert-equal "definition" (lookup define-result "kind")
                        "define macro should infer definition shape")
          (assert-equal 0 (lookup define-result
                                  "definition_name_position")
                        "define macro should infer name position")
          (assert-true odd-result "odd macro shape failed: ~S"
                       odd-resp)
          (assert-equal "unknown" (lookup odd-result "kind")
                        "unfamiliar macro shape should stay unknown")
          (assert-true (lookup odd-result "uncertain")
                       "unknown macro shape should be uncertain"))
        (let* ((contract-path (make-macro-contract-source-file))
               (contract-file (namestring contract-path))
               (contracts-resp
                 (clpm.repl:send-request
                  sock "sexpr-macro-contracts"
                  :params (json-object "file" contract-file)))
               (contracts-result (lookup contracts-resp "result"))
               (contracts
                 (array-items (lookup contracts-result "contracts")))
               (contract-diagnostics
                 (array-items (lookup contracts-result "diagnostics")))
               (with-contract
                 (find "WITH-BOUND" contracts
                       :key (lambda (contract)
                              (lookup contract "macro"))
                       :test #'string=))
               (shape-resp
                 (clpm.repl:send-request
                  sock "sexpr-macro-shape"
                  :params (json-object "file" contract-file
                                       "name" "with-bound")))
               (shape-result (lookup shape-resp "result"))
               (scope-resp
                 (clpm.repl:send-request
                  sock "sexpr-bindings-at"
                  :params (json-object
                           "path" (json-object
                                   "file" contract-file
                                   "kind" "defun"
                                   "name" "contract-scope"
                                   "child_path"
                                   (list :array (list 3 2))))))
               (scope-result (lookup scope-resp "result"))
               (lexical-bindings
                 (array-items
                  (lookup (lookup scope-result "scope")
                          "lexical_variables")))
               (resource-binding
                 (find "RESOURCE" lexical-bindings
                       :key (lambda (binding)
                              (lookup binding "name"))
                       :test #'string=))
               (list-resp
                 (clpm.repl:send-request
                  sock "sexpr-list-top-level-forms"
                  :params (json-object "file" contract-file)))
               (list-result (lookup list-resp "result"))
               (forms (array-items (lookup list-result "forms")))
               (widget-form
                 (find "SAMPLE-WIDGET" forms
                       :key (lambda (form)
                              (lookup form "name"))
                       :test #'string=)))
          (assert-true contracts-result
                       "macro contracts failed: ~S" contracts-resp)
          (assert-true with-contract
                       "missing explicit with-bound contract: ~S"
                       contracts-result)
          (assert-equal 1 (length contract-diagnostics)
                        "malformed contract should produce one diagnostic")
          (assert-true shape-result
                       "contract-backed macro shape failed: ~S"
                       shape-resp)
          (assert-equal :false (lookup shape-result "inferred")
                        "explicit contract shape should not be inferred")
          (assert-equal "binding_macro" (lookup shape-result "kind")
                        "explicit contract should drive macro shape")
          (assert-true resource-binding
                       "contract binding should appear in bindings-at: ~S"
                       scope-result)
          (assert-equal "editing-contract:WITH-BOUND"
                        (lookup resource-binding "introduced_by")
                        "contract binding should record its source")
          (assert-true widget-form
                       "contracted definition macro missing from index: ~S"
                       list-result)
          (assert-equal "definition" (lookup widget-form "kind")
                        "contracted definition macro should be classified"))
        (let* ((effect-path (make-effect-source-file))
               (effect-file (namestring effect-path))
               (slot-resp
                 (clpm.repl:send-request
                  sock "sexpr-effect-summary"
                  :params (json-object
                           "path" (json-object
                                   "file" effect-file
                                   "top_level" 1
                                   "child_path"
                                   (list :array (list 3))))))
               (slot-result (lookup slot-resp "result"))
               (slot-effect (lookup slot-result "effect"))
               (slot-writes (array-items (lookup slot-effect "writes")))
               (slot-reads (array-items (lookup slot-effect "reads")))
               (unknown-resp
                 (clpm.repl:send-request
                  sock "sexpr-effect-summary"
                  :params (json-object
                           "path" (json-object
                                   "file" effect-file
                                   "top_level" 1
                                   "child_path"
                                   (list :array (list 4))))))
               (unknown-result (lookup unknown-resp "result"))
               (unknown-effect (lookup unknown-result "effect"))
               (constant-resp
                 (clpm.repl:send-request
                  sock "sexpr-effect-summary"
                  :params (json-object
                           "path" (json-object
                                   "file" effect-file
                                   "top_level" 1
                                   "child_path"
                                   (list :array (list 5))))))
               (constant-result (lookup constant-resp "result"))
               (constant-effect (lookup constant-result "effect"))
               (variable-resp
                 (clpm.repl:send-request
                  sock "sexpr-effect-summary"
                  :params (json-object
                           "path" (json-object
                                   "file" effect-file
                                   "top_level" 1
                                   "child_path"
                                   (list :array (list 6))))))
               (variable-result (lookup variable-resp "result"))
               (variable-effect (lookup variable-result "effect")))
          (assert-true slot-result "slot effect failed: ~S" slot-resp)
          (assert-true (find "slot" slot-writes
                             :key (lambda (write)
                                    (lookup write "kind"))
                             :test #'string=)
                       "slot-value SETF should report a slot write: ~S"
                       slot-effect)
          (assert-true (find "X" slot-reads
                             :key (lambda (read)
                                    (lookup read "name"))
                             :test #'string=)
                       "slot-value SETF should read its object: ~S"
                       slot-effect)
          (assert-true unknown-result
                       "unknown call effect failed: ~S" unknown-resp)
          (assert-true (lookup unknown-effect "calls_unknown")
                       "unknown function call should be marked unknown: ~S"
                       unknown-effect)
          (assert-equal 1
                        (length (array-items
                                 (lookup unknown-effect "unknown_calls")))
                        "unknown function call should be listed once")
          (assert-true constant-result
                       "constant effect failed: ~S" constant-resp)
          (assert-equal 0
                        (length (array-items
                                 (lookup constant-effect "reads")))
                        "constant should have no reads")
          (assert-equal 0
                        (length (array-items
                                 (lookup constant-effect "writes")))
                        "constant should have no writes")
          (assert-equal :false (lookup constant-effect "calls_unknown")
                        "constant should not call unknown functions")
          (assert-equal :false (lookup constant-effect "allocates")
                        "constant should not allocate")
          (assert-true variable-result
                       "variable effect failed: ~S" variable-resp)
          (assert-equal 1
                        (length (array-items
                                 (lookup variable-effect "reads")))
                        "variable reference should have one read")
          (assert-equal "X"
                        (lookup (first (array-items
                                        (lookup variable-effect "reads")))
                                "name")
                        "variable read should preserve the symbol name")
          (assert-equal 0
                        (length (array-items
                                 (lookup variable-effect "writes")))
                        "variable reference should not write")
          (assert-equal :false (lookup variable-effect "calls_unknown")
                        "variable reference should not call unknown functions"))
        (let* ((rewrite-path (make-rewrite-source-file))
               (rewrite-file (namestring rewrite-path))
               (progn-resp
                 (clpm.repl:send-request
                  sock "sexpr-classify-rewrite"
                  :params (json-object
                           "path" (json-object
                                   "file" rewrite-file
                                   "top_level" 1
                                   "child_path"
                                   (list :array (list 3)))
                           "rewrite" "splice-progn")))
               (progn-result (lookup progn-resp "result"))
               (progn-classification
                 (lookup progn-result "classification"))
               (inline-resp
                 (clpm.repl:send-request
                  sock "sexpr-classify-rewrite"
                  :params (json-object
                           "path" (json-object
                                   "file" rewrite-file
                                   "top_level" 2
                                   "child_path"
                                   (list :array (list 3)))
                           "rewrite" "inline-let")))
               (inline-result (lookup inline-resp "result"))
               (inline-classification
                 (lookup inline-result "classification"))
               (inline-reasons
                 (array-items (lookup inline-classification "reasons")))
               (special-resp
                 (clpm.repl:send-request
                  sock "sexpr-classify-rewrite"
                  :params (json-object
                           "path" (json-object
                                   "file" rewrite-file
                                   "top_level" 3
                                   "child_path"
                                   (list :array (list 3)))
                           "rewrite" "inline-let")))
               (special-result (lookup special-resp "result"))
               (special-classification
                 (lookup special-result "classification"))
               (special-reasons
                 (array-items (lookup special-classification "reasons"))))
          (assert-true progn-result
                       "progn rewrite classification failed: ~S"
                       progn-resp)
          (assert-equal "safe"
                        (lookup progn-classification "classification")
                        "body-position PROGN splice should be safe")
          (assert-true (lookup progn-classification "safe")
                       "safe PROGN classification should set safe true")
          (assert-true inline-result
                       "inline rewrite classification failed: ~S"
                       inline-resp)
          (assert-equal "unsafe"
                        (lookup inline-classification "classification")
                        "effectful repeated LET value should be unsafe")
          (assert-true (find "would_duplicate_effects" inline-reasons
                             :key (lambda (reason)
                                    (lookup reason "kind"))
                             :test #'string=)
                       "inline classifier missing duplicate effect reason: ~S"
                       inline-classification)
          (assert-true special-result
                       "special declaration rewrite classification failed: ~S"
                       special-resp)
          (assert-equal "unsafe"
                        (lookup special-classification "classification")
                        "SPECIAL declaration boundary should be unsafe")
          (assert-true (find "declaration_scope" special-reasons
                             :key (lambda (reason)
                                    (lookup reason "kind"))
                             :test #'string=)
                       "inline classifier missing declaration-scope reason: ~S"
                       special-classification))
        (let* ((introduce-path (make-introduce-source-file))
               (introduce-file (namestring introduce-path))
               (introduce-resp
                 (clpm.repl:send-request
                  sock "sexpr-introduce-let"
                  :params (json-object
                           "path" (json-object
                                   "file" introduce-file
                                   "top_level" 1
                                   "child_path"
                                   (list :array (list 3 1)))
                           "name" "cached")))
               (introduce-result (lookup introduce-resp "result"))
               (updated-source (read-file-string introduce-path)))
          (assert-true introduce-result
                       "introduce-let failed: ~S" introduce-resp)
          (assert-equal "ok" (lookup introduce-result "status")
                        "introduce-let should commit a readable edit")
          (assert-true (lookup introduce-result "committed")
                       "introduce-let should commit by default")
          (assert-true (search "(let ((cached (cell-value cell)))"
                               updated-source)
                       "introduce-let did not wrap selected form: ~A"
                       updated-source)
          (assert-equal 1 (count-substrings "(cell-value cell)"
                                            updated-source)
                        "selected expression should appear once")
          (assert-equal 2 (count-substrings "cached" updated-source)
                        "introduced binding should appear twice"))
        (let* ((extract-path (make-extract-source-file))
               (extract-file (namestring extract-path))
               (extract-resp
                 (clpm.repl:send-request
                  sock "sexpr-extract-function"
                  :params (json-object
                           "path" (json-object
                                   "file" extract-file
                                   "top_level" 1
                                   "child_path"
                                   (list :array (list 3 1)))
                           "name" "multiply-values")))
               (extract-result (lookup extract-resp "result"))
               (extracted-source (read-file-string extract-path))
               (lambda-list
                 (array-items (lookup extract-result "lambda_list"))))
          (assert-true extract-result
                       "extract-function failed: ~S" extract-resp)
          (assert-equal "ok" (lookup extract-result "status")
                        "extract-function should commit")
          (assert-equal '("x" "y") lambda-list
                        "extract-function should compute free variables")
          (assert-true (search "(defun multiply-values (x y)"
                               extracted-source)
                       "extract-function did not insert defun: ~A"
                       extracted-source)
          (assert-true (search "(multiply-values x y)"
                               extracted-source)
                       "extract-function did not replace selected form: ~A"
                       extracted-source)
           (assert-equal 1 (count-substrings "(* x y)" extracted-source)
                         "selected expression should only remain in new defun"))
        (let* ((lambda-path (make-lambda-refactor-source-file))
               (lambda-file (namestring lambda-path))
               (optional-path
                 (json-object "file" lambda-file
                              "kind" "defun"
                              "name" "optional-target"))
               (candidate-resp
                 (clpm.repl:send-request
                  sock "sexpr-suggest-edit-candidates"
                  :params (json-object
                           "goal" "add-argument"
                           "path" optional-path
                           "name" "context"
                           "constraints"
                           (json-object
                            "preserve_existing_call_sites" t))))
               (candidate-result (lookup candidate-resp "result"))
               (candidates
                 (array-items (lookup candidate-result "candidates")))
               (optional-candidate
                 (find "add-optional-parameter" candidates
                       :key (lambda (candidate)
                              (lookup candidate "id"))
                       :test #'string=))
               (keyword-candidate
                 (find "add-keyword-parameter" candidates
                       :key (lambda (candidate)
                              (lookup candidate "id"))
                       :test #'string=))
               (relaxed-resp
                 (clpm.repl:send-request
                  sock "sexpr-suggest-edit-candidates"
                  :params (json-object
                           "goal" "add-argument"
                           "path" optional-path
                           "name" "context")))
               (relaxed-result (lookup relaxed-resp "result"))
               (relaxed-candidates
                 (array-items (lookup relaxed-result "candidates")))
               (relaxed-first (first relaxed-candidates))
               (candidate-apply-resp
                 (and optional-candidate
                      (clpm.repl:send-request
                       sock (lookup optional-candidate "apply_method")
                       :params (lookup optional-candidate
                                       "apply_params"))))
               (candidate-apply-result
                 (lookup candidate-apply-resp "result"))
               (add-resp
                 (clpm.repl:send-request
                  sock "sexpr-add-keyword-arg"
                  :params (json-object
                           "path" optional-path
                           "name" "context")))
               (add-result (lookup add-resp "result"))
               (after-add (read-file-string lambda-path))
               (add-broken
                 (array-items (lookup add-result "broken_call_sites"))))
          (assert-true candidate-result
                       "candidate generation failed: ~S" candidate-resp)
          (assert-equal 2 (lookup candidate-result "candidate_count")
                        "add-argument should return two candidates")
          (assert-true optional-candidate
                       "missing optional candidate: ~S" candidate-result)
          (assert-true keyword-candidate
                       "missing keyword candidate: ~S" candidate-result)
          (assert-true (> (lookup keyword-candidate "rank")
                          (lookup optional-candidate "rank"))
                       "preserve-existing-call-sites should rank keyword first")
          (assert-equal "add-optional-parameter"
                        (lookup relaxed-first "id")
                        "relaxed constraints should rank optional first")
          (assert-equal "sexpr-change-lambda-list"
                        (lookup optional-candidate "apply_method")
                        "optional candidate should use lambda-list transaction")
          (assert-true candidate-apply-result
                       "candidate transaction dry run failed: ~S"
                       candidate-apply-resp)
          (assert-equal :false
                        (lookup candidate-apply-result "committed")
                        "candidate params should not commit by default")
          (assert-true add-result
                       "add-keyword-arg failed: ~S" add-resp)
          (assert-equal "ok" (lookup add-result "status")
                        "add-keyword-arg should commit")
          (assert-equal 0 (length add-broken)
                        "adding a keyword argument should preserve old call sites")
          (assert-true (search "(defun optional-target (x &key context)"
                               after-add)
                       "add-keyword-arg did not update lambda list: ~A"
                       after-add)
          (assert-true (search "(optional-target 1)" after-add)
                       "add-keyword-arg should leave old direct calls valid: ~A"
                       after-add)
          (let* ((change-resp
                   (clpm.repl:send-request
                    sock "sexpr-change-lambda-list"
                    :params (json-object
                             "path" optional-path
                             "lambda_list" "(x &optional context)")))
                 (change-result (lookup change-resp "result"))
                 (change-broken
                   (array-items (lookup change-result
                                        "broken_call_sites"))))
            (assert-true change-result
                         "change-lambda-list failed: ~S" change-resp)
            (assert-equal "ok" (lookup change-result "status")
                          "change-lambda-list should commit")
            (assert-equal 0 (length change-broken)
                          "adding an optional argument should preserve old call sites"))
          (let* ((convert-resp
                   (clpm.repl:send-request
                    sock "sexpr-convert-to-keyword-argument"
                    :params (json-object
                             "path" (json-object
                                     "file" lambda-file
                                     "kind" "defun"
                                     "name" "api-target")
                             "argument_position" 1
                             "keyword" ":formula")))
                 (convert-result (lookup convert-resp "result"))
                 (after-convert (read-file-string lambda-path))
                 (dynamic-caveats
                   (array-items (lookup convert-result
                                        "dynamic_caveats"))))
            (assert-true convert-result
                         "convert-to-keyword-argument failed: ~S"
                         convert-resp)
            (assert-equal "ok" (lookup convert-result "status")
                          "convert-to-keyword-argument should commit")
            (assert-true (search "(defun api-target (cell env &key formula)"
                                 after-convert)
                         "convert did not update lambda list: ~A"
                         after-convert)
            (assert-true (search "(api-target cell env :formula formula)"
                                 after-convert)
                         "convert did not update direct call site: ~A"
                         after-convert)
            (assert-equal 1 (length dynamic-caveats)
                          "convert should report APPLY as a dynamic caveat")))
        (let* ((structural-path (make-structural-move-source-file))
               (structural-file (namestring structural-path))
               (splice-resp
                 (clpm.repl:send-request
                  sock "sexpr-splice-form"
                  :params (json-object
                           "path" (json-object
                                   "file" structural-file
                                   "kind" "defun"
                                   "name" "splice-demo"
                                   "child_path"
                                   (list :array (list 3))))))
               (splice-result (lookup splice-resp "result"))
               (after-splice (read-file-string structural-path)))
          (assert-true splice-result
                       "splice-form failed: ~S" splice-resp)
          (assert-equal "ok" (lookup splice-result "status")
                        "splice-form should commit")
          (assert-true (not (search "(progn" after-splice))
                       "splice-form should remove the PROGN wrapper: ~A"
                       after-splice)
          (assert-true (and (search "(alpha)" after-splice)
                            (search "(beta)" after-splice)
                            (search "(gamma)" after-splice))
                       "splice-form should keep all body forms: ~A"
                       after-splice)
          (let* ((raise-resp
                   (clpm.repl:send-request
                    sock "sexpr-raise-form"
                    :params (json-object
                             "path" (json-object
                                     "file" structural-file
                                     "kind" "defun"
                                     "name" "raise-demo"
                                     "child_path"
                                     (list :array (list 3 1))))))
                 (raise-result (lookup raise-resp "result"))
                 (after-raise (read-file-string structural-path)))
            (assert-true raise-result
                         "raise-form failed: ~S" raise-resp)
            (assert-equal "ok" (lookup raise-result "status")
                          "raise-form should commit")
            (assert-true (not (search "(outer" after-raise))
                         "raise-form should replace the parent: ~A"
                         after-raise)
            (assert-true (search "(inner x)" after-raise)
                         "raise-form should keep the selected child: ~A"
                         after-raise))
          (let* ((transpose-resp
                   (clpm.repl:send-request
                    sock "sexpr-transpose-forms"
                    :params (json-object
                             "path" (json-object
                                     "file" structural-file
                                     "kind" "defun"
                                     "name" "transpose-demo"
                                     "child_path"
                                     (list :array (list 3)))
                             "other_child_path"
                             (list :array (list 4)))))
                 (transpose-result (lookup transpose-resp "result"))
                 (after-transpose (read-file-string structural-path))
                 (first-pos (search "(first)" after-transpose))
                 (second-pos (search "(second)" after-transpose))
                 (third-pos (search "(third)" after-transpose)))
            (assert-true transpose-result
                         "transpose-forms failed: ~S" transpose-resp)
            (assert-equal "ok" (lookup transpose-result "status")
                          "transpose-forms should commit")
            (assert-true (and first-pos second-pos third-pos
                              (< second-pos first-pos)
                              (< first-pos third-pos))
                         "transpose-forms should swap sibling extents: ~A"
                         after-transpose)))
        (let* ((bind-path (make-bind-repeated-source-file))
               (bind-file (namestring bind-path))
               (bind-resp
                 (clpm.repl:send-request
                  sock "sexpr-bind-repeated-expression"
                  :params (json-object
                           "path" (json-object
                                   "file" bind-file
                                   "top_level" 1)
                           "expression" "(car cell)"
                           "name" "cached")))
               (bind-result (lookup bind-resp "result"))
               (bound-source (read-file-string bind-path))
               (before-effect-source bound-source)
               (reject-resp
                 (clpm.repl:send-request
                  sock "sexpr-bind-repeated-expression"
                  :params (json-object
                           "path" (json-object
                                   "file" bind-file
                                   "top_level" 2)
                           "expression" "(unknown-effect cell)"
                           "name" "bad")))
               (reject-result (lookup reject-resp "result"))
               (after-effect-source (read-file-string bind-path)))
          (assert-true bind-result
                       "bind-repeated-expression failed: ~S" bind-resp)
          (assert-equal "ok" (lookup bind-result "status")
                        "pure repeated expression should be bound")
          (assert-equal 2 (lookup bind-result "match_count")
                        "expected two repeated matches")
          (assert-true (search "(let ((cached (car cell)))"
                               bound-source)
                       "bind-repeated did not introduce a LET: ~A"
                       bound-source)
          (assert-equal 1 (count-substrings "(car cell)"
                                            bound-source)
                        "bound expression should evaluate once")
          (assert-equal 3 (count-substrings "cached" bound-source)
                        "binding should appear once plus two uses")
          (assert-true reject-result
                       "effectful repeated expression response missing: ~S"
                       reject-resp)
          (assert-equal "rejected" (lookup reject-result "status")
                        "effectful repeated expression should be rejected")
          (assert-equal before-effect-source after-effect-source
                        "rejected repeated binding should not edit the file"))
        (let* ((scope-path (make-scope-source-file))
               (scope-file (namestring scope-path))
               (bindings-resp
                 (clpm.repl:send-request
                  sock "sexpr-bindings-at"
                  :params (json-object
                           "path" (json-object
                                   "file" scope-file
                                   "top_level" 1
                                   "child_path"
                                   (list :array (list 3 2 2 2))))))
               (bindings-result (lookup bindings-resp "result"))
               (scope (lookup bindings-result "scope")))
          (assert-true bindings-result "bindings-at failed: ~S"
                       bindings-resp)
          (assert-equal "ok" (lookup bindings-result "status")
                        "bindings-at should resolve uniquely")
          (let ((lexical (entry-names (lookup scope "lexical_variables")))
                (locals (entry-names (lookup scope "local_functions")))
                (symbol-macros (entry-names (lookup scope "symbol_macros"))))
            (dolist (name '("CELL" "ENV" "VALUE"))
              (assert-true (member name lexical :test #'string=)
                           "missing lexical binding ~A in ~S"
                           name lexical))
            (assert-true (member "RESOLVE" locals :test #'string=)
                         "missing local function in ~S" locals)
            (assert-true (member "CURRENT" symbol-macros :test #'string=)
                         "missing symbol macro in ~S" symbol-macros)))
        (let* ((symbol-resp
                 (clpm.repl:send-request
                  sock "sexpr-symbol-info"
                  :params (json-object "symbol" "format"
                                       "package" "CL")))
               (symbol-result (lookup symbol-resp "result"))
               (kinds (array-items (lookup symbol-result "kinds")))
               (definitions (array-items (lookup symbol-result
                                                "definitions"))))
          (assert-true symbol-result "symbol-info failed: ~S" symbol-resp)
          (assert-equal "FORMAT" (lookup symbol-result "symbol")
                        "wrong symbol name")
          (assert-equal "COMMON-LISP" (lookup symbol-result "home_package")
                        "wrong home package")
          (assert-equal "external" (lookup symbol-result "package_status")
                        "FORMAT should be external in CL")
          (assert-true (lookup symbol-result "external")
                       "FORMAT should be exported from CL")
          (assert-true (member "function" kinds :test #'string=)
                       "FORMAT should include function kind: ~S" kinds)
          (assert-true definitions
                       "FORMAT should have definition entries"))
        (let* ((setup-resp
                 (clpm.repl:send-request
                  sock "eval"
                  :params
                  (json-object
                   "form"
                   "(defclass sexpr-workbook-demo ()
                      ((cells :initarg :cells
                              :accessor sexpr-workbook-cells)
                       (mode :initarg :mode
                             :reader sexpr-workbook-mode
                             :initform :automatic)))")))
               (class-resp
                 (clpm.repl:send-request
                  sock "sexpr-class-info"
                  :params (json-object "symbol" "sexpr-workbook-demo")))
               (class-result (lookup class-resp "result"))
               (slots (array-items (lookup class-result "slots")))
               (cells-slot (find "CELLS" slots
                                 :key (lambda (slot)
                                        (lookup slot "name"))
                                 :test #'string=))
               (initargs (and cells-slot
                              (array-items (lookup cells-slot
                                                   "initargs"))))
               (readers (and cells-slot
                             (array-items (lookup cells-slot
                                                  "readers")))))
          (assert-true (lookup setup-resp "result")
                       "class setup failed: ~S" setup-resp)
          (assert-true class-result
                       "class-info failed: ~S" class-resp)
          (assert-equal "SEXPR-WORKBOOK-DEMO"
                        (lookup class-result "name")
                        "wrong class name")
          (assert-true cells-slot
                       "class-info missing CELLS slot: ~S" class-result)
          (assert-true (lookup cells-slot "direct")
                       "CELLS slot should be direct")
          (assert-true (member ":CELLS" initargs :test #'string=)
                       "CELLS slot missing initarg: ~S" cells-slot)
          (assert-true (member "SEXPR-WORKBOOK-CELLS" readers
                               :test #'string=)
                       "CELLS slot missing accessor reader: ~S"
                       cells-slot))
        (let* ((method-path (make-add-method-source-file))
               (method-file (namestring method-path))
               (before (read-file-string method-path))
               (plan-resp
                 (clpm.repl:send-request
                  sock "sexpr-add-method"
                  :params
                  (json-object
                   "file" method-file
                   "generic" "add-demo"
                   "lambda_list" "((thing add-formula-demo) env)"
                   "body" "(list thing env)"
                   "dry_run" t)))
               (plan-result (lookup plan-resp "result"))
               (plan-diff (and plan-result
                               (array-items
                                (lookup plan-result
                                        "structural_diff")))))
          (assert-true plan-result "add-method dry-run failed: ~S"
                       plan-resp)
          (assert-true (lookup plan-result "dry_run")
                       "add-method plan should be marked dry-run")
          (assert-true plan-diff
                       "add-method plan should include structural diff")
          (assert-equal before (read-file-string method-path)
                        "add-method dry-run changed the file")
          (let* ((apply-resp
                   (clpm.repl:send-request
                    sock "sexpr-add-method"
                    :params
                    (json-object
                     "file" method-file
                     "generic" "add-demo"
                     "lambda_list" "((thing add-formula-demo) env)"
                     "body" "(list thing env)")))
                 (apply-result (lookup apply-resp "result"))
                 (after (read-file-string method-path)))
            (assert-true apply-result "add-method apply failed: ~S"
                         apply-resp)
            (assert-true (search "(defmethod add-demo ((thing add-formula-demo) env)
  (list thing env))"
                                 after)
                         "add-method did not insert expected defmethod: ~S"
                         after)))
        (let* ((setup-resp
                 (clpm.repl:send-request
                  sock "eval"
                  :params
                  (json-object
                   "form"
                   "(progn
                      (defclass sexpr-formula-demo () ())
                      (defclass sexpr-env-demo () ())
                      (defgeneric sexpr-evaluate-demo (thing env))
                      (defmethod sexpr-evaluate-demo
                          ((thing sexpr-formula-demo)
                           (env sexpr-env-demo))
                        (list thing env))
                      (defmethod sexpr-evaluate-demo :around
                          ((thing sexpr-formula-demo) env)
                        (call-next-method)))")))
               (generic-resp
                 (clpm.repl:send-request
                  sock "sexpr-generic-info"
                  :params (json-object "symbol" "sexpr-evaluate-demo")))
               (generic-result (lookup generic-resp "result"))
               (methods (array-items (lookup generic-result "methods")))
               (method-qualifiers
                 (mapcar (lambda (method)
                           (array-items (lookup method "qualifiers")))
                         methods))
               (specializer-names
                 (loop for method in methods
                       append
                       (loop for specializer in
                             (array-items (lookup method "specializers"))
                             collect (lookup specializer "name")))))
          (assert-true (lookup setup-resp "result")
                       "generic setup failed: ~S" setup-resp)
          (assert-true generic-result
                       "generic-info failed: ~S" generic-resp)
          (assert-equal "SEXPR-EVALUATE-DEMO"
                        (lookup generic-result "name")
                        "wrong generic function name")
          (assert-equal 2 (lookup generic-result "method_count")
                        "wrong method count")
          (assert-true (member '(":AROUND") method-qualifiers
                               :test #'equal)
                       "missing :around qualifier: ~S"
                       method-qualifiers)
          (assert-true (member "SEXPR-FORMULA-DEMO" specializer-names
                               :test #'string=)
                       "missing formula specializer: ~S"
                       specializer-names))
        (let* ((graph-resp
                 (clpm.repl:send-request
                  sock "sexpr-system-graph"
                  :params (json-object "name" "clpm")))
               (graph-result (lookup graph-resp "result"))
               (components (array-items (lookup graph-result
                                                "components")))
               (component-names (mapcar (lambda (component)
                                          (lookup component "name"))
                                        components)))
          (assert-true graph-result "system graph failed: ~S" graph-resp)
          (assert-true (member "packages" component-names :test #'string=)
                       "system graph missing packages component")
          (assert-true (member "sexpr_edit" component-names :test #'string=)
                       "system graph missing sexpr_edit component")
          (let* ((affected-resp
                   (clpm.repl:send-request
                    sock "sexpr-affected-files"
                    :params (json-object
                             "name" "clpm"
                             "file" (namestring
                                     (truename "src/packages.lisp")))))
                 (affected-result (lookup affected-resp "result"))
                 (affected-files (array-items
                                  (lookup affected-result
                                          "affected_files"))))
            (assert-true affected-result
                         "affected files failed: ~S" affected-resp)
            (assert-true
             (some (lambda (affected-file)
                     (string-suffix-p "src/sexpr_edit.lisp"
                                      affected-file))
                   affected-files)
             "packages.lisp should affect sexpr_edit.lisp: ~S"
             affected-result)
            (assert-true
             (some (lambda (affected-file)
                     (string-suffix-p "src/repl.lisp" affected-file))
                   affected-files)
             "packages.lisp should affect repl.lisp: ~S"
             affected-result)))))))
(format t "  RPC source lenses OK~%")

(format t "~%SexprEdit tests PASSED!~%")
(sb-ext:exit :code 0)
