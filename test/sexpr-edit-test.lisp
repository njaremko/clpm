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
(when t :ok)
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
      (assert-true (member "sexpr-macroexpand-at" method-names :test #'string=)
                   "sexpr-macroexpand-at missing from methods")
      (assert-true (member "sexpr-bindings-at" method-names :test #'string=)
                   "sexpr-bindings-at missing from methods")
      (assert-true (member "sexpr-symbol-info" method-names :test #'string=)
                   "sexpr-symbol-info missing from methods")
      (assert-true (member "sexpr-generic-info" method-names :test #'string=)
                   "sexpr-generic-info missing from methods")
      (assert-true (member "sexpr-system-graph" method-names :test #'string=)
                   "sexpr-system-graph missing from methods")
      (assert-true (member "sexpr-affected-files" method-names :test #'string=)
                   "sexpr-affected-files missing from methods")
      (assert-true (member "sexpr-validate-edit" method-names :test #'string=)
                   "sexpr-validate-edit missing from methods")
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
                                                "structural_diff"))))
          (assert-true plan-result "plan edit failed: ~S" plan-resp)
          (assert-true (lookup plan-result "dry_run")
                       "plan result should be marked dry-run")
          (assert-true plan-diff
                       "plan result should include structural diff")
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
               (bad-read (first bad-steps)))
          (assert-true bad-result "read validation failed to return: ~S"
                       bad-resp)
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
