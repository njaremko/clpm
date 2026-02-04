;;;; lisp.lisp - Lisp implementation detection and argv construction

(in-package #:clpm.lisp)

(defun parse-lisp-kind (value)
  "Parse VALUE as a Lisp implementation selector.

Accepts keywords (:sbcl/:ccl/:ecl) or strings (\"sbcl\"/\"ccl\"/\"ecl\").
Returns a keyword."
  (cond
    ((keywordp value)
     (case value
       ((:sbcl :ccl :ecl) value)
       (t
        (clpm.errors:signal-error 'clpm.errors:clpm-user-error
                                  "Unsupported --lisp value: ~S (expected sbcl|ccl|ecl)" value))))
    ((stringp value)
     (let ((s (string-downcase value)))
       (cond
         ((string= s "sbcl") :sbcl)
         ((string= s "ccl") :ccl)
         ((string= s "ecl") :ecl)
         (t
          (clpm.errors:signal-error 'clpm.errors:clpm-user-error
                                    "Unsupported --lisp value: ~S (expected sbcl|ccl|ecl)" value)))))
    (t
     (clpm.errors:signal-error 'clpm.errors:clpm-user-error
                               "Invalid --lisp value: ~S (expected sbcl|ccl|ecl)" value))))

(defun lisp-program-name (kind)
  "Return the program name for KIND."
  (ecase kind
    (:sbcl "sbcl")
    (:ccl "ccl")
    (:ecl "ecl")))

(defun find-lisp (kind)
  "Find the Lisp implementation for KIND.

Returns an absolute path string or NIL."
  (clpm.platform:which (lisp-program-name kind)))

(defun lisp-version (kind)
  "Return the implementation version string for KIND by running `<impl> --version`."
  (let ((prog (find-lisp kind)))
    (unless prog
      (error 'clpm.errors:clpm-missing-tool-error
             :tool (lisp-program-name kind)
             :install-hints (clpm.platform:tool-install-hints (lisp-program-name kind))))
    (multiple-value-bind (output _err exit-code)
        (clpm.platform:run-program (list prog "--version")
                                   :output :string
                                   :error-output nil)
      (declare (ignore _err))
      (unless (zerop exit-code)
        (clpm.errors:signal-error 'clpm.errors:clpm-user-error
                                  "Failed to run ~A --version (exit ~D)"
                                  (lisp-program-name kind) exit-code))
      (string-trim '(#\Space #\Tab #\Newline #\Return) (or output "")))))

(defun %ensure-string-list (value label)
  (unless (or (null value) (and (listp value) (every #'stringp value)))
    (clpm.errors:signal-error 'clpm.errors:clpm-user-error
                              "~A must be a list of strings, got ~S" label value))
  (or value '()))

(defun lisp-run-argv (kind &key load-files eval-forms noinform noninteractive disable-debugger)
  "Build an argv list to run KIND and evaluate forms.

LOAD-FILES is a list of path strings to load.
EVAL-FORMS is a list of strings to evaluate in order."
  (let* ((kind (parse-lisp-kind kind))
         (prog (find-lisp kind)))
    (unless prog
      (error 'clpm.errors:clpm-missing-tool-error
             :tool (lisp-program-name kind)
             :install-hints (clpm.platform:tool-install-hints (lisp-program-name kind))))
    (let* ((load-files (%ensure-string-list load-files "load-files"))
           (eval-forms (%ensure-string-list eval-forms "eval-forms"))
           (disable-debugger (if (null disable-debugger)
                                 (and noninteractive (eq kind :sbcl))
                                 disable-debugger))
           (args
             (append
              (list prog)
              (case kind
                (:sbcl
                 (append
                  (when noinform '("--noinform"))
                  (when noninteractive '("--non-interactive"))
                  (when disable-debugger '("--disable-debugger"))))
                (:ccl
                 (append
                  (when noinform '("--quiet"))
                  (when noninteractive '("--batch"))))
                (:ecl
                 (append
                  '("-norc")
                  (when noinform '("-q")))))
              (mapcan (lambda (p)
                        (let ((p (namestring (uiop:ensure-pathname p :want-existing nil))))
                          (case kind
                            (:ecl (list "-load" p))
                            (t (list "--load" p)))))
                      load-files)
              (mapcan (lambda (form)
                        (case kind
                          (:ecl (list "-eval" form))
                          (t (list "--eval" form))))
                      eval-forms))))
      args)))
