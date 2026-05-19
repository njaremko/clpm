;;;; test/repl-bridge-preload-test.lisp -- serve preloads project :systems

(require :asdf)

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

;;; ----------------------------------------------------------------------------
;;; Preload: %bridge-load-project loads each name in :systems.

(clpm.store:with-temp-dir (tmp)
  (let* ((proj (merge-pathnames "myproj/" tmp))
         (clpm-dir (merge-pathnames ".clpm/" proj))
         (manifest (merge-pathnames "clpm.project" proj))
         (asd (merge-pathnames "rb-preload-fixture.asd" proj))
         (lisp (merge-pathnames "rb-preload-fixture.lisp" proj))
         (asdf-config (merge-pathnames "asdf-config.lisp" clpm-dir)))
    (ensure-directories-exist clpm-dir)

    ;; A minimal system in the project root: one file defining a package
    ;; that exports one obvious symbol.
    (with-open-file (s asd :direction :output :if-exists :supersede
                           :external-format :utf-8)
      (write-string
"(asdf:defsystem #:rb-preload-fixture
  :version \"0.0.1\"
  :components ((:file \"rb-preload-fixture\")))" s))

    (with-open-file (s lisp :direction :output :if-exists :supersede
                            :external-format :utf-8)
      (write-string
"(defpackage #:rb-preload-fixture (:use #:cl) (:export #:marker))
(in-package #:rb-preload-fixture)
(defparameter marker :preloaded)" s))

    (with-open-file (s manifest :direction :output :if-exists :supersede
                                :external-format :utf-8)
      (write-string
"(:project :name \"myproj\" :version \"0.1.0\"
 :systems (\"rb-preload-fixture\") :registries ())" s))

    ;; asdf-config: push the project root onto the source-registry so the
    ;; .asd above is discoverable.
    (with-open-file (s asdf-config :direction :output :if-exists :supersede
                                   :external-format :utf-8)
      (let ((root (namestring proj)))
        (format s "(asdf:initialize-source-registry~%  '(:source-registry (:directory ~S) :inherit-configuration))~%"
                root)))

    ;; Make sure the system isn't already loaded from a previous run.
    (assert-true (null (asdf:component-loaded-p "rb-preload-fixture"))
                 "rb-preload-fixture was already loaded before the test")

    ;; Drive %bridge-load-project directly.
    (let ((fn (find-symbol "%BRIDGE-LOAD-PROJECT" (find-package "CLPM.COMMANDS"))))
      (assert-true fn "could not find %BRIDGE-LOAD-PROJECT in CLPM.COMMANDS")
      (funcall fn proj))

    (assert-true (asdf:component-loaded-p "rb-preload-fixture")
                 "rb-preload-fixture was not loaded by %bridge-load-project")
    (assert-true (find-package "RB-PRELOAD-FIXTURE")
                 "package RB-PRELOAD-FIXTURE was not defined")
    (let* ((sym (find-symbol "MARKER" "RB-PRELOAD-FIXTURE"))
           (val (and sym (boundp sym) (symbol-value sym))))
      (assert-true (eq val :preloaded)
                   "MARKER not bound to :preloaded; got ~S" val))
    (format t "  preload OK~%")))

(format t "~%REPL-bridge preload tests PASSED!~%")
(sb-ext:exit :code 0)
