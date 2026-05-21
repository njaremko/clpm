;;;; test/repl-manifest-test.lisp - :repl manifest field

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

(defun assert-equal (expected actual)
  (unless (equal expected actual)
    (fail "expected ~S, got ~S" expected actual)))

;;; ----------------------------------------------------------------------------
;;; Parser accepts :repl plist.

(format t "Test: parser accepts :repl field~%")
(clpm.store:with-temp-dir (tmp)
  (let ((manifest (merge-pathnames "clpm.project" tmp)))
    (with-open-file (s manifest :direction :output :if-exists :supersede
                                :external-format :utf-8)
      (write-string
"(:project :name \"x\" :version \"0.1.0\" :systems (\"x\") :registries ()
 :repl (:autostart t :preload (\"alexandria\")))" s))
    (let ((p (clpm.project:read-project-file manifest)))
      (assert-true p "read-project-file returned NIL")
      (let ((rb (clpm.project:project-repl p)))
        (assert-true (consp rb) "project-repl returned ~S, expected a plist" rb)
        (assert-true (eq t (getf rb :autostart))
                     "expected :autostart t, got ~S" (getf rb :autostart))
        (assert-equal '("alexandria") (getf rb :preload)))))
  (format t "  parse OK~%"))

;;; ----------------------------------------------------------------------------
;;; Serializer round-trips :repl.

(format t "Test: serializer round-trips :repl~%")
(clpm.store:with-temp-dir (tmp)
  (let* ((manifest (merge-pathnames "clpm.project" tmp))
         (p (clpm.project:make-project
             :name "y"
             :version "0.1.0"
             :systems '("y")
             :repl '(:autostart t :preload ("alexandria")))))
    (clpm.project:write-project-file p manifest)
    (let ((p2 (clpm.project:read-project-file manifest)))
      (let ((rb (clpm.project:project-repl p2)))
        (assert-true (eq t (getf rb :autostart)) "lost :autostart on round-trip")
        (assert-equal '("alexandria") (getf rb :preload)))))
  (format t "  round-trip OK~%"))

;;; ----------------------------------------------------------------------------
;;; Manifest with NO :repl field stays NIL (no spurious key in
;;; serialization either).

(format t "Test: omitting :repl keeps the field NIL~%")
(clpm.store:with-temp-dir (tmp)
  (let ((manifest (merge-pathnames "clpm.project" tmp)))
    (with-open-file (s manifest :direction :output :if-exists :supersede
                                :external-format :utf-8)
      (write-string "(:project :name \"z\" :version \"0.1.0\" :systems () :registries ())" s))
    (let ((p (clpm.project:read-project-file manifest)))
      (assert-true (null (clpm.project:project-repl p))
                   "expected NIL repl slot, got ~S"
                   (clpm.project:project-repl p))
      ;; Re-serialize and confirm no :repl key leaks in.
      (let* ((serialized (with-output-to-string (s)
                           (let ((form (clpm.project:serialize-project p)))
                             (write form :stream s :readably t)))))
        (assert-true (not (search ":REPL" serialized :test #'char-equal))
                     "serialized form leaked :repl: ~A" serialized))))
  (format t "  omission OK~%"))

;;; ----------------------------------------------------------------------------
;;; Preload pulls :preload systems in addition to :systems.

(format t "Test: %bridge-load-project preloads :repl :preload entries~%")
(clpm.store:with-temp-dir (tmp)
  (let* ((proj (merge-pathnames "myproj/" tmp))
         (clpm-dir (merge-pathnames ".clpm/" proj))
         (manifest (merge-pathnames "clpm.project" proj))
         (asd-a (merge-pathnames "rb-fixture-a.asd" proj))
         (lisp-a (merge-pathnames "rb-fixture-a.lisp" proj))
         (asd-b (merge-pathnames "rb-fixture-b.asd" proj))
         (lisp-b (merge-pathnames "rb-fixture-b.lisp" proj))
         (asdf-config (merge-pathnames "asdf-config.lisp" clpm-dir)))
    (ensure-directories-exist clpm-dir)
    (with-open-file (s asd-a :direction :output :if-exists :supersede
                             :external-format :utf-8)
      (write-string
"(asdf:defsystem #:rb-fixture-a :components ((:file \"rb-fixture-a\")))" s))
    (with-open-file (s lisp-a :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (write-string
"(defpackage #:rb-fixture-a (:use #:cl)) (in-package #:rb-fixture-a)" s))
    (with-open-file (s asd-b :direction :output :if-exists :supersede
                             :external-format :utf-8)
      (write-string
"(asdf:defsystem #:rb-fixture-b :components ((:file \"rb-fixture-b\")))" s))
    (with-open-file (s lisp-b :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (write-string
"(defpackage #:rb-fixture-b (:use #:cl)) (in-package #:rb-fixture-b)" s))
    (with-open-file (s manifest :direction :output :if-exists :supersede
                                :external-format :utf-8)
      (write-string
"(:project :name \"myproj\" :version \"0.1.0\"
 :systems (\"rb-fixture-a\") :registries ()
 :repl (:preload (\"rb-fixture-b\")))" s))
    (with-open-file (s asdf-config :direction :output :if-exists :supersede
                                   :external-format :utf-8)
      (format s "(asdf:initialize-source-registry~%  '(:source-registry (:directory ~S) :inherit-configuration))~%"
              (namestring proj)))

    (assert-true (null (asdf:component-loaded-p "rb-fixture-a"))
                 "rb-fixture-a already loaded")
    (assert-true (null (asdf:component-loaded-p "rb-fixture-b"))
                 "rb-fixture-b already loaded")

    (funcall (find-symbol "%BRIDGE-LOAD-PROJECT"
                          (find-package "CLPM.COMMANDS"))
             proj)

    (assert-true (asdf:component-loaded-p "rb-fixture-a")
                 "rb-fixture-a (in :systems) was not loaded")
    (assert-true (asdf:component-loaded-p "rb-fixture-b")
                 "rb-fixture-b (in :preload) was not loaded"))
  (format t "  preload-and-systems OK~%"))

(format t "~%REPL manifest tests PASSED!~%")
(sb-ext:exit :code 0)
