;;;; test/repl-methods-test.lisp - `methods' / `help' discovery.
;;;;
;;;; BRIDGE_V2 #106: the daemon advertises its RPC surface from the same
;;;; registry the dispatcher reads, so docs cannot drift from code.

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

(defun lookup (object key)
  (when (and (consp object) (eq (car object) :object))
    (cdr (assoc key (cadr object) :test #'string=))))

(defun array-items (a)
  (when (and (consp a) (eq (car a) :array))
    (cadr a)))

(defparameter +schema-param-types+
  '(:any :string :integer :boolean :array :object :string-or-boolean))

(format t "Test: method parameter types use a closed internal algebra~%")
(dolist (entry clpm.repl::+method-registry+)
  (let ((spec (cdr entry)))
    (dolist (param (clpm.repl::method-spec-params spec))
      (let ((type (getf param :type)))
        (assert-true (and (keywordp type)
                          (member type +schema-param-types+))
                     "open param type ~S in method ~A param ~A"
                     type
                     (clpm.repl::method-spec-name spec)
                     (getf param :name))))))
(format t "  closed param types OK~%")

(defun with-daemon (fn)
  (let* ((sock (format nil "/tmp/clpm-rb-methods-~A.sock" (random (expt 2 32))))
         (thread (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (clpm.repl:start-server :socket-path sock)
                      (error (c) (format *error-output* "daemon: ~A~%" c))))
                  :name "test-bridge-methods")))
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

;;; ----------------------------------------------------------------------------
;;; `methods' lists the public discovery surface, not protocol-only frames.

(format t "Test: `methods' RPC enumerates public methods~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (clpm.repl:send-request sock "methods"))
           (result (lookup resp "result"))
           (methods (array-items (lookup result "methods"))))
      (assert-true methods "no methods array in response: ~S" resp)
      (let ((names (loop for m in methods
                         collect (lookup m "name"))))
        (dolist (expected '("ping" "current-package" "set-package"
                            "interrupt" "reset" "describe"
                            "list-redefinitions" "methods" "help"))
          (assert-true (member expected names :test #'string=)
                       "missing method ~A; got ~S" expected names))
        (dolist (internal '("eval" "shutdown" "query-response"))
          (assert-true (not (member internal names :test #'string=))
                       "protocol-only method ~A leaked into methods: ~S"
                       internal names))))))
(format t "  enumeration OK~%")

;;; ----------------------------------------------------------------------------
;;; `help' returns long-form documentation for a method.

(format t "Test: `help' returns spec for a known method~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (clpm.repl:send-request
                  sock "help"
                  :params (list :object (list (cons "method" "gc")))))
           (result (lookup resp "result"))
           (method (lookup result "method")))
      (assert-true method "no method object: ~S" resp)
      (let ((name (lookup method "name"))
            (doc (lookup method "doc"))
            (params (array-items (lookup method "params"))))
        (assert-true (string= "gc" name) "wrong name: ~S" name)
        (assert-true (and (stringp doc) (search "full" doc))
                     "doc should mention 'full': ~S" doc)
        (assert-true (find "full" params
                           :test (lambda (s p) (string= s (lookup p "name"))))
                     "no `full' param in gc spec: ~S" params)))))
(with-daemon
  (lambda (sock)
    (dolist (internal '("eval" "shutdown" "query-response"))
      (let* ((resp (clpm.repl:send-request
                    sock "help"
                    :params (list :object (list (cons "method" internal)))))
             (err (lookup resp "error")))
        (assert-true err "help for ~A should fail: ~S" internal resp)
        (assert-true (search "unknown method" (lookup err "message"))
                     "wrong help error for ~A: ~S" internal err)))))
(format t "  help spec OK~%")

;;; ----------------------------------------------------------------------------
;;; `help' for an unknown method returns a protocol-error.

(format t "Test: `help' for unknown method returns protocol-error~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (clpm.repl:send-request
                  sock "help"
                  :params (list :object (list (cons "method" "nope-not-here")))))
           (err (lookup resp "error")))
      (assert-true err "expected error: ~S" resp)
      (assert-true (string= "protocol-error" (lookup err "code"))
                   "wrong error code: ~S" err))))
(format t "  unknown-method error OK~%")

;;; ----------------------------------------------------------------------------
;;; Method specs are constructors: params must be closed and typed.

(format t "Test: request params are decoded from method specs~%")
(with-daemon
  (lambda (sock)
    (let* ((extra (clpm.repl:send-request
                   sock "eval"
                   :params (list :object
                                 (list (cons "form" "(+ 1 2)")
                                       (cons "bogus" t)))))
           (extra-error (lookup extra "error")))
      (assert-true extra-error
                   "unknown eval param should be rejected: ~S"
                   extra)
      (assert-true (search "unknown param"
                           (lookup extra-error "message"))
                   "wrong unknown-param message: ~S"
                   extra-error))
    (let* ((typed (clpm.repl:send-request
                   sock "eval"
                   :params (list :object
                                 (list (cons "form" 42)))))
           (typed-error (lookup typed "error")))
      (assert-true typed-error
                   "wrong-typed eval param should be rejected: ~S"
                   typed)
      (assert-true (search "expected string"
                           (lookup typed-error "message"))
                   "wrong type message: ~S"
                   typed-error))
    (let* ((null-bool (clpm.repl:send-request
                       sock "gc"
                       :params (list :object
                                     (list (cons "full" nil)))))
           (null-bool-error (lookup null-bool "error")))
      (assert-true null-bool-error
                   "null boolean param should be rejected: ~S"
                   null-bool)
      (assert-true (search "expected boolean"
                           (lookup null-bool-error "message"))
                   "wrong null-boolean message: ~S"
                   null-bool-error))
    (let* ((not-object (clpm.repl:send-request
                        sock "help"
                        :params (list :array nil)))
           (not-object-error (lookup not-object "error")))
      (assert-true not-object-error
                   "non-object params should be rejected: ~S"
                   not-object)
      (assert-true (search "params must be an object"
                           (lookup not-object-error "message"))
                   "wrong non-object params message: ~S"
                   not-object-error))))
(format t "  schema decode OK~%")

(format t "~%REPL methods tests PASSED!~%")
(sb-ext:exit :code 0)
