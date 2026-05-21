;;;; test/repl-docs-test.lisp - discoverability + ergonomics.
;;;;
;;;; Covers BRIDGE_V2 #200 (methods), #201 (help METHOD), #205 (explain).

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

(defun with-daemon (fn)
  (let* ((sock (format nil "/tmp/clpm-rb-docs-~A.sock" (random (expt 2 32))))
         (thread (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (clpm.repl:start-server :socket-path sock)
                      (error (c) (format *error-output* "daemon: ~A~%" c))))
                  :name "test-bridge-docs")))
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

(defun do-rpc (sock method &optional params &key on-event)
  (clpm.repl:send-request sock method
                                  :params (and params (list :object params))
                                  :on-event on-event))

;;; ----------------------------------------------------------------------------
;;; #200: `methods' lists every registered RPC.

(format t "Test: methods includes eval + watch + list-workers~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (do-rpc sock "methods"))
           (entries (array-items (lookup (lookup resp "result") "methods")))
           (names (mapcar (lambda (e) (lookup e "name")) entries)))
      (dolist (m '("eval" "watch" "list-workers" "inspect"
                   "kill-worker" "unwatch" "image-info"))
        (assert-true (find m names :test #'string=)
                     "~A missing from methods: ~S" m names)))))
(format t "  methods OK~%")

;;; ----------------------------------------------------------------------------
;;; #201: `help eval' returns its doc + params.

(format t "Test: help eval has doc and param schema~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (do-rpc sock "help"
                          (list (cons "method" "eval"))))
           (spec (lookup (lookup resp "result") "method"))
           (params (array-items (lookup spec "params"))))
      (assert-true (string= "eval" (lookup spec "name"))
                   "wrong name in help: ~S" spec)
      (assert-true (and (stringp (lookup spec "doc"))
                        (plusp (length (lookup spec "doc"))))
                   "eval should have a docstring")
      (assert-true (find "form" params
                          :test (lambda (s p) (string= s (lookup p "name"))))
                   "form param missing"))))
(format t "  help OK~%")

;;; ----------------------------------------------------------------------------
;;; #205: explain emits a `plan' event before the handler runs.

(format t "Test: explain emits a plan event~%")
(with-daemon
  (lambda (sock)
    (let* ((events '())
           (resp (do-rpc sock "ping"
                          (list (cons "explain" t))
                          :on-event (lambda (frame)
                                      (push frame events)
                                      nil))))
      (assert-true (lookup resp "result") "ping should have succeeded")
      (let ((plan (find-if (lambda (f)
                             (string= (lookup f "event") "plan"))
                           events)))
        (assert-true plan "no plan event observed: ~S" events)
        (assert-true (string= "ping" (lookup plan "method"))
                     "plan should echo method: ~S" plan)))))
(format t "  explain OK~%")

(format t "~%REPL docs tests PASSED!~%")
(sb-ext:exit :code 0)
