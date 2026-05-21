;;;; test/repl-condition-test.lisp - rich condition serialization.
;;;;
;;;; BRIDGE_V2 #110 acceptance: a known-shape condition (simple-type-error)
;;;; round-trips its slots; `interactive' reflects whether the restart was
;;;; created with `:interactive-function'; `args_arity' is computed from
;;;; that function or defaults conservatively.

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
  (let* ((sock (format nil "/tmp/clpm-rb-cond-~A.sock" (random (expt 2 32))))
         (thread (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (clpm.repl:start-server :socket-path sock)
                      (error (c) (format *error-output* "daemon: ~A~%" c))))
                  :name "test-bridge-cond")))
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

(defun do-eval (sock form)
  (clpm.repl:send-request
   sock "eval"
   :params (list :object (list (cons "form" form)))))

;;; ----------------------------------------------------------------------------
;;; Slot capture: simple-type-error's expected-type and datum round-trip.

(format t "Test: simple-type-error slot values round-trip~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (do-eval sock
                  "(error 'simple-type-error
                          :format-control \"need integer\"
                          :format-arguments nil
                          :datum :hello
                          :expected-type 'integer)"))
           (err (lookup resp "error"))
           (details (lookup err "details"))
           (conds (and details (array-items (lookup details "conditions"))))
           (c0 (and conds (first conds)))
           (slots (and c0 (lookup c0 "slot_values"))))
      (assert-true c0 "no condition: ~S" resp)
      (let ((datum (lookup slots "DATUM"))
            (expected (lookup slots "EXPECTED-TYPE")))
        (assert-true (and (stringp datum)
                          (search "HELLO" datum :test #'char-equal))
                     "expected :HELLO in DATUM, got ~S" datum)
        (assert-true (and (stringp expected)
                          (search "INTEGER" expected :test #'char-equal))
                     "expected INTEGER in EXPECTED-TYPE, got ~S" expected)))))
(format t "  slot round-trip OK~%")

;;; ----------------------------------------------------------------------------
;;; Restart objects are full {name, report, interactive, args_arity} forms.

(format t "Test: restart shape includes report and arity~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (do-eval sock
                  "(restart-case
                       (error \"boom\")
                     (use-value (v) v)
                     (custom () :custom))"))
           (err (lookup resp "error"))
           (conds (array-items (lookup (lookup err "details") "conditions")))
           (c0 (first conds))
           (restarts (array-items (lookup c0 "restarts"))))
      (let ((use-value (find "USE-VALUE" restarts
                              :test (lambda (s r)
                                      (string= s (lookup r "name")))))
            (custom (find "CUSTOM" restarts
                           :test (lambda (s r)
                                   (string= s (lookup r "name"))))))
        (assert-true use-value "no USE-VALUE restart: ~S" restarts)
        (assert-true custom "no CUSTOM restart: ~S" restarts)
        ;; USE-VALUE has no :interactive-function in this form -> false, but
        ;; the fallback heuristic gives it arity 1 from the name convention.
        (assert-true (= 1 (lookup use-value "args_arity"))
                     "expected USE-VALUE arity 1, got ~S"
                     (lookup use-value "args_arity"))
        (assert-true (= 0 (lookup custom "args_arity"))
                     "expected CUSTOM arity 0, got ~S"
                     (lookup custom "args_arity"))
        (assert-true (and (stringp (lookup use-value "report"))
                          (plusp (length (lookup use-value "report"))))
                     "USE-VALUE missing report")))))
(format t "  rich restart shape OK~%")

;;; ----------------------------------------------------------------------------
;;; Backtrace is a list of frame objects (i, name, args, source, vars),
;;; not a list of strings.

(format t "Test: backtrace is structured~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (do-eval sock "(error \"boom\")"))
           (err (lookup resp "error"))
           (conds (array-items (lookup (lookup err "details") "conditions")))
           (c0 (first conds))
           (frames (array-items (lookup c0 "backtrace"))))
      (assert-true (and (listp frames) (plusp (length frames)))
                   "expected non-empty frames, got ~S" frames)
      (let ((top (first frames)))
        (assert-true (and (consp top) (eq (car top) :object))
                     "frames should be JSON objects, got ~S" top)
        (assert-true (integerp (lookup top "i"))
                     "frame missing index, got ~S" top)
        (assert-true (stringp (lookup top "name"))
                     "frame missing name, got ~S" top)))))
(format t "  structured backtrace OK~%")

(format t "~%REPL condition tests PASSED!~%")
(sb-ext:exit :code 0)
