;;;; test/repl-bridge-v2-eval-test.lisp - v2 eval payload semantics.
;;;;
;;;; Covers BRIDGE_V2 #150-#155:
;;;;   - Multiple values returned as a JSON array (with v1-compat scalar)
;;;;   - REPL history bindings (`*`, `**`, `***`, `+`, `++`, `+++`, `/`, `//`, `///`)
;;;;     are updated and snapshotted into the response
;;;;   - Defensive prin1: a print-object that signals an error is rendered as
;;;;     `#<unprintable ...>` instead of bringing down the eval
;;;;   - record_signals: t captures non-error conditions
;;;;   - print_length and print_level bind during prin1 of returned values

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

(defun assert-equal-string (expected actual)
  (unless (and (stringp actual) (string= expected actual))
    (fail "expected string ~S, got ~S" expected actual)))

(defun lookup (object key)
  (when (and (consp object) (eq (car object) :object))
    (cdr (assoc key (cadr object) :test #'string=))))

(defun array-items (a)
  (when (and (consp a) (eq (car a) :array))
    (cadr a)))

(defun with-daemon (fn)
  (let* ((sock (format nil "/tmp/clpm-rb-v2-~A.sock" (random (expt 2 32))))
         (thread (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (clpm.repl-bridge:start-server :socket-path sock)
                      (error (c) (format *error-output* "daemon: ~A~%" c))))
                  :name "test-bridge-v2")))
    (unwind-protect
         (progn
           (loop for i from 0 below 50
                 while (not (probe-file sock))
                 do (sleep 0.05))
           (assert-true (probe-file sock) "daemon never started")
           (funcall fn sock))
      (handler-case (clpm.repl-bridge:send-request sock "shutdown")
        (error () nil))
      (loop for i from 0 below 30
            while (sb-thread:thread-alive-p thread)
            do (sleep 0.05))
      (when (sb-thread:thread-alive-p thread)
        (ignore-errors (sb-thread:terminate-thread thread)))
      (ignore-errors (delete-file sock)))))

(defun eval-form (sock form &rest extra)
  "Helper: build an eval params object with FORM and any extra k/v pairs."
  (let ((params
          (list :object
                (append (list (cons "form" form))
                        (loop for (k v) on extra by #'cddr
                              collect (cons k v))))))
    (clpm.repl-bridge:send-request sock "eval"
                                    :params params)))

;;; ----------------------------------------------------------------------------
;;; Multiple values: `values' is the array, `value' aliases the primary.

(format t "Test: multiple values shipped as a JSON array~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (eval-form sock "(values 1 2 3)"))
           (result (lookup resp "result")))
      (assert-true result "no result frame: ~S" resp)
      (let ((values (array-items (lookup result "values"))))
        (assert-true (equal '("1" "2" "3") values)
                     "expected (\"1\" \"2\" \"3\"), got ~S" values))
      (assert-equal-string "1" (lookup result "value")))))
(format t "  multiple values OK~%")

(format t "Test: (values) returns an empty array, and value = nil~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (eval-form sock "(values)"))
           (result (lookup resp "result")))
      (assert-true result "no result frame: ~S" resp)
      (let ((values (array-items (lookup result "values"))))
        (assert-true (null values)
                     "expected empty values array, got ~S" values))
      (assert-true (null (lookup result "value"))
                   "expected value=nil, got ~S" (lookup result "value")))))
(format t "  zero values OK~%")

;;; ----------------------------------------------------------------------------
;;; History: every eval updates *, **, +, / etc.; the response includes a
;;; snapshot of the bindings.

(format t "Test: history bindings update across evals~%")
(with-daemon
  (lambda (sock)
    (eval-form sock "(+ 1 2)")
    (eval-form sock "(* 6 7)")
    (let* ((resp (eval-form sock "(values *)"))
           (result (lookup resp "result")))
      ;; (values *) reads * at eval time -- which is 42, the primary value
      ;; of the previous eval (* 6 7).
      (assert-equal-string "42" (lookup result "value"))
      ;; The history snapshot is taken *after* %update-history! has shifted
      ;; the bindings. So post-shift: * = 42 (this eval), ** = 42 (prior *),
      ;; *** = 3 (prior **).
      (let* ((history (lookup result "history"))
             (star (and history (lookup history "*")))
             (star-star (and history (lookup history "**")))
             (star-star-star (and history (lookup history "***")))
             (plus (and history (lookup history "+"))))
        (assert-true history "history not included in payload")
        (assert-equal-string "42" star)
        (assert-equal-string "42" star-star)
        (assert-equal-string "3" star-star-star)
        ;; `+' is the last form just evaluated.
        (assert-true (and (stringp plus)
                          (search "VALUES" plus :test #'char-equal))
                     "expected `+' to mention VALUES, got ~S" plus)))))
(format t "  history bindings OK~%")

;;; ----------------------------------------------------------------------------
;;; Defensive prin1: an evil print-object method does not crash the response.

(format t "Test: unprintable values render with a fallback string~%")
(with-daemon
  (lambda (sock)
    (eval-form sock
               "(progn
                  (defclass evil () ())
                  (defmethod print-object ((o evil) s)
                    (declare (ignore s))
                    (error \"intentional print failure\")))")
    (let* ((resp (eval-form sock "(make-instance 'evil)"))
           (result (lookup resp "result")))
      (assert-true result "no result frame: ~S" resp)
      (let ((v (lookup result "value")))
        (assert-true (and (stringp v)
                          (search "unprintable" v :test #'char-equal))
                     "expected unprintable fallback, got ~S" v)))))
(format t "  defensive prin1 OK~%")

;;; ----------------------------------------------------------------------------
;;; record_signals: t captures non-error conditions that the form chose
;;; to handle internally.

(format t "Test: record_signals captures non-error signaled conditions~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (eval-form
                  sock
                  ;; `signal' on a SIMPLE-WARNING with no handler in the user's
                  ;; form falls through to our handler-bind, which logs it.
                  ;; The form's value is :DONE because signal returns NIL when
                  ;; nothing transfers control.
                  "(progn (signal 'simple-warning :format-control \"hi\")
                          :done)"
                  "record_signals" t))
           (result (lookup resp "result")))
      (assert-true result "no result frame: ~S" resp)
      (assert-equal-string ":DONE" (lookup result "value"))
      (let ((sigs (array-items (lookup result "signaled_conditions"))))
        (assert-true sigs "expected at least one signaled condition")
        (let* ((first (first sigs))
               (type (and first (lookup first "type"))))
          (assert-true (and (stringp type)
                            (search "WARNING" type :test #'char-equal))
                       "expected a warning type, got ~S" type))))))
(format t "  record_signals OK~%")

;;; ----------------------------------------------------------------------------
;;; print_length: capping list-element prin1 affects returned values.

(format t "Test: print_length bound during result prin1~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (eval-form sock
                            "(loop for i from 0 below 50 collect i)"
                            "print_length" 3))
           (result (lookup resp "result"))
           (v (lookup result "value")))
      (assert-true (and (stringp v) (search "..." v))
                   "expected `...' truncation marker, got ~S" v))))
(format t "  print_length OK~%")

(format t "~%REPL-bridge v2 eval tests PASSED!~%")
(sb-ext:exit :code 0)
