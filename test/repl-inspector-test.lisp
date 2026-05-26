;;;; test/repl-inspector-test.lisp - inspector sessions.
;;;;
;;;; Covers BRIDGE_V2 #120 (inspect FORM), #121 (inspect-into / pop),
;;;; #122 (inspect-eval), #123 (inspect-mutate), #124 (pagination),
;;;; #125 (inspect-close).

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
    (fail "expected ~S, got ~S" expected actual)))

(defun lookup (object key)
  (when (and (consp object) (eq (car object) :object))
    (cdr (assoc key (cadr object) :test #'string=))))

(defun array-items (a)
  (when (and (consp a) (eq (car a) :array))
    (cadr a)))

(defun with-daemon (fn)
  (let* ((sock (format nil "/tmp/clpm-rb-ins-~A.sock" (random (expt 2 32))))
         (thread (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (clpm.repl:start-server :socket-path sock)
                      (error (c) (format *error-output* "daemon: ~A~%" c))))
                  :name "test-bridge-ins")))
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

(defun do-rpc (sock method params)
  (clpm.repl:send-request sock method
                                  :params (list :object params)))

;;; ----------------------------------------------------------------------------
;;; #120: inspect FORM returns parts for a list.

(format t "Test: inspect a 5-element list~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (do-rpc sock "inspect"
                          (list (cons "form" "(list 10 20 30 40 50)"))))
           (result (lookup resp "result"))
           (parts (array-items (lookup result "parts"))))
      (assert-true result "no result: ~S" resp)
      (assert-true (= 5 (length parts))
                   "expected 5 parts, got ~A" (length parts))
      (assert-equal-string "10" (lookup (first parts) "repr"))
      (assert-equal-string "50" (lookup (fifth parts) "repr")))))
(format t "  inspect list OK~%")

(format t "Test: inspect circular list does not hang~%")
(with-daemon
  (lambda (sock)
    (let ((resp nil)
          (done nil)
          (worker nil))
      (unwind-protect
           (progn
             (setf worker
                   (sb-thread:make-thread
                    (lambda ()
                      (setf resp
                            (do-rpc sock "inspect"
                                    (list (cons "form"
                                                "(let ((x (list :a :b)))
                                                   (setf (cdr (last x)) x)
                                                   x)")))
                            done t))
                    :name "test-inspect-circular"))
             (loop for i from 0 below 20
                   while (not done)
                   do (sleep 0.1))
             (unless done
               (fail "circular list inspection hung"))
             (let ((result (lookup resp "result")))
               (assert-true result "expected inspect result, got ~S" resp)
               (assert-equal-string "cons" (lookup result "kind"))))
        (when (and worker (sb-thread:thread-alive-p worker))
          (ignore-errors (sb-thread:terminate-thread worker)))))))
(format t "  circular list OK~%")

;;; ----------------------------------------------------------------------------
;;; #121: inspect-into descends; inspect-pop restores.

(format t "Test: inspect-into / inspect-pop~%")
(with-daemon
  (lambda (sock)
    (let* ((initial (do-rpc sock "inspect"
                             (list (cons "form" "(list (list :a :b) :c)"))))
           (sid (lookup (lookup initial "result") "session"))
           (into (do-rpc sock "inspect-into"
                          (list (cons "session" sid)
                                (cons "i" 0))))
           (in-result (lookup into "result"))
           (in-parts (array-items (lookup in-result "parts"))))
      (assert-equal-string "(:A :B)" (lookup in-result "value_repr"))
      (assert-true (= 2 (length in-parts))
                   "inner expected 2 parts, got ~A" (length in-parts))
      ;; Pop back; verify we're at the outer list again.
      (let* ((popped (do-rpc sock "inspect-pop" (list (cons "session" sid))))
             (po-result (lookup popped "result")))
        (assert-true (search "C" (lookup po-result "value_repr"))
                     "after pop, expected outer list focus, got ~S"
                     (lookup po-result "value_repr"))))))
(format t "  into/pop OK~%")

(format t "Test: inspect-into follows displayed proper-list indices~%")
(with-daemon
  (lambda (sock)
    (let* ((initial (do-rpc sock "inspect"
                             (list (cons "form" "(list :a :b :c)"))))
           (sid (lookup (lookup initial "result") "session"))
           (into (do-rpc sock "inspect-into"
                          (list (cons "session" sid)
                                (cons "i" 2))))
           (result (lookup into "result")))
      (assert-true result
                   "inspect-into should accept displayed list index 2: ~S"
                   into)
      (assert-equal-string ":C" (lookup result "value_repr")))))
(format t "  proper-list into OK~%")

;;; ----------------------------------------------------------------------------
;;; #122: inspect-eval gets * bound to focus.

(format t "Test: inspect-eval reads * = focus~%")
(with-daemon
  (lambda (sock)
    (let* ((init (do-rpc sock "inspect"
                          (list (cons "form" "(list 5 6 7)"))))
           (sid (lookup (lookup init "result") "session"))
           (resp (do-rpc sock "inspect-eval"
                          (list (cons "session" sid)
                                (cons "form" "(length *)"))))
           (vr (lookup (lookup resp "result") "value_repr")))
      (assert-equal-string "3" vr))))
(format t "  inspect-eval OK~%")

;;; ----------------------------------------------------------------------------
;;; #123: inspect-mutate replaces a vector element.

(format t "Test: inspect-mutate on a vector~%")
(with-daemon
  (lambda (sock)
    (let* ((init (do-rpc sock "inspect"
                          (list (cons "form" "(make-array 3 :initial-element 0)")
                                (cons "mutable" t))))
           (sid (lookup (lookup init "result") "session")))
      (do-rpc sock "inspect-mutate"
              (list (cons "session" sid)
                    (cons "i" 1)
                    (cons "form" "42")))
      (let* ((view (do-rpc sock "inspect-pop"
                            ;; pop is a no-op when stack is len 1, so use it
                            ;; just to re-fetch the rendered view.
                            (list (cons "session" sid))))
             (parts (array-items
                     (lookup (lookup view "result") "parts"))))
        (assert-equal-string "42" (lookup (second parts) "repr"))))))
(format t "  mutate OK~%")

(format t "Test: inspect-mutate follows displayed proper-list indices~%")
(with-daemon
  (lambda (sock)
    (let* ((init (do-rpc sock "inspect"
                          (list (cons "form" "(list :a :b :c)")
                                (cons "mutable" t))))
           (sid (lookup (lookup init "result") "session"))
           (mutate (do-rpc sock "inspect-mutate"
                           (list (cons "session" sid)
                                 (cons "i" 2)
                                 (cons "form" ":z")))))
      (assert-true (lookup mutate "result")
                   "inspect-mutate should accept displayed list index 2: ~S"
                   mutate)
      (let* ((parts (array-items (lookup (lookup mutate "result") "parts"))))
        (assert-equal-string ":Z" (lookup (third parts) "repr"))))))
(format t "  proper-list mutate OK~%")

(format t "Test: inspect hash-table indices are stable and support NIL keys~%")
(with-daemon
  (lambda (sock)
    (let* ((init (do-rpc sock "inspect"
                          (list (cons "form"
                                      "(let ((h (make-hash-table)))
                                         (setf (gethash nil h) :nil-value
                                               (gethash :other h) :other-value)
                                         h)")
                                (cons "mutable" t))))
           (sid (lookup (lookup init "result") "session"))
           (parts (array-items (lookup (lookup init "result") "parts")))
           (nil-part (find "NIL" parts
                           :key (lambda (part) (lookup part "label"))
                           :test #'string=)))
      (assert-true nil-part "hash inspector did not render NIL key: ~S" parts)
      (let* ((index (lookup nil-part "i"))
             (mutate (do-rpc sock "inspect-mutate"
                             (list (cons "session" sid)
                                   (cons "i" index)
                                   (cons "form" "99"))))
             (new-parts (array-items (lookup (lookup mutate "result")
                                             "parts")))
             (new-nil-part (find "NIL" new-parts
                                 :key (lambda (part) (lookup part "label"))
                                 :test #'string=)))
        (assert-true (lookup mutate "result")
                     "hash-table mutate failed: ~S" mutate)
        (assert-equal-string "99" (lookup new-nil-part "repr"))))))
(format t "  hash-table NIL-key mutate OK~%")

(format t "Test: inspect-mutate reads forms in the current package~%")
(with-daemon
  (lambda (sock)
    (dolist (form '("(defpackage :rb-inspector-package (:use :cl))"
                   "(in-package :rb-inspector-package)"
                   "(defparameter replacement 77)"))
      (let ((resp (do-rpc sock "eval" (list (cons "form" form)))))
        (assert-true (lookup resp "result")
                     "setup failed for ~S: ~S" form resp)))
    (let* ((init (do-rpc sock "inspect"
                          (list (cons "form" "(vector 1 2 3)")
                                (cons "mutable" t))))
           (sid (lookup (lookup init "result") "session")))
      (let ((mutate (do-rpc sock "inspect-mutate"
                            (list (cons "session" sid)
                                  (cons "i" 1)
                                  (cons "form" "replacement")))))
        (assert-true (lookup mutate "result")
                     "inspect-mutate should resolve REPLACEMENT: ~S"
                     mutate))
      (let* ((view (do-rpc sock "inspect-pop"
                            (list (cons "session" sid))))
             (parts (array-items (lookup (lookup view "result") "parts"))))
        (assert-equal-string "77" (lookup (second parts) "repr"))))))
(format t "  mutate package OK~%")

(format t "Test: inspect mutable=false rejects mutation~%")
(with-daemon
  (lambda (sock)
    (let* ((init (do-rpc sock "inspect"
                          (list (cons "form" "(vector 1 2 3)")
                                (cons "mutable" :false))))
           (sid (lookup (lookup init "result") "session"))
           (mutate (do-rpc sock "inspect-mutate"
                           (list (cons "session" sid)
                                 (cons "i" 1)
                                 (cons "form" "42")))))
      (assert-true (lookup mutate "error")
                   "mutable=false should reject mutation: ~S" mutate))))
(format t "  mutable=false OK~%")

;;; ----------------------------------------------------------------------------
;;; #124: pagination.

(format t "Test: inspect-page on a 250-element list~%")
(with-daemon
  (lambda (sock)
    (let* ((init (do-rpc sock "inspect"
                          (list (cons "form"
                                      "(loop for i from 0 below 250 collect i)"))))
           (sid (lookup (lookup init "result") "session"))
           (first-result (lookup init "result")))
      (assert-true (= 250 (lookup first-result "total")) "total mismatch")
      (assert-true (= 100 (length (array-items
                                    (lookup first-result "parts"))))
                   "first page should be 100")
      (let* ((paged (do-rpc sock "inspect-page"
                             (list (cons "session" sid)
                                   (cons "offset" 100))))
             (pr (lookup paged "result"))
             (parts (array-items (lookup pr "parts"))))
        (assert-equal-string "100" (lookup (first parts) "repr"))))))
(format t "  pagination OK~%")

;;; ----------------------------------------------------------------------------
;;; #125: inspect-close drops the session.

(format t "Test: inspect-close~%")
(with-daemon
  (lambda (sock)
    (let* ((init (do-rpc sock "inspect"
                          (list (cons "form" "(list 1 2 3)"))))
           (sid (lookup (lookup init "result") "session")))
      (do-rpc sock "inspect-close" (list (cons "session" sid)))
      (let ((after (do-rpc sock "inspect-into"
                            (list (cons "session" sid) (cons "i" 0)))))
        (assert-true (lookup after "error") "expected error after close")))))
(format t "  close OK~%")

(format t "~%REPL inspector tests PASSED!~%")
(sb-ext:exit :code 0)
