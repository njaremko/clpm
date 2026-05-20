;;;; test/repl-bridge-workers-test.lisp - named / concurrent workers.
;;;;
;;;; Covers BRIDGE_V2 #170 (named workers), #171 (list-workers),
;;;; #172 (kill-worker), #173 (eval --concurrent).

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
  (let* ((sock (format nil "/tmp/clpm-rb-workers-~A.sock" (random (expt 2 32))))
         (thread (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (clpm.repl-bridge:start-server :socket-path sock)
                      (error (c) (format *error-output* "daemon: ~A~%" c))))
                  :name "test-bridge-workers")))
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

(defun do-rpc (sock method &optional params)
  (clpm.repl-bridge:send-request sock method
                                  :params (and params (list :object params))))

;;; ----------------------------------------------------------------------------
;;; #170: named workers isolate package state.

(format t "Test: two named workers keep independent *package*~%")
(with-daemon
  (lambda (sock)
    ;; Worker A: switch into CL.
    (do-rpc sock "eval"
            (list (cons "form" "(in-package :common-lisp)")
                  (cons "worker" "alpha")))
    ;; Worker B: switch into KEYWORD.
    (do-rpc sock "eval"
            (list (cons "form" "(in-package :keyword)")
                  (cons "worker" "beta")))
    (let* ((a (do-rpc sock "current-package"
                      (list (cons "worker" "alpha"))))
           (b (do-rpc sock "current-package"
                      (list (cons "worker" "beta")))))
      (assert-equal-string "COMMON-LISP"
                           (lookup (lookup a "result") "package"))
      (assert-equal-string "KEYWORD"
                           (lookup (lookup b "result") "package")))))
(format t "  named-worker isolation OK~%")

;;; ----------------------------------------------------------------------------
;;; #170: named workers don't pollute the default worker.

(format t "Test: default worker package unaffected by named worker~%")
(with-daemon
  (lambda (sock)
    (do-rpc sock "eval"
            (list (cons "form" "(in-package :keyword)")
                  (cons "worker" "scratch")))
    (let ((d (do-rpc sock "current-package" nil)))
      (assert-equal-string "COMMON-LISP-USER"
                           (lookup (lookup d "result") "package")))))
(format t "  default isolation OK~%")

;;; ----------------------------------------------------------------------------
;;; #171: list-workers reports the workers we created.

(format t "Test: list-workers shows default + alpha~%")
(with-daemon
  (lambda (sock)
    (do-rpc sock "eval"
            (list (cons "form" "(+ 1 2)")
                  (cons "worker" "alpha")))
    (let* ((resp (do-rpc sock "list-workers"))
           (entries (array-items (lookup (lookup resp "result") "entries")))
           (names (mapcar (lambda (e) (lookup e "name")) entries)))
      (assert-true (find "default" names :test #'string=)
                   "default worker missing: ~S" names)
      (assert-true (find "alpha" names :test #'string=)
                   "alpha worker missing: ~S" names)
      (let ((alpha-entry (find "alpha" entries
                               :test (lambda (s e) (string= s (lookup e "name"))))))
        (assert-equal-string "idle" (lookup alpha-entry "state"))
        (assert-true (integerp (lookup alpha-entry "age_seconds"))
                     "age_seconds not integer")))))
(format t "  list-workers OK~%")

;;; ----------------------------------------------------------------------------
;;; #172: kill-worker drops a named worker; default is protected.

(format t "Test: kill-worker removes named, refuses default~%")
(with-daemon
  (lambda (sock)
    ;; Create then kill.
    (do-rpc sock "eval"
            (list (cons "form" "(+ 1 2)")
                  (cons "worker" "ephemeral")))
    (let ((k (do-rpc sock "kill-worker"
                     (list (cons "name" "ephemeral")))))
      (assert-true (lookup (lookup k "result") "killed")
                   "kill-worker should report killed=t: ~S" k))
    ;; Default is protected.
    (let ((bad (do-rpc sock "kill-worker"
                       (list (cons "name" "default")))))
      (assert-true (lookup bad "error")
                   "killing default should error: ~S" bad))
    ;; Idempotency: killing a missing worker returns killed=NIL but no error.
    (let ((idem (do-rpc sock "kill-worker"
                        (list (cons "name" "never-existed")))))
      (assert-true (not (lookup (lookup idem "result") "killed"))
                   "missing worker should report killed=NIL: ~S" idem))))
(format t "  kill-worker OK~%")

;;; ----------------------------------------------------------------------------
;;; #173: --concurrent runs on a one-shot worker that's gone afterward.

(format t "Test: eval --concurrent doesn't persist a worker~%")
(with-daemon
  (lambda (sock)
    (let* ((before (do-rpc sock "list-workers"))
           (before-count (length (array-items
                                  (lookup (lookup before "result") "entries"))))
           (resp (do-rpc sock "eval"
                         (list (cons "form" "(+ 100 200)")
                               (cons "concurrent" t))))
           (val (lookup (lookup resp "result") "value")))
      (assert-equal-string "300" val)
      ;; Concurrent workers self-destruct after the eval.
      (let* ((after (do-rpc sock "list-workers"))
             (after-entries (array-items (lookup (lookup after "result") "entries")))
             (after-count (length after-entries))
             (concurrent-names
               (remove-if-not
                (lambda (e)
                  (let ((n (lookup e "name")))
                    (and (stringp n)
                         (>= (length n) (length "$concurrent-"))
                         (string= n "$concurrent-"
                                  :end1 (length "$concurrent-")))))
                after-entries)))
        (assert-true (null concurrent-names)
                     "concurrent worker should have been cleaned up: ~S"
                     (mapcar (lambda (e) (lookup e "name")) after-entries))
        ;; Worker count should be back to (or below) the pre-call value.
        (assert-true (<= after-count (max before-count 1))
                     "worker count grew unexpectedly: ~A -> ~A"
                     before-count after-count)))))
(format t "  --concurrent OK~%")

;;; ----------------------------------------------------------------------------
;;; #170: redefinition log is per-worker.

(format t "Test: redefinitions are scoped per worker~%")
(with-daemon
  (lambda (sock)
    (do-rpc sock "eval"
            (list (cons "form" "(defun worker-a-fn () 1)")
                  (cons "worker" "alpha")))
    (do-rpc sock "eval"
            (list (cons "form" "(defun worker-b-fn () 2)")
                  (cons "worker" "beta")))
    (let* ((a (do-rpc sock "list-redefinitions"
                      (list (cons "worker" "alpha"))))
           (a-entries (array-items (lookup (lookup a "result") "entries")))
           (a-names (mapcar (lambda (e) (lookup e "name")) a-entries))
           (b (do-rpc sock "list-redefinitions"
                      (list (cons "worker" "beta"))))
           (b-entries (array-items (lookup (lookup b "result") "entries")))
           (b-names (mapcar (lambda (e) (lookup e "name")) b-entries)))
      (assert-true (find "WORKER-A-FN" a-names :test #'string=)
                   "alpha log missing WORKER-A-FN: ~S" a-names)
      (assert-true (not (find "WORKER-B-FN" a-names :test #'string=))
                   "alpha log should not contain WORKER-B-FN: ~S" a-names)
      (assert-true (find "WORKER-B-FN" b-names :test #'string=)
                   "beta log missing WORKER-B-FN: ~S" b-names))))
(format t "  per-worker redefinitions OK~%")

(format t "~%REPL-bridge workers tests PASSED!~%")
(sb-ext:exit :code 0)
