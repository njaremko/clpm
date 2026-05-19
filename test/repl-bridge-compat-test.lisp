;;;; test/repl-bridge-compat-test.lisp - cross-impl primitives
;;;;
;;;; The bridge is portable across SBCL/CCL/ECL via clpm.repl-bridge.compat.
;;;; The end-to-end bridge tests already exercise the SBCL paths heavily.
;;;; This file targets the compat layer itself so the SBCL backend's
;;;; thread+mutex+mailbox+pid behavior is checked in isolation, and so a
;;;; future CCL/ECL contributor has a clearly-scoped regression suite to
;;;; port over -- not a multi-thousand-line bridge-and-protocol test.

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

(defun assert-eql (expected actual)
  (unless (eql expected actual)
    (fail "expected ~S, got ~S" expected actual)))

;;; ----------------------------------------------------------------------------
;;; host-impl returns a recognized keyword for the current impl.

(format t "Test: host-impl identifies the running Lisp~%")
(let ((impl (clpm.repl-bridge.compat:host-impl)))
  (assert-true (member impl '(:sbcl :ccl :ecl))
               "host-impl returned ~S; bridge has only been validated on those three"
               impl))
(format t "  host-impl OK (~A)~%" (clpm.repl-bridge.compat:host-impl))

;;; ----------------------------------------------------------------------------
;;; getpid returns a positive integer.

(format t "Test: getpid returns the current pid~%")
(let ((pid (clpm.repl-bridge.compat:getpid)))
  (assert-true (and (integerp pid) (plusp pid))
               "getpid returned ~S, expected a positive integer" pid))
(format t "  getpid OK~%")

;;; ----------------------------------------------------------------------------
;;; make-thread/join-thread round-trip + thread-alive-p transitions.

(format t "Test: make-thread + join-thread~%")
(let* ((flag nil)
       (th (clpm.repl-bridge.compat:make-thread
            (lambda () (setf flag :ran))
            :name "compat-test-1")))
  (clpm.repl-bridge.compat:join-thread th)
  (assert-eql :ran flag)
  (assert-true (not (clpm.repl-bridge.compat:thread-alive-p th))
               "thread reported alive after join"))
(format t "  make-thread + join OK~%")

;;; ----------------------------------------------------------------------------
;;; interrupt-thread: the interrupt body runs in the worker context.

(format t "Test: interrupt-thread delivers a signal~%")
(let* ((latch (make-array 1 :initial-element nil))
       (th (clpm.repl-bridge.compat:make-thread
            (lambda ()
              (handler-case
                  (loop (sleep 0.05))
                (error (c) (setf (aref latch 0) c))))
            :name "compat-test-interrupt")))
  ;; Give the worker a moment to settle into its sleep loop.
  (sleep 0.1)
  (clpm.repl-bridge.compat:interrupt-thread
   th (lambda () (error "compat interrupt fired")))
  (loop for i from 0 below 50
        while (clpm.repl-bridge.compat:thread-alive-p th)
        do (sleep 0.05))
  (assert-true (not (clpm.repl-bridge.compat:thread-alive-p th))
               "thread survived the interrupt")
  (assert-true (aref latch 0) "interrupt body did not run"))
(format t "  interrupt-thread OK~%")

;;; ----------------------------------------------------------------------------
;;; terminate-thread kills a runaway thread.

(format t "Test: terminate-thread stops a runaway thread~%")
(let ((th (clpm.repl-bridge.compat:make-thread
           (lambda () (loop (sleep 0.05)))
           :name "compat-test-term")))
  (sleep 0.05)
  (assert-true (clpm.repl-bridge.compat:thread-alive-p th)
               "thread died unexpectedly before termination")
  (clpm.repl-bridge.compat:terminate-thread th)
  (loop for i from 0 below 50
        while (clpm.repl-bridge.compat:thread-alive-p th)
        do (sleep 0.05))
  (assert-true (not (clpm.repl-bridge.compat:thread-alive-p th))
               "thread did not die after terminate-thread"))
(format t "  terminate-thread OK~%")

;;; ----------------------------------------------------------------------------
;;; Mutex: with-mutex serializes access.

(format t "Test: with-mutex serializes concurrent updates~%")
(let* ((mut (clpm.repl-bridge.compat:make-mutex :name "compat-test"))
       (counter 0)
       (n 8)
       (per-thread 1000)
       (threads
         (loop repeat n
               collect (clpm.repl-bridge.compat:make-thread
                        (lambda ()
                          (loop repeat per-thread do
                            (clpm.repl-bridge.compat:with-mutex (mut)
                              (incf counter))))
                        :name "compat-test-mutex"))))
  (dolist (th threads)
    (clpm.repl-bridge.compat:join-thread th))
  (assert-eql (* n per-thread) counter))
(format t "  with-mutex OK~%")

;;; ----------------------------------------------------------------------------
;;; Mailbox: send/receive with multiple producers + one consumer.

(format t "Test: mailbox send/receive preserves count and FIFO per-producer~%")
(let* ((box (clpm.repl-bridge.compat:make-mailbox))
       (producers 4)
       (per-producer 25)
       (total (* producers per-producer))
       (producer-threads
         (loop for p from 0 below producers
               collect (clpm.repl-bridge.compat:make-thread
                        (let ((pp p))
                          (lambda ()
                            (loop for j from 0 below per-producer do
                              (clpm.repl-bridge.compat:send-message
                               box (cons pp j)))))
                        :name (format nil "compat-test-mbox-prod-~D" p))))
       (received '()))
  (dolist (th producer-threads)
    (clpm.repl-bridge.compat:join-thread th))
  (dotimes (i total)
    (push (clpm.repl-bridge.compat:receive-message box) received))
  (assert-eql total (length received))
  ;; Per-producer FIFO: extract this producer's messages in arrival order and
  ;; assert they are strictly increasing.
  (dotimes (p producers)
    (let* ((mine (remove-if-not (lambda (m) (= (car m) p)) (reverse received)))
           (seqs (mapcar #'cdr mine)))
      (assert-eql per-producer (length seqs))
      (assert-true (equal seqs (sort (copy-seq seqs) #'<))
                   "producer ~D's messages arrived out of order: ~S" p seqs))))
(format t "  mailbox OK~%")

;;; ----------------------------------------------------------------------------
;;; list-backtrace runs without error. Content varies wildly between impls,
;;; so we only assert that the result is a (possibly empty) list of objects
;;; that can be PRINC'd.

(format t "Test: list-backtrace returns a printable list~%")
(let ((frames (clpm.repl-bridge.compat:list-backtrace)))
  (assert-true (listp frames) "list-backtrace did not return a list")
  (dolist (f frames)
    (princ-to-string f)))
(format t "  list-backtrace OK (~D frames)~%"
        (length (clpm.repl-bridge.compat:list-backtrace)))

(format t "~%REPL-bridge compat layer tests PASSED on ~A!~%"
        (clpm.repl-bridge.compat:host-impl))
(sb-ext:exit :code 0)
