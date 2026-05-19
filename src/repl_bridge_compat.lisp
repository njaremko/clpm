;;;; repl_bridge_compat.lisp - Cross-implementation threading / mailbox / pid
;;;;
;;;; The repl-bridge core needs threads, mutexes, a one-producer-one-consumer
;;;; mailbox, the current pid, and (optionally) a backtrace. Each Lisp ships
;;;; its own primitives:
;;;;
;;;;   SBCL: sb-thread, sb-concurrency, sb-posix, sb-debug
;;;;   CCL:  ccl: (process / lock); mailbox is hand-rolled on a lock + queue
;;;;   ECL:  mp:  (process / lock / queue); ext:getpid
;;;;
;;;; This module is the single seam: bridge code calls
;;;; `clpm.repl-bridge.compat:...' and never touches `sb-...' directly.
;;;;
;;;; The SBCL implementation is the reference; CCL and ECL implementations
;;;; mirror it function-for-function and depend on those impls' standard
;;;; thread libraries being present.

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (require :sb-bsd-sockets)
  #+sbcl (require :sb-concurrency)
  #+sbcl (require :sb-posix))

(in-package #:clpm.repl-bridge.compat)

(defun host-impl ()
  "Return a keyword naming the host Lisp implementation. Used by tests so
they can `skip with a clear message' on impls the bridge does not yet
exercise end-to-end."
  #+sbcl :sbcl
  #+ccl :ccl
  #+ecl :ecl
  #-(or sbcl ccl ecl) :unknown)

;;; --------------------------------------------------------------------------
;;; Threads
;;; --------------------------------------------------------------------------

(defun make-thread (function &key name)
  "Spawn a thread / process running FUNCTION with no arguments. Returns the
implementation-specific thread handle to be passed to the other helpers."
  #+sbcl (sb-thread:make-thread function :name name)
  #+ccl (ccl:process-run-function (or name "clpm.repl-bridge") function)
  #+ecl (mp:process-run-function (or name "clpm.repl-bridge") function)
  #-(or sbcl ccl ecl)
  (error "clpm.repl-bridge.compat:make-thread is not implemented on ~A"
         (lisp-implementation-type)))

(defun interrupt-thread (thread function)
  "Run FUNCTION in THREAD's context (asynchronously). Used to deliver the
worker-interrupt signal."
  #+sbcl (sb-thread:interrupt-thread thread function)
  #+ccl (ccl:process-interrupt thread function)
  #+ecl (mp:interrupt-process thread function)
  #-(or sbcl ccl ecl)
  (error "interrupt-thread not implemented on ~A" (lisp-implementation-type)))

(defun thread-alive-p (thread)
  "T while THREAD is still runnable."
  #+sbcl (sb-thread:thread-alive-p thread)
  #+ccl (not (ccl:process-exhausted-p thread))
  #+ecl (mp:process-active-p thread)
  #-(or sbcl ccl ecl) nil)

(defun terminate-thread (thread)
  "Forcibly stop THREAD. The thread's stack is unwound asynchronously."
  #+sbcl (sb-thread:terminate-thread thread)
  #+ccl (ccl:process-kill thread)
  #+ecl (mp:process-kill thread)
  #-(or sbcl ccl ecl)
  (error "terminate-thread not implemented on ~A" (lisp-implementation-type)))

(defun join-thread (thread)
  "Block until THREAD finishes. Returns its return value where supported,
ignored otherwise."
  #+sbcl (sb-thread:join-thread thread)
  #+ccl (ccl:join-process thread)
  #+ecl (mp:process-join thread)
  #-(or sbcl ccl ecl)
  (error "join-thread not implemented on ~A" (lisp-implementation-type)))

;;; --------------------------------------------------------------------------
;;; Mutexes
;;; --------------------------------------------------------------------------

(defun make-mutex (&key name)
  #+sbcl (sb-thread:make-mutex :name name)
  #+ccl (ccl:make-lock name)
  #+ecl (mp:make-lock :name name)
  #-(or sbcl ccl ecl) (cons :stub-mutex name))

(defun call-with-mutex (mutex fn)
  #+sbcl (sb-thread:with-mutex (mutex) (funcall fn))
  #+ccl (ccl:with-lock-grabbed (mutex) (funcall fn))
  #+ecl (mp:with-lock (mutex) (funcall fn))
  #-(or sbcl ccl ecl) (funcall fn))

(defmacro with-mutex ((mutex) &body body)
  `(call-with-mutex ,mutex (lambda () ,@body)))

;;; --------------------------------------------------------------------------
;;; Mailbox: a blocking FIFO of arbitrary messages. The worker thread pulls
;;; one job at a time off its mailbox; eval-dispatch posts a reply onto a
;;; per-request mailbox and blocks.
;;; --------------------------------------------------------------------------

#+sbcl
(progn
  (defun make-mailbox () (sb-concurrency:make-mailbox))
  (defun send-message (mailbox value)
    (sb-concurrency:send-message mailbox value))
  (defun receive-message (mailbox)
    (sb-concurrency:receive-message mailbox)))

;;; CCL: hand-rolled mailbox on a lock + condition variable.
#+ccl
(progn
  (defstruct (mailbox (:constructor %make-mailbox))
    (queue '() :type list)
    (lock (ccl:make-lock "clpm.repl-bridge.mbox"))
    (sem (ccl:make-semaphore)))

  (defun make-mailbox () (%make-mailbox))

  (defun send-message (mailbox value)
    (ccl:with-lock-grabbed ((mailbox-lock mailbox))
      (setf (mailbox-queue mailbox)
            (append (mailbox-queue mailbox) (list value))))
    (ccl:signal-semaphore (mailbox-sem mailbox))
    value)

  (defun receive-message (mailbox)
    (loop
      (ccl:wait-on-semaphore (mailbox-sem mailbox))
      (let ((value (ccl:with-lock-grabbed ((mailbox-lock mailbox))
                     (when (mailbox-queue mailbox)
                       (let ((v (first (mailbox-queue mailbox))))
                         (setf (mailbox-queue mailbox) (rest (mailbox-queue mailbox)))
                         v)))))
        (when value (return value))))))

;;; ECL: mp:make-queue, but the API varies by ECL version. Use a hand-rolled
;;; one for stability.
#+ecl
(progn
  (defstruct (mailbox (:constructor %make-mailbox))
    (queue '() :type list)
    (lock (mp:make-lock :name "clpm.repl-bridge.mbox"))
    (cv (mp:make-condition-variable)))

  (defun make-mailbox () (%make-mailbox))

  (defun send-message (mailbox value)
    (mp:with-lock ((mailbox-lock mailbox))
      (setf (mailbox-queue mailbox)
            (append (mailbox-queue mailbox) (list value)))
      (mp:condition-variable-broadcast (mailbox-cv mailbox)))
    value)

  (defun receive-message (mailbox)
    (mp:with-lock ((mailbox-lock mailbox))
      (loop
        (when (mailbox-queue mailbox)
          (let ((v (first (mailbox-queue mailbox))))
            (setf (mailbox-queue mailbox) (rest (mailbox-queue mailbox)))
            (return v)))
        (mp:condition-variable-wait (mailbox-cv mailbox)
                                    (mailbox-lock mailbox))))))

#-(or sbcl ccl ecl)
(progn
  (defun make-mailbox ()
    (error "mailbox not implemented on ~A" (lisp-implementation-type)))
  (defun send-message (mailbox value)
    (declare (ignore mailbox value))
    (error "send-message not implemented"))
  (defun receive-message (mailbox)
    (declare (ignore mailbox))
    (error "receive-message not implemented")))

;;; --------------------------------------------------------------------------
;;; POSIX bits + backtrace
;;; --------------------------------------------------------------------------

(defun getpid ()
  #+sbcl (sb-posix:getpid)
  #+ccl (ccl::getpid)
  #+ecl (ext:getpid)
  #-(or sbcl ccl ecl) -1)

(defun list-backtrace ()
  "Return a list of stack frames (any printable values) for inclusion in
eval-error responses. Best-effort: returns an empty list on impls that
don't expose a portable accessor."
  (handler-case
      #+sbcl (sb-debug:list-backtrace)
      #+ccl (let (frames)
              (ccl:map-call-frames
               (lambda (p context)
                 (declare (ignore context))
                 (push p frames))
               :start-frame-number 0
               :count 64)
              (nreverse frames))
      #+ecl '()
      #-(or sbcl ccl ecl) '()
    (error () '())))
