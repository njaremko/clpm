;;;; test/repl-watch-test.lisp - directory watchers.
;;;;
;;;; Covers BRIDGE_V2 #180 (watch DIR), #181 (unwatch ID / list-watches),
;;;; and ASDF-backed watch-system reloads.

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

(defun receive-message-or-fail (mailbox description &optional (timeout 8))
  (multiple-value-bind (message received?)
      (sb-concurrency:receive-message mailbox :timeout timeout)
    (unless received?
      (fail "timed out waiting for ~A" description))
    message))

(defun with-daemon (fn)
  (let* ((sock (format nil "/tmp/clpm-rb-watch-~A.sock" (random (expt 2 32))))
         (thread (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (clpm.repl:start-server :socket-path sock)
                      (error (c) (format *error-output* "daemon: ~A~%" c))))
                  :name "test-bridge-watch")))
    (unwind-protect
         (progn
           (loop for i from 0 below 50
                 while (not (probe-file sock))
                 do (sleep 0.05))
           (assert-true (probe-file sock) "daemon never started")
           (funcall fn sock))
      (handler-case (clpm.repl:send-request sock "shutdown")
        (error () nil))
      (loop for i from 0 below 60
            while (sb-thread:thread-alive-p thread)
            do (sleep 0.1))
      (when (sb-thread:thread-alive-p thread)
        (ignore-errors (sb-thread:terminate-thread thread)))
      (ignore-errors (delete-file sock)))))

(defun do-rpc (sock method &optional params &key on-event)
  (clpm.repl:send-request sock method
                                  :params (and params (list :object params))
                                  :on-event on-event))

;;; ----------------------------------------------------------------------------
;;; Make a fresh scratch directory per test so the seed file we create doesn't
;;; collide with previous runs.

(defun make-watch-dir ()
  (let ((path (format nil "/tmp/clpm-rb-watch-dir-~A/"
                      (random (expt 2 32)))))
    (ensure-directories-exist path)
    path))

(defun write-file (path contents)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                           :external-format :utf-8)
    (write-string contents s)))

(defun with-watchman-enabled (enabled fn)
  (let ((old clpm.repl::*watchman-enabled*))
    (unwind-protect
         (progn
           (setf clpm.repl::*watchman-enabled* enabled)
           (funcall fn))
      (setf clpm.repl::*watchman-enabled* old))))

(defun find-watch-entry (entries id)
  (find id entries
        :test (lambda (needle entry)
                (eql needle (lookup entry "id")))))

(defun make-watch-system ()
  (let* ((n (random (expt 2 32)))
         (name (format nil "clpm-repl-watch-system-~A" n))
         (package (string-upcase name))
         (dir (uiop:ensure-directory-pathname
               (format nil "/tmp/~A/" name))))
    (ensure-directories-exist dir)
    (values name package dir)))

(defun write-watch-system-macro (dir package value)
  (write-file
   (merge-pathnames "src/macros.lisp" dir)
   (format nil "(in-package #:~A)~%(defmacro watched-token () ~A)~%"
           package value)))

(defun write-macro-watch-system (dir name package value)
  (write-file
   (merge-pathnames (format nil "~A.asd" name) dir)
   (format nil "(asdf:defsystem ~S~%  :serial t~%  :components ((:module \"src\"~%                :serial t~%                :components ((:file \"package\")~%                             (:file \"macros\")~%                             (:file \"user\")))))~%"
           name))
  (let ((src-dir (merge-pathnames "src/" dir)))
    (write-file
     (merge-pathnames "package.lisp" src-dir)
     (format nil "(defpackage #:~A (:use #:cl) (:export #:macro-value))~%"
             package))
    (write-watch-system-macro dir package value)
    (write-file
     (merge-pathnames "user.lisp" src-dir)
     (format nil "(in-package #:~A)~%(defun macro-value () (watched-token))~%"
             package))))

(defun register-system-dir-form (dir)
  (format nil "(pushnew #P~S asdf:*central-registry* :test #'equal)"
          (namestring dir)))

;;; ----------------------------------------------------------------------------
;;; #180 + #181: watch acknowledges, list-watches shows the entry, unwatch
;;; produces a terminal `unwatched: true' result.

(format t "Test: watch acknowledged, listed, unwatched~%")
(with-daemon
  (lambda (sock)
    (let* ((dir (make-watch-dir))
           (events '())
           ;; The watch RPC streams events and only finalizes on unwatch.
           ;; Start it in a dedicated thread; have the main thread issue
           ;; `unwatch' once we've seen the acknowledgement.
           (watch-id-box (sb-concurrency:make-mailbox))
           (watcher
             (sb-thread:make-thread
              (lambda ()
                (do-rpc sock "watch"
                        (list (cons "dir" dir))
                        :on-event
                        (lambda (frame)
                          (push frame events)
                          (let ((ack-id (and (string= (lookup frame "event")
                                                      "watch-acknowledged")
                                             (lookup frame "id"))))
                            (when ack-id
                              (sb-concurrency:send-message watch-id-box
                                                            ack-id)))
                          nil)))
              :name "test-watch-client"))
           (wid (sb-concurrency:receive-message watch-id-box)))
      (assert-true (integerp wid) "no watch id from ack")
      ;; list-watches should know about it.
      (let* ((listed (do-rpc sock "list-watches"))
             (entries (array-items (lookup (lookup listed "result")
                                            "entries"))))
        (assert-true (find wid entries
                            :test (lambda (id e)
                                    (eql id (lookup e "id"))))
                     "list-watches missing wid=~A entries=~S" wid entries))
      ;; Tear it down; the watcher thread's RPC call should now return.
      (let ((un (do-rpc sock "unwatch" (list (cons "id" wid)))))
        (assert-true (lookup (lookup un "result") "stopped")
                     "unwatch should report stopped=t: ~S" un))
      (sb-thread:join-thread watcher)
      (assert-true (find-if (lambda (f)
                              (string= (lookup f "event")
                                       "watch-acknowledged"))
                            events)
                   "no acknowledged event observed"))))
(format t "  watch lifecycle OK~%")

;;; ----------------------------------------------------------------------------
;;; Watchman is an optional notification backend. Disabling it must leave the
;;; existing polling behavior selected and visible through watch-started and
;;; list-watches.

(format t "Test: watchman-disabled watches use polling backend~%")
(with-watchman-enabled
 nil
 (lambda ()
   (with-daemon
     (lambda (sock)
       (let* ((dir (make-watch-dir))
              (id-box (sb-concurrency:make-mailbox))
              (started-box (sb-concurrency:make-mailbox))
              (watcher
                (sb-thread:make-thread
                 (lambda ()
                   (do-rpc sock "watch"
                           (list (cons "dir" dir))
                           :on-event
                           (lambda (frame)
                             (let ((ev (lookup frame "event")))
                               (cond
                                 ((string= ev "watch-acknowledged")
                                  (sb-concurrency:send-message
                                   id-box (lookup frame "id")))
                                 ((string= ev "watch-started")
                                  (sb-concurrency:send-message
                                   started-box frame))))
                             nil)))
                 :name "test-watch-polling-backend")))
         (let* ((wid (receive-message-or-fail id-box "watch acknowledgement"))
                (started (receive-message-or-fail started-box "watch-started"))
                (listed (do-rpc sock "list-watches"))
                (entries (array-items (lookup (lookup listed "result")
                                              "entries")))
                (entry (find-watch-entry entries wid)))
           (assert-true (string= "polling" (lookup started "backend"))
                        "watch-started should report polling backend: ~S"
                        started)
           (assert-true entry "list-watches missing wid=~A entries=~S"
                        wid entries)
           (assert-true (string= "polling" (lookup entry "backend"))
                        "list-watches should report polling backend: ~S"
                        entry)
           (do-rpc sock "unwatch" (list (cons "id" wid)))
           (sb-thread:join-thread watcher)))))))
(format t "  watchman-disabled fallback OK~%")

;;; ----------------------------------------------------------------------------
;;; Watchman notification packets are only an event source; reload decisions
;;; still go through the same mtime table and file-level glob semantics.

(format t "Test: watchman event translation preserves watch semantics~%")
(let* ((dir (make-watch-dir))
       (file (format nil "~Atarget.lisp" dir))
       (nested (format nil "~Asub/nested.lisp" dir))
       (watch (clpm.repl::make-watch :id 1 :kind :file :dir dir
                                     :glob "*.lisp")))
  (unwind-protect
       (progn
         (write-file file "(defun watchman-target () :ok)")
         (multiple-value-bind (changed removed)
             (clpm.repl::%watchman-update-mtimes
              watch
              (clpm.repl::%json-object
               "files"
               (clpm.repl::%json-array
                (list (clpm.repl::%json-object "name" "target.lisp"
                                                "exists" t)))))
           (assert-true (and changed (null removed))
                        "existing watchman file event should be changed: ~S ~S"
                        changed removed)
           (assert-true (search "target.lisp" (first changed))
                        "changed file should be target.lisp: ~S" changed))
         (write-file nested "(defun nested-target () :ignored)")
         (multiple-value-bind (changed removed)
             (clpm.repl::%watchman-update-mtimes
              watch
              (clpm.repl::%json-object
               "files"
               (clpm.repl::%json-array
                (list (clpm.repl::%json-object "name" "sub/nested.lisp"
                                                "exists" t)))))
           (assert-true (and (null changed) (null removed))
                        "nested files should not match file-level watch: ~S ~S"
                        changed removed))
         (delete-file file)
         (multiple-value-bind (changed removed)
             (clpm.repl::%watchman-update-mtimes
              watch
              (clpm.repl::%json-object
               "files"
               (clpm.repl::%json-array
                (list (clpm.repl::%json-object "name" "target.lisp"
                                                "exists" :false)))))
           (assert-true (and (null changed) removed)
                        "removed watchman file event should be removed: ~S ~S"
                        changed removed)
           (assert-true (search "target.lisp" (first removed))
                        "removed file should be target.lisp: ~S" removed)))
    (ignore-errors (delete-file file))
    (ignore-errors (delete-file nested))
    (ignore-errors (uiop:delete-directory-tree dir :validate t))))
(format t "  watchman event translation OK~%")

;;; ----------------------------------------------------------------------------
;;; #180: mtime change fires file-reloaded.

(format t "Test: file-reloaded fires when a watched file changes~%")
(with-daemon
  (lambda (sock)
    (let* ((dir (make-watch-dir))
           (file (format nil "~Atarget.lisp" dir))
           (events '())
           (id-box (sb-concurrency:make-mailbox))
           (reload-box (sb-concurrency:make-mailbox))
           (watcher
             (sb-thread:make-thread
              (lambda ()
                (do-rpc sock "watch"
                        (list (cons "dir" dir))
                        :on-event
                        (lambda (frame)
                          (push frame events)
                          (let ((ev (lookup frame "event")))
                            (cond
                              ((string= ev "watch-acknowledged")
                               (sb-concurrency:send-message
                                id-box (lookup frame "id")))
                              ((string= ev "file-reloaded")
                               (sb-concurrency:send-message
                                reload-box frame))))
                          nil)))
              :name "test-watch-reload")))
      (let ((wid (sb-concurrency:receive-message id-box)))
        ;; Give the watcher time to seed its mtime map and select a backend.
        (sleep 1.2)
        ;; Now create the file. The watcher should emit file-reloaded.
        (write-file file
                    "(defun watch-target () :hello)")
        (let ((reload-evt (sb-concurrency:receive-message reload-box)))
          (assert-true (string= "file-reloaded"
                                 (lookup reload-evt "event"))
                       "expected file-reloaded, got ~S" reload-evt)
          (assert-true (search "target.lisp"
                                (or (lookup reload-evt "file") ""))
                       "wrong file: ~S" (lookup reload-evt "file")))
        (do-rpc sock "unwatch" (list (cons "id" wid)))
        (sb-thread:join-thread watcher)
        ;; The reloaded file is now in-image.
        (let ((resp (do-rpc sock "eval"
                            (list (cons "form" "(watch-target)")))))
          (assert-true (search "HELLO"
                                (or (lookup (lookup resp "result")
                                            "value")
                                    ""))
                       "watch-target should be defined: ~S" resp))))))
(format t "  file-reloaded OK~%")

;;; ----------------------------------------------------------------------------
;;; #180: a broken file produces a reload-failed event.

(format t "Test: reload-failed fires for invalid source~%")
(with-daemon
  (lambda (sock)
    (let* ((dir (make-watch-dir))
           (file (format nil "~Abroken.lisp" dir))
           (id-box (sb-concurrency:make-mailbox))
           (fail-box (sb-concurrency:make-mailbox))
           (watcher
             (sb-thread:make-thread
              (lambda ()
                (do-rpc sock "watch"
                        (list (cons "dir" dir))
                        :on-event
                        (lambda (frame)
                          (let ((ev (lookup frame "event")))
                            (cond
                              ((string= ev "watch-acknowledged")
                               (sb-concurrency:send-message
                                id-box (lookup frame "id")))
                              ((string= ev "reload-failed")
                               (sb-concurrency:send-message
                                fail-box frame))))
                          nil)))
              :name "test-watch-fail")))
      (let ((wid (sb-concurrency:receive-message id-box)))
        (sleep 1.2)
        (write-file file "(defun broken (")  ; unterminated form
        (let ((fail-evt (sb-concurrency:receive-message fail-box)))
          (assert-true (string= "reload-failed"
                                 (lookup fail-evt "event"))
                       "expected reload-failed, got ~S" fail-evt))
        (do-rpc sock "unwatch" (list (cons "id" wid)))
        (sb-thread:join-thread watcher)))))
(format t "  reload-failed OK~%")

;;; ----------------------------------------------------------------------------
;;; watch-system watches ASDF component files and reloads through ASDF, not by
;;; direct-loading the changed file. Changing a macro file must recompile the
;;; dependent user file, otherwise MACRO-VALUE would keep returning :OLD.

(format t "Test: watch-system reloads through ASDF~%")
(multiple-value-bind (name package dir)
    (make-watch-system)
  (unwind-protect
       (progn
         (write-macro-watch-system dir name package ":old")
         (with-daemon
           (lambda (sock)
             (let ((registered (do-rpc sock "eval"
                                       (list (cons "form"
                                                   (register-system-dir-form dir))))))
                (assert-true (lookup registered "result")
                             "failed to register temp system: ~S" registered))
             (let ((loaded (do-rpc sock "load-system"
                                   (list (cons "name" name)))))
               (assert-true (lookup loaded "result")
                            "initial load-system failed: ~S" loaded))
             (let* ((old (do-rpc sock "eval"
                                 (list (cons "form"
                                             (format nil "(~A:macro-value)"
                                                     package)))))
                    (value (lookup (lookup old "result") "value")))
               (assert-true (search "OLD" value)
                            "initial macro-value should be old: ~S" old))
             (let* ((id-box (sb-concurrency:make-mailbox))
                    (reload-box (sb-concurrency:make-mailbox))
                     (watcher
                       (sb-thread:make-thread
                        (lambda ()
                         (do-rpc sock "watch-system"
                                 (list (cons "name" name))
                                 :on-event
                                 (lambda (frame)
                                   (let ((ev (lookup frame "event")))
                                     (cond
                                        ((string= ev "watch-acknowledged")
                                         (sb-concurrency:send-message
                                          id-box (lookup frame "id")))
                                        ((string= ev "system-reloaded")
                                         (sb-concurrency:send-message
                                          reload-box frame))))
                                    nil)))
                        :name "test-watch-system")))
                (let ((wid (receive-message-or-fail id-box "watch-system acknowledgement")))
                  (sleep 1.2)
                  (write-watch-system-macro dir package ":new")
                  (let ((reload-evt (receive-message-or-fail reload-box
                                                            "ASDF system reload")))
                    (assert-true (string= "system-reloaded"
                                         (lookup reload-evt "event"))
                                 "expected system-reloaded, got ~S" reload-evt)
                    (assert-true (string= name (lookup reload-evt "system"))
                                 "reload event should name the system: ~S"
                                 reload-evt))
                  (do-rpc sock "unwatch" (list (cons "id" wid)))
                  (sb-thread:join-thread watcher)
                  (let* ((resp (do-rpc sock "eval"
                                       (list (cons "form"
                                                   (format nil "(~A:macro-value)"
                                                           package)))))
                         (value (lookup (lookup resp "result") "value")))
                    (assert-true (search "NEW" value)
                                 "watch-system should recompile ASDF dependents: ~S"
                                 resp)))))))
    (ignore-errors (delete-file (merge-pathnames "src/package.lisp" dir)))
    (ignore-errors (delete-file (merge-pathnames "src/macros.lisp" dir)))
    (ignore-errors (delete-file (merge-pathnames "src/user.lisp" dir)))
    (ignore-errors (delete-file (merge-pathnames (format nil "~A.asd" name) dir)))
    (ignore-errors (uiop:delete-directory-tree dir :validate t))))
(format t "  watch-system OK~%")

(format t "~%REPL watch tests PASSED!~%")
(sb-ext:exit :code 0)
