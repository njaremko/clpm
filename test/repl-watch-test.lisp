;;;; test/repl-watch-test.lisp - directory watchers.
;;;;
;;;; Covers BRIDGE_V2 #180 (watch DIR), #181 (unwatch ID / list-watches),
;;;; #182 (auto-revert).

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

(defun make-watch-system ()
  (let* ((n (random (expt 2 32)))
         (name (format nil "clpm-repl-watch-system-~A" n))
         (package (string-upcase name))
         (dir (uiop:ensure-directory-pathname
               (format nil "/tmp/~A/" name))))
    (ensure-directories-exist dir)
    (values name package dir)))

(defun write-watch-system (dir name package value)
  (write-file
   (merge-pathnames (format nil "~A.asd" name) dir)
   (format nil "(asdf:defsystem ~S :serial t :components ((:file \"source\")))~%"
           name))
  (write-file
   (merge-pathnames "source.lisp" dir)
   (format nil "(defpackage #:~A (:use #:cl) (:export #:value))~%(in-package #:~A)~%(defun value () ~A)~%"
           package package value)))

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
        ;; Give the watcher one polling cycle to seed its mtime map.
        (sleep 1.2)
        ;; Now create the file. The next poll should emit file-reloaded.
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
;;; #182: auto_revert emits revert-applied after a reload.

(format t "Test: auto_revert emits revert-applied~%")
(with-daemon
  (lambda (sock)
    (let* ((dir (make-watch-dir))
           (file (format nil "~Arevertible.lisp" dir))
           (id-box (sb-concurrency:make-mailbox))
           (revert-box (sb-concurrency:make-mailbox))
           (watcher
             (sb-thread:make-thread
              (lambda ()
                (do-rpc sock "watch"
                        (list (cons "dir" dir)
                              (cons "auto_revert" t))
                        :on-event
                        (lambda (frame)
                          (let ((ev (lookup frame "event")))
                            (cond
                              ((string= ev "watch-acknowledged")
                               (sb-concurrency:send-message
                                id-box (lookup frame "id")))
                              ((string= ev "revert-applied")
                               (sb-concurrency:send-message
                                revert-box frame))))
                          nil)))
              :name "test-watch-revert")))
      (let ((wid (sb-concurrency:receive-message id-box)))
        (sleep 1.2)
        (write-file file "(defun revertible () :ok)")
        (let ((revert-evt (sb-concurrency:receive-message revert-box)))
          (assert-true (string= "revert-applied"
                                 (lookup revert-evt "event"))
                       "expected revert-applied"))
        (do-rpc sock "unwatch" (list (cons "id" wid)))
        (sb-thread:join-thread watcher)))))
(format t "  auto-revert OK~%")

;;; ----------------------------------------------------------------------------
;;; watch-system resolves an ASDF system source directory, then uses the same
;;; reload path as watch.

(format t "Test: watch-system reloads changed system source~%")
(multiple-value-bind (name package dir)
    (make-watch-system)
  (unwind-protect
       (progn
         (write-watch-system dir name package ":old")
         (with-daemon
           (lambda (sock)
             (let ((registered (do-rpc sock "eval"
                                       (list (cons "form"
                                                   (register-system-dir-form dir))))))
               (assert-true (lookup registered "result")
                            "failed to register temp system: ~S" registered))
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
                                       ((string= ev "file-reloaded")
                                        (sb-concurrency:send-message
                                         reload-box frame))))
                                   nil)))
                       :name "test-watch-system")))
               (let ((wid (sb-concurrency:receive-message id-box)))
                 (sleep 1.2)
                 (write-watch-system dir name package ":new")
                 (let ((reload-evt (sb-concurrency:receive-message reload-box)))
                   (assert-true (string= "file-reloaded"
                                        (lookup reload-evt "event"))
                                "expected file-reloaded, got ~S" reload-evt))
                 (do-rpc sock "unwatch" (list (cons "id" wid)))
                 (sb-thread:join-thread watcher)
                 (let* ((resp (do-rpc sock "eval"
                                      (list (cons "form"
                                                  (format nil "(~A:value)"
                                                          package)))))
                        (value (lookup (lookup resp "result") "value")))
                   (assert-true (search "NEW" value)
                                "watch-system should reload new value: ~S"
                                resp)))))))
    (ignore-errors (delete-file (merge-pathnames "source.lisp" dir)))
    (ignore-errors (delete-file (merge-pathnames (format nil "~A.asd" name) dir)))
    (ignore-errors (uiop:delete-directory-tree dir :validate t))))
(format t "  watch-system OK~%")

(format t "~%REPL watch tests PASSED!~%")
(sb-ext:exit :code 0)
