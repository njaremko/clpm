;;;; test/repl-bridge-image-test.lisp - image management RPCs.
;;;;
;;;; Covers BRIDGE_V2 #190 (image-info), #191 (loaded-systems),
;;;; #192 (list-packages), #194 (gc).

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
  (let* ((sock (format nil "/tmp/clpm-rb-image-~A.sock" (random (expt 2 32))))
         (thread (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (clpm.repl-bridge:start-server :socket-path sock)
                      (error (c) (format *error-output* "daemon: ~A~%" c))))
                  :name "test-bridge-image")))
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

(format t "Test: image-info has pid, lisp, features~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (do-rpc sock "image-info"))
           (result (lookup resp "result")))
      (assert-true (integerp (lookup result "pid"))
                   "pid missing")
      (assert-true (stringp (lookup result "lisp"))
                   "lisp missing")
      (assert-true (consp (lookup result "features"))
                   "features missing"))))
(format t "  image-info OK~%")

(format t "Test: loaded-systems includes clpm itself~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (do-rpc sock "loaded-systems"))
           (entries (array-items (lookup (lookup resp "result") "entries"))))
      (assert-true (find "clpm" entries
                          :test (lambda (s e)
                                  (string-equal s (lookup e "name"))))
                   "clpm not in loaded-systems"))))
(format t "  loaded-systems OK~%")

(format t "Test: list-packages includes CL~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (do-rpc sock "list-packages"))
           (entries (array-items (lookup (lookup resp "result") "entries")))
           (cl-entry (find "COMMON-LISP" entries
                            :test (lambda (s e)
                                    (string= s (lookup e "name"))))))
      (assert-true cl-entry "no COMMON-LISP entry")
      (assert-true (> (lookup cl-entry "external") 500)
                   "CL should have >500 externals"))))
(format t "  list-packages OK~%")

(format t "Test: gc returns before/after bytes~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (do-rpc sock "gc"))
           (result (lookup resp "result")))
      (assert-true (integerp (lookup result "before_bytes"))
                   "before_bytes missing")
      (assert-true (integerp (lookup result "after_bytes"))
                   "after_bytes missing"))))
(format t "  gc OK~%")

(format t "~%REPL-bridge image tests PASSED!~%")
(sb-ext:exit :code 0)
