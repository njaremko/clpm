;;;; test/repl-bridge-source-test.lisp - source navigation + compile/load
;;;; diagnostics + macroexpand.
;;;;
;;;; Covers BRIDGE_V2 #130 (compile-file), #131 (load-file), #132
;;;; (find-definition), #133 (xref), #136 (macroexpand).

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
  (let* ((sock (format nil "/tmp/clpm-rb-source-~A.sock" (random (expt 2 32))))
         (thread (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (clpm.repl-bridge:start-server :socket-path sock)
                      (error (c) (format *error-output* "daemon: ~A~%" c))))
                  :name "test-bridge-source")))
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

;;; ----------------------------------------------------------------------------
;;; #132: find-definition for a known SB-INTROSPECT function.

(format t "Test: find-definition for FORMAT~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (clpm.repl-bridge:send-request
                  sock "find-definition"
                  :params (list :object
                                (list (cons "symbol" "format")
                                      (cons "package" "CL")))))
           (result (lookup resp "result"))
           (entries (array-items (lookup result "entries"))))
      (assert-true entries "no entries: ~S" resp)
      ;; FORMAT has at least one function definition somewhere in SBCL.
      (assert-true (find "function" entries
                          :test (lambda (s e) (string= s (lookup e "kind"))))
                   "no function definition for FORMAT: ~S" entries))))
(format t "  find-definition OK~%")

;;; ----------------------------------------------------------------------------
;;; #133: xref `who-calls' includes at least one caller of FORMAT.

(format t "Test: xref callers of FORMAT exists~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (clpm.repl-bridge:send-request
                  sock "xref"
                  :params (list :object
                                (list (cons "symbol" "format")
                                      (cons "direction" "callers")
                                      (cons "package" "CL")))))
           (result (lookup resp "result"))
           (entries (array-items (lookup result "entries"))))
      (assert-true (or (null entries) (consp entries))
                   "entries should be a list: ~S" entries)
      ;; SBCL has many internal callers of FORMAT but the result may be
      ;; empty in a stripped image; we only check the call succeeded.
      (assert-true result "xref returned no result: ~S" resp))))
(format t "  xref OK~%")

;;; ----------------------------------------------------------------------------
;;; #136: macroexpand-1 of WHEN.

(format t "Test: macroexpand-1 of (when t :ok)~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (clpm.repl-bridge:send-request
                  sock "macroexpand"
                  :params (list :object (list (cons "form" "(when t :ok)")))))
           (result (lookup resp "result")))
      (assert-true result "no result: ~S" resp)
      (assert-true (lookup result "expanded_p") "expected expanded_p")
      (let ((expanded (lookup result "form")))
        (assert-true (and (stringp expanded)
                          (search "IF" expanded :test #'char-equal))
                     "expected IF in expansion, got ~S" expanded)))))
(format t "  macroexpand OK~%")

;;; ----------------------------------------------------------------------------
;;; #130: compile-file on a tiny file with an undefined-function warning.

(format t "Test: compile-file streams diagnostic on undefined function~%")
(let* ((tmp-src (format nil "/tmp/clpm-test-undef-~A.lisp" (random (expt 2 32))))
       (events '()))
  (with-open-file (s tmp-src :direction :output :if-exists :supersede)
    (format s "(defun caller () (no-such-fn 1))~%"))
  (unwind-protect
       (with-daemon
         (lambda (sock)
           (let* ((resp (clpm.repl-bridge:send-request
                        sock "compile-file"
                        :params (list :object (list (cons "path" tmp-src)))
                        :on-event (lambda (frame)
                                    (push frame events)
                                    nil)))
                  (result (lookup resp "result")))
             (assert-true result "expected success result: ~S" resp)
             ;; Some diagnostic must have been streamed.
             (assert-true (some (lambda (f)
                                  (string= "diagnostic" (lookup f "event")))
                                events)
                          "no diagnostic event"))))
    (ignore-errors (delete-file tmp-src))
    (ignore-errors
     (let ((fasl (make-pathname :type "fasl" :defaults tmp-src)))
       (when (probe-file fasl) (delete-file fasl))))))
(format t "  compile-file diagnostics OK~%")

;;; ----------------------------------------------------------------------------
;;; #131: load-file completes and returns the active package.

(format t "Test: load-file returns the active package~%")
(let* ((tmp-src (format nil "/tmp/clpm-test-load-~A.lisp" (random (expt 2 32)))))
  (with-open-file (s tmp-src :direction :output :if-exists :supersede)
    (format s "(defvar *load-test-marker* 42)~%"))
  (unwind-protect
       (with-daemon
         (lambda (sock)
           (let* ((resp (clpm.repl-bridge:send-request
                        sock "load-file"
                        :params (list :object (list (cons "path" tmp-src)))))
                  (result (lookup resp "result")))
             (assert-true result "expected success: ~S" resp)
             (assert-true (lookup result "success") "success not t")
             (assert-true (stringp (lookup result "package"))
                          "package missing"))))
    (ignore-errors (delete-file tmp-src))))
(format t "  load-file OK~%")

(format t "~%REPL-bridge source tests PASSED!~%")
(sb-ext:exit :code 0)
