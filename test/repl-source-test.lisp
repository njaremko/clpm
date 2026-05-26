;;;; test/repl-source-test.lisp - source navigation + compile/load
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

(defun write-file (path contents)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string contents s)))

(defun make-temp-system ()
  (let* ((n (random (expt 2 32)))
         (name (format nil "clpm-repl-system-~A" n))
         (package (string-upcase name))
         (dir (uiop:ensure-directory-pathname
               (format nil "/tmp/~A/" name))))
    (ensure-directories-exist dir)
    (values name package dir)))

(defun write-test-system (dir name package value)
  (let ((asd (merge-pathnames (format nil "~A.asd" name) dir))
        (src (merge-pathnames "source.lisp" dir)))
    (write-file
     asd
     (format nil "(asdf:defsystem ~S~%  :serial t~%  :components ((:file \"source\"))~%  :perform (asdf:test-op (op c)~%             (declare (ignore op c))~%             (let* ((pkg (find-package ~S))~%                    (sym (and pkg (find-symbol \"VALUE\" pkg))))~%               (unless (and sym (eq (funcall sym) :new))~%                 (error \"test system did not see new value\")))))~%"
             name package))
    (write-file
     src
     (format nil "(defpackage #:~A (:use #:cl) (:export #:value))~%(in-package #:~A)~%(defun value () ~A)~%"
             package package value))
    src))

(defun register-system-dir-form (dir)
  (format nil "(pushnew #P~S asdf:*central-registry* :test #'equal)"
          (namestring dir)))

(defun with-daemon (fn)
  (let* ((sock (format nil "/tmp/clpm-rb-source-~A.sock" (random (expt 2 32))))
         (thread (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (clpm.repl:start-server :socket-path sock)
                      (error (c) (format *error-output* "daemon: ~A~%" c))))
                  :name "test-bridge-source")))
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

;;; ----------------------------------------------------------------------------
;;; #132: find-definition for a known SB-INTROSPECT function.

(format t "Test: find-definition for FORMAT~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (clpm.repl:send-request
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

(format t "Test: find-definition reports real source line numbers~%")
(with-daemon
  (lambda (sock)
    (multiple-value-bind (name package dir)
        (make-temp-system)
      (let ((src (write-test-system dir name package ":line-test")))
        (clpm.repl:send-request
         sock "eval"
         :params (list :object
                       (list (cons "form" (register-system-dir-form dir)))))
        (let ((load-resp
                (clpm.repl:send-request
                 sock "load-system"
                 :params (list :object (list (cons "name" name))))))
          (assert-true (lookup load-resp "result")
                       "load-system failed: ~S" load-resp))
        (let* ((resp (clpm.repl:send-request
                      sock "find-definition"
                      :params (list :object
                                    (list (cons "symbol" "value")
                                          (cons "package" package)
                                          (cons "kind" "function")))))
               (entries (array-items (lookup (lookup resp "result")
                                            "entries")))
                (entry (find (namestring (truename src)) entries
                             :test (lambda (path e)
                                     (let* ((loc (lookup e "location"))
                                            (file (lookup loc "file")))
                                      (and file (string= path file))))))
               (line (and entry (lookup (lookup entry "location") "line"))))
          (assert-true entry "no entry for temp source: ~S" resp)
          (assert-true (= 3 line)
                       "expected defun on line 3, got ~S in ~S"
                       line resp))))))
(format t "  source line OK~%")

;;; ----------------------------------------------------------------------------
;;; #133: xref `who-calls' includes at least one caller of FORMAT.

(format t "Test: xref callers tracks live in-image relationships~%")
(with-daemon
  (lambda (sock)
    ;; Define two functions on the wire so they get compiled with
    ;; xref enabled, then ask who-calls the inner one.
    (let* ((define-resp
             (clpm.repl:send-request
              sock "eval"
              :params (list :object
                            (list (cons "form"
                                        "(progn (defun xref-callee () 'inner)
                                                (defun xref-caller () (xref-callee))
                                                :ok)"))))))
      (assert-true (lookup define-resp "result")
                   "couldn't define callee/caller: ~S" define-resp))
    (let* ((resp (clpm.repl:send-request
                  sock "xref"
                  :params (list :object
                                (list (cons "symbol" "xref-callee")
                                      (cons "direction" "callers")
                                      (cons "package" "CL-USER")))))
           (result (lookup resp "result"))
           (entries (array-items (lookup result "entries"))))
      (assert-true result "xref returned no result: ~S" resp)
      (assert-true (consp entries)
                   "expected xref-callee to have a caller, got: ~S" entries)
      ;; The caller is XREF-CALLER -- name field carries it.
      (let ((found nil))
        (dolist (e entries)
          (when (and (consp e) (eq (car e) :object))
            (let ((nm (cdr (assoc "name" (cadr e) :test #'string=))))
              (when (and (stringp nm)
                         (search "XREF-CALLER" (string-upcase nm)))
                (setf found t)))))
        (assert-true found
                     "expected XREF-CALLER among entries: ~S" entries)))))
(format t "  xref OK~%")

(format t "Test: introspection resolves exact mixed-case symbols~%")
(with-daemon
  (lambda (sock)
    (let ((def (clpm.repl:send-request
                sock "eval"
                :params (list :object
                              (list (cons "form"
                                          "(defun |MixedCaseFn| (x) x)"))))))
      (assert-true (lookup def "result") "defun failed: ~S" def))
    (let* ((resp (clpm.repl:send-request
                  sock "arglist"
                  :params (list :object
                                (list (cons "symbol" "MixedCaseFn")
                                      (cons "package" "CL-USER")))))
           (result (lookup resp "result")))
      (assert-true result "mixed-case symbol was not resolved: ~S" resp)
      (assert-true (search "X" (lookup result "arglist"))
                   "arglist did not mention X: ~S" resp))))
(format t "  mixed-case symbol OK~%")

;;; ----------------------------------------------------------------------------
;;; #136: macroexpand-1 of WHEN.

(format t "Test: macroexpand-1 of (when t :ok)~%")
(with-daemon
  (lambda (sock)
    (let* ((resp (clpm.repl:send-request
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
           (let* ((resp (clpm.repl:send-request
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
           (let* ((resp (clpm.repl:send-request
                        sock "load-file"
                        :params (list :object (list (cons "path" tmp-src)))))
                  (result (lookup resp "result")))
             (assert-true result "expected success: ~S" resp)
             (assert-true (lookup result "success") "success not t")
             (assert-true (stringp (lookup result "package"))
                          "package missing"))))
    (ignore-errors (delete-file tmp-src))))
(format t "  load-file OK~%")

;;; ----------------------------------------------------------------------------
;;; ASDF system rehydration: load-system recompiles/reloads stale files and
;;; test-system runs test-op in the same live image.

(format t "Test: load-system reloads changed ASDF source~%")
(multiple-value-bind (name package dir)
    (make-temp-system)
  (let ((src nil))
    (unwind-protect
         (progn
           (setf src (write-test-system dir name package ":old"))
           (with-daemon
             (lambda (sock)
               (let ((register (clpm.repl:send-request
                                sock "eval"
                                :params (list :object
                                              (list (cons "form"
                                                          (register-system-dir-form dir)))))))
                 (assert-true (lookup register "result")
                              "central-registry push failed: ~S" register))
               (let* ((loaded (clpm.repl:send-request
                               sock "load-system"
                               :params (list :object
                                             (list (cons "name" name)))))
                      (result (lookup loaded "result")))
                 (assert-true result "load-system failed: ~S" loaded)
                 (assert-true (lookup result "success")
                              "load-system success missing: ~S" result))
               (let* ((old (clpm.repl:send-request
                            sock "eval"
                            :params (list :object
                                          (list (cons "form"
                                                      (format nil "(~A:value)"
                                                              package))))))
                      (old-result (lookup old "result")))
                 (assert-true (search "OLD" (lookup old-result "value"))
                              "expected old value, got: ~S" old))
               (sleep 1.2)
               (write-test-system dir name package ":new")
               (let* ((loaded (clpm.repl:send-request
                               sock "load-system"
                               :params (list :object
                                             (list (cons "name" name)
                                                   (cons "force" :false)))))
                      (result (lookup loaded "result")))
                  (assert-true result "second load-system failed: ~S" loaded)
                  (assert-true (eq :false (lookup result "force"))
                               "JSON false force should remain false: ~S" result))
               (let* ((new (clpm.repl:send-request
                            sock "eval"
                            :params (list :object
                                          (list (cons "form"
                                                      (format nil "(~A:value)"
                                                              package))))))
                      (new-result (lookup new "result")))
                 (assert-true (search "NEW" (lookup new-result "value"))
                              "expected new value after load-system, got: ~S"
                              new))
               (let* ((tested (clpm.repl:send-request
                               sock "test-system"
                               :params (list :object
                                             (list (cons "name" name)))))
                      (test-result (lookup tested "result")))
                 (assert-true test-result "test-system failed: ~S" tested)
                 (assert-true (string= "test-system"
                                       (lookup test-result "operation"))
                              "wrong operation: ~S" test-result)))))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file (merge-pathnames (format nil "~A.asd" name) dir)))
      (ignore-errors (uiop:delete-directory-tree dir :validate t)))))
(format t "  load-system/test-system OK~%")

(format t "~%REPL source tests PASSED!~%")
(sb-ext:exit :code 0)
