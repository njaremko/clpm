;;;; test/repl-cli-test.lisp - end-to-end CLI: daemon, eval, call

(require :asdf)
(require :sb-posix)

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

(defun assert-eql (expected actual)
  (unless (eql expected actual)
    (fail "expected ~S, got ~S" expected actual)))

(defun assert-true (x fmt &rest args)
  (unless x (apply #'fail fmt args)))

(defun assert-contains (haystack needle)
  (unless (and (stringp haystack) (search needle haystack))
    (fail "expected ~S in:~%~A" needle haystack)))

(defun run-cli-captured (args)
  "Run clpm via run-cli, capturing stdout/stderr. Returns (values rc stdout stderr)."
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((*standard-output* out)
          (*error-output* err))
      (let ((rc (clpm:run-cli args)))
        (values rc
                (get-output-stream-string out)
                (get-output-stream-string err))))))

(defun make-short-temp-dir ()
  "Create a short temp directory so Unix-domain socket paths stay portable."
  (let* ((template (namestring
                    (merge-pathnames "rbXXXXXX" (uiop:temporary-directory))))
         (dir (sb-posix:mkdtemp template)))
    (uiop:ensure-directory-pathname dir)))

(defmacro with-short-temp-dir ((var) &body body)
  `(let ((,var (make-short-temp-dir)))
     (unwind-protect
          (progn ,@body)
       (ignore-errors
         (uiop:delete-directory-tree ,var :validate t)))))

;;; ----------------------------------------------------------------------------

(with-short-temp-dir (tmp)
  (let* ((proj (merge-pathnames "myproj/" tmp))
         (manifest (merge-pathnames "clpm.project" proj))
         (clpm-dir (merge-pathnames ".clpm/" proj))
         (asdf-config (merge-pathnames "asdf-config.lisp" clpm-dir)))
    (ensure-directories-exist clpm-dir)
    ;; Minimal project file. Empty :systems so the preload pass has nothing
    ;; to chase and we don't depend on a fixture .asd existing here.
    (with-open-file (s manifest :direction :output :if-exists :supersede
                                :external-format :utf-8)
      (write-string "(:project :name \"myproj\" :version \"0.1.0\" :systems () :registries ())" s))
    ;; Empty asdf-config so %bridge-load-project finds something.
    (with-open-file (s asdf-config :direction :output :if-exists :supersede
                                   :external-format :utf-8)
      (write-string ";; empty for test~%" s))

    (uiop:with-current-directory (proj)
      (format t "Test: daemon (no --detach) blocks; we run it in a thread~%")
      (let ((srv (sb-thread:make-thread
                  (lambda ()
                    ;; Surface daemon-startup failures instead of letting
                    ;; them disappear behind a captured *error-output*.
                    (handler-case
                        (run-cli-captured '("repl" "daemon"))
                      (error (c)
                        (format *error-output* "daemon thread died: ~A~%" c)
                        (force-output *error-output*))))
                  :name "test-daemon")))
        (declare (ignorable srv))
        ;; Yield once so the daemon thread reaches accept() before we
        ;; start polling. Without this, sbcl --script can hand the
        ;; polling loop most of the cpu and time us out.
        (sleep 0.05)
        ;; Wait for the socket to appear.
        (let ((sock (namestring (merge-pathnames ".clpm/repl.sock" proj))))
          (loop for i from 0 below 100
                while (not (probe-file sock))
                do (sleep 0.1))
          (assert-true (probe-file sock) "daemon socket did not appear: ~A" sock)
          (format t "  daemon up~%")

          (format t "Test: clpm repl eval (default human output)~%")
          (multiple-value-bind (rc stdout stderr)
              (run-cli-captured '("repl" "eval" "(+ 1 2)" "--no-autostart"))
            (declare (ignore stderr))
            (assert-eql 0 rc)
            ;; Default rendering is "=> 3"; no raw JSON.
            (assert-contains stdout "=> 3"))
          (format t "  eval OK~%")

          (format t "Test: eval --json prints raw JSON~%")
          (multiple-value-bind (rc stdout)
              (run-cli-captured '("repl" "eval" "(+ 1 2)"
                                  "--no-autostart" "--json"))
            (assert-eql 0 rc)
            (assert-contains stdout "\"value\":\"3\"")
            (assert-contains stdout "\"result\""))
          (format t "  eval --json OK~%")

          (format t "Test: state persists across eval calls~%")
          (run-cli-captured '("repl" "eval"
                              "(defparameter *cli-x* 41)" "--no-autostart"))
          (multiple-value-bind (rc stdout)
              (run-cli-captured '("repl" "eval" "*cli-x*"
                                  "--no-autostart"))
            (declare (ignore rc))
            (assert-contains stdout "=> 41"))
          (format t "  state persistence OK~%")

          (format t "Test: call ping returns daemon info~%")
          (multiple-value-bind (rc stdout)
              (run-cli-captured '("repl" "call" "ping"))
            (assert-eql 0 rc)
            (assert-contains stdout "\"pid\"")
            (assert-contains stdout "\"uptime_ms\""))
          (format t "  call ping OK~%")

          (format t "Test: daemon --status reports running~%")
          (multiple-value-bind (rc stdout)
              (run-cli-captured '("repl" "daemon" "--status"))
            (assert-eql 0 rc)
            (assert-contains stdout "running")
            (assert-contains stdout "pid"))
          (format t "  status OK~%")

          (format t "Test: daemon --status --json reports raw status~%")
          (multiple-value-bind (rc stdout)
              (run-cli-captured '("repl" "daemon" "--status" "--json"))
            (assert-eql 0 rc)
            (assert-contains stdout "\"state\":\"running\"")
            (assert-contains stdout "\"pid\""))
          (format t "  status --json OK~%")

          (format t "Test: eval --no-autostart fails after stop~%")
          (multiple-value-bind (rc stdout stderr)
              (run-cli-captured '("repl" "daemon" "--stop"))
            (declare (ignore stderr stdout))
            (assert-eql 0 rc))
          ;; Wait for daemon thread to finish.
          (loop for i from 0 below 30
                while (sb-thread:thread-alive-p srv)
                do (sleep 0.1))
          (when (sb-thread:thread-alive-p srv)
            (ignore-errors (sb-thread:terminate-thread srv)))
          (multiple-value-bind (rc stdout stderr)
              (run-cli-captured '("repl" "eval" "(+ 1 2)" "--no-autostart"))
            (declare (ignore stdout))
            (assert-eql 2 rc)
            (assert-contains stderr "No daemon"))
          (format t "  no-daemon path OK~%")

          (format t "Test: daemon --status reports not running after stop~%")
          (multiple-value-bind (rc stdout)
              (run-cli-captured '("repl" "daemon" "--status"))
            (assert-eql 0 rc)
            (assert-contains stdout "not running"))
          (format t "  status-after-stop OK~%"))))))

(format t "~%REPL CLI tests PASSED!~%")
(sb-ext:exit :code 0)
