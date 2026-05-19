;;;; test/repl-bridge-cli-test.lisp - end-to-end CLI: serve, eval, status, stop

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

;;; ----------------------------------------------------------------------------

(clpm.store:with-temp-dir (tmp)
  (let* ((proj (merge-pathnames "myproj/" tmp))
         (manifest (merge-pathnames "clpm.project" proj))
         (clpm-dir (merge-pathnames ".clpm/" proj))
         (asdf-config (merge-pathnames "asdf-config.lisp" clpm-dir)))
    (ensure-directories-exist clpm-dir)
    ;; Minimal project file.
    (with-open-file (s manifest :direction :output :if-exists :supersede
                                :external-format :utf-8)
      (write-string "(:project :name \"myproj\" :version \"0.1.0\" :systems (\"myproj\") :registries ())" s))
    ;; Empty asdf-config so %bridge-load-project finds something.
    (with-open-file (s asdf-config :direction :output :if-exists :supersede
                                   :external-format :utf-8)
      (write-string ";; empty for test~%" s))

    (uiop:with-current-directory (proj)
      (format t "Test: serve (no --detach) blocks; we run it in a thread~%")
      (let ((srv (sb-thread:make-thread
                  (lambda ()
                    (run-cli-captured '("repl-bridge" "serve")))
                  :name "test-serve")))
        ;; Wait for the socket to appear.
        (let ((sock (namestring (merge-pathnames ".clpm/repl-bridge.sock" proj))))
          (loop for i from 0 below 50
                while (not (probe-file sock))
                do (sleep 0.1))
          (assert-true (probe-file sock) "daemon socket did not appear: ~A" sock)
          (format t "  daemon up~%")

          (format t "Test: clpm repl-bridge eval~%")
          (multiple-value-bind (rc stdout stderr)
              (run-cli-captured '("repl-bridge" "eval" "(+ 1 2)" "--no-autostart"))
            (declare (ignore stderr))
            (assert-eql 0 rc)
            ;; JSON one-line with value:"3"
            (assert-contains stdout "\"value\":\"3\"")
            (assert-contains stdout "\"result\""))
          (format t "  eval OK~%")

          (format t "Test: state persists across eval calls~%")
          (run-cli-captured '("repl-bridge" "eval" "(defparameter *cli-x* 41)" "--no-autostart"))
          (multiple-value-bind (rc stdout)
              (run-cli-captured '("repl-bridge" "eval" "*cli-x*" "--no-autostart"))
            (declare (ignore rc))
            (assert-contains stdout "\"value\":\"41\""))
          (format t "  state persistence OK~%")

          (format t "Test: ping returns daemon info~%")
          (multiple-value-bind (rc stdout)
              (run-cli-captured '("repl-bridge" "ping"))
            (assert-eql 0 rc)
            (assert-contains stdout "\"pid\":")
            (assert-contains stdout "\"uptime_ms\":"))
          (format t "  ping OK~%")

          (format t "Test: status reports running~%")
          (multiple-value-bind (rc stdout)
              (run-cli-captured '("repl-bridge" "status"))
            (assert-eql 0 rc)
            (assert-contains stdout "running")
            (assert-contains stdout "pid"))
          (format t "  status OK~%")

          (format t "Test: eval --no-autostart fails after stop~%")
          (multiple-value-bind (rc stdout stderr)
              (run-cli-captured '("repl-bridge" "stop"))
            (declare (ignore stderr stdout))
            (assert-eql 0 rc))
          ;; Wait for daemon thread to finish.
          (loop for i from 0 below 30
                while (sb-thread:thread-alive-p srv)
                do (sleep 0.1))
          (when (sb-thread:thread-alive-p srv)
            (ignore-errors (sb-thread:terminate-thread srv)))
          (multiple-value-bind (rc stdout stderr)
              (run-cli-captured '("repl-bridge" "eval" "(+ 1 2)" "--no-autostart"))
            (declare (ignore stdout))
            (assert-eql 2 rc)
            (assert-contains stderr "No daemon"))
          (format t "  no-daemon path OK~%")

          (format t "Test: status reports not running after stop~%")
          (multiple-value-bind (rc stdout)
              (run-cli-captured '("repl-bridge" "status"))
            (assert-eql 0 rc)
            (assert-contains stdout "not running"))
          (format t "  status-after-stop OK~%"))))))

(format t "~%REPL-bridge CLI tests PASSED!~%")
(sb-ext:exit :code 0)
