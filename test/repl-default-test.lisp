;;;; test/repl-default-test.lisp - bare `clpm repl' defaults.

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

(defun assert-not-contains (haystack needle)
  (when (and (stringp haystack) (search needle haystack))
    (fail "did not expect ~S in:~%~A" needle haystack)))

(defun run-cli-captured (args)
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
                    (merge-pathnames "rdXXXXXX" (uiop:temporary-directory))))
         (dir (sb-posix:mkdtemp template)))
    (uiop:ensure-directory-pathname dir)))

(defmacro with-short-temp-dir ((var) &body body)
  `(let ((,var (make-short-temp-dir)))
     (unwind-protect
          (progn ,@body)
       (ignore-errors
         (uiop:delete-directory-tree ,var :validate t)))))

(defun write-minimal-project (project-root)
  (let ((manifest (merge-pathnames "clpm.project" project-root))
        (clpm-dir (merge-pathnames ".clpm/" project-root)))
    (ensure-directories-exist clpm-dir)
    (with-open-file (s manifest :direction :output :if-exists :supersede
                                :external-format :utf-8)
      (write-string
       "(:project :name \"default\" :version \"0.1.0\" :systems () :registries ())"
       s))
    (with-open-file (s (merge-pathnames "asdf-config.lisp" clpm-dir)
                       :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (write-string ";; empty for test~%" s))))

(defun write-executable-file (path contents)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output :if-exists :supersede
                          :external-format :utf-8)
    (write-string contents s))
  (sb-posix:chmod (namestring path) #o755))

(with-short-temp-dir (tmp)
  (let* ((project-root (merge-pathnames "app/" tmp))
         (socket (merge-pathnames ".clpm/repl.sock" project-root)))
    (write-minimal-project project-root)
    (uiop:with-current-directory (project-root)
      (unwind-protect
           (progn
             (format t "Test: bare non-interactive repl starts detached daemon~%")
             (multiple-value-bind (rc stdout stderr)
                 (run-cli-captured '("repl"))
               (unless (zerop rc)
                 (fail "expected bare repl to start daemon, rc ~D~%stdout:~%~A~%stderr:~%~A"
                       rc stdout stderr))
               (loop for i from 0 below 100
                     while (not (probe-file socket))
                     do (sleep 0.1))
               (assert-true (probe-file socket)
                            "bare repl did not create daemon socket: ~A"
                            socket))
             (multiple-value-bind (rc stdout stderr)
                 (run-cli-captured '("repl" "--non-interactive"))
               (unless (zerop rc)
                 (fail "expected forced non-interactive repl to be idempotent, rc ~D~%stdout:~%~A~%stderr:~%~A"
                       rc stdout stderr))
               (assert-true (probe-file socket)
                            "daemon socket disappeared after forced non-interactive repl: ~A"
                            socket))
             (format t "  bare non-interactive repl OK~%"))
        (ignore-errors
          (run-cli-captured '("repl" "daemon" "--stop")))))))

(format t "Test: repl mode flags reject conflict~%")
(multiple-value-bind (rc stdout stderr)
    (run-cli-captured '("repl" "--interactive" "--non-interactive"))
  (declare (ignore stdout))
  (assert-eql 1 rc)
  (assert-contains stderr "Use only one of --interactive or --non-interactive"))
(format t "  repl mode conflict OK~%")

(with-short-temp-dir (tmp)
  (let* ((project-root (merge-pathnames "eval-autostart/" tmp))
         (socket (merge-pathnames ".clpm/repl.sock" project-root)))
    (write-minimal-project project-root)
    (uiop:with-current-directory (project-root)
      (unwind-protect
           (progn
             (format t "Test: source-loaded eval autostarts detached daemon~%")
             (multiple-value-bind (rc stdout stderr)
                 (run-cli-captured '("repl" "eval" "(+ 2 3)"))
               (unless (zerop rc)
                 (fail "expected eval autostart to succeed, rc ~D~%stdout:~%~A~%stderr:~%~A"
                       rc stdout stderr))
               (assert-contains stdout "=> 5")
               (assert-true (probe-file socket)
                            "eval autostart did not leave daemon socket: ~A"
                            socket))
             (format t "  source-loaded eval autostart OK~%"))
        (ignore-errors
          (run-cli-captured '("repl" "daemon" "--stop")))))))

(with-short-temp-dir (tmp)
  (let* ((project-root (merge-pathnames "interactive/" tmp))
         (bin-dir (merge-pathnames "bin/" tmp))
         (fake-sbcl (merge-pathnames "sbcl" bin-dir))
         (argv-file (merge-pathnames "repl-argv.txt" project-root))
         (pwd-file (merge-pathnames "repl-pwd.txt" project-root))
         (old-path (sb-posix:getenv "PATH")))
    (write-minimal-project project-root)
    (write-executable-file
     fake-sbcl
     "#!/bin/sh
printf '%s\n' \"$@\" > repl-argv.txt
pwd > repl-pwd.txt
exit 0
")
    (unwind-protect
         (progn
           (sb-posix:setenv "PATH"
                            (concatenate 'string
                                         (namestring bin-dir)
                                         ":"
                                         (or old-path ""))
                            1)
           (uiop:with-current-directory (project-root)
             (format t "Test: forced interactive repl starts foreground Lisp~%")
             (multiple-value-bind (rc stdout stderr)
                 (run-cli-captured '("repl" "--interactive"))
               (declare (ignore stderr))
               (assert-eql 0 rc)
               (assert-contains stdout "Starting sbcl project REPL")
               (assert-true (probe-file argv-file)
                            "interactive repl did not run fake sbcl")
               (let ((argv (uiop:read-file-string argv-file))
                     (pwd (uiop:ensure-directory-pathname
                           (string-trim '(#\Newline #\Return)
                                        (uiop:read-file-string pwd-file)))))
                 (assert-contains argv "--load")
                 (assert-not-contains argv "--non-interactive")
                 (assert-not-contains argv "--disable-debugger")
                 (assert-true (string= (namestring (truename pwd))
                                       (namestring (truename project-root)))
                              "expected repl cwd ~S, got ~S"
                              (namestring (truename project-root))
                              (namestring (truename pwd)))))
             (format t "  forced interactive repl OK~%")))
      (if old-path
          (sb-posix:setenv "PATH" old-path 1)
          (sb-posix:unsetenv "PATH")))))

(format t "~%REPL default tests PASSED!~%")
(sb-ext:exit :code 0)
