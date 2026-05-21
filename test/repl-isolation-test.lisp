;;;; test/repl-isolation-test.lisp - project-local repl daemon state.

(require :asdf)
(require :sb-posix)

(defparameter *repo-root*
  (let* ((this-file (or *load-truename* *load-pathname*))
         (test-dir (uiop:pathname-directory-pathname this-file)))
    (uiop:pathname-parent-directory-pathname test-dir)))

(push *repo-root* asdf:*central-registry*)

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

(defun run-cli-captured (args &key directory)
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((*standard-output* out)
          (*error-output* err))
      (let ((rc (if directory
                    (uiop:with-current-directory (directory)
                      (clpm:run-cli args))
                    (clpm:run-cli args))))
        (values rc
                (get-output-stream-string out)
                (get-output-stream-string err))))))

(defun make-short-temp-dir ()
  "Create a short temp directory so Unix-domain socket paths stay portable."
  (let* ((template (namestring
                    (merge-pathnames "riXXXXXX" (uiop:temporary-directory))))
         (dir (sb-posix:mkdtemp template)))
    (uiop:ensure-directory-pathname dir)))

(defmacro with-short-temp-dir ((var) &body body)
  `(let ((,var (make-short-temp-dir)))
     (unwind-protect
          (progn ,@body)
       (ignore-errors
         (uiop:delete-directory-tree ,var :validate t)))))

(defun write-minimal-project (project-root name)
  (let ((manifest (merge-pathnames "clpm.project" project-root))
        (clpm-dir (merge-pathnames ".clpm/" project-root)))
    (ensure-directories-exist clpm-dir)
    (with-open-file (s manifest :direction :output :if-exists :supersede
                                :external-format :utf-8)
      (format s "(:project :name ~S :version \"0.1.0\" :systems () :registries ())"
              name))
    (with-open-file (s (merge-pathnames "asdf-config.lisp" clpm-dir)
                       :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (write-string ";; empty for test~%" s))))

(defun write-workspace (workspace-root members)
  (ensure-directories-exist workspace-root)
  (clpm.workspace:write-workspace-file
   (clpm.workspace:make-workspace :format 1 :members members)
   (merge-pathnames "clpm.workspace" workspace-root)))

(defun start-daemon-thread (directory name &optional (args '("repl" "daemon")))
  (sb-thread:make-thread
   (lambda ()
     (handler-case
         (run-cli-captured args :directory directory)
       (error (c)
         (format *error-output* "~A daemon thread died: ~A~%" name c)
         (force-output *error-output*))))
   :name (format nil "test-repl-isolation-~A" name)))

(defun wait-for-socket (path)
  (loop for i from 0 below 100
        while (not (probe-file path))
        do (sleep 0.1))
  (assert-true (probe-file path) "daemon socket did not appear: ~A" path))

(defun stop-daemon (project-root)
  (ignore-errors
    (run-cli-captured '("repl" "call" "debug-abort")
                      :directory project-root))
  (ignore-errors
    (run-cli-captured '("repl" "daemon" "--stop")
                      :directory project-root)))

(with-short-temp-dir (tmp)
  (let* ((project-a (merge-pathnames "project-a/" tmp))
         (project-b (merge-pathnames "project-b/" tmp))
         (sock-a (namestring (merge-pathnames ".clpm/repl.sock" project-a)))
         (sock-b (namestring (merge-pathnames ".clpm/repl.sock" project-b))))
    (write-minimal-project project-a "project-a")
    (write-minimal-project project-b "project-b")
    (format t "Test: two foreground project daemons do not share sessions~%")
    (let ((thread-a (start-daemon-thread project-a "project-a"))
          (thread-b nil))
      (sleep 0.05)
      (wait-for-socket sock-a)
      (setf thread-b (start-daemon-thread project-b "project-b"))
      (sleep 0.05)
      (wait-for-socket sock-b)
      (unwind-protect
           (progn
             (multiple-value-bind (rc _stdout stderr)
                 (run-cli-captured '("repl" "eval"
                                     "(error \"project-a-only\")"
                                     "--debug" "--keep"
                                     "--no-autostart")
                                   :directory project-a)
               (declare (ignore _stdout))
               (assert-eql 3 rc)
               (assert-contains stderr "session:"))
             (multiple-value-bind (rc stdout)
                 (run-cli-captured '("repl" "call" "list-debug-sessions"
                                     "--no-autostart")
                                   :directory project-a)
               (assert-eql 0 rc)
               (assert-contains stdout "project-a-only"))
             (multiple-value-bind (rc stdout)
                 (run-cli-captured '("repl" "call" "list-debug-sessions"
                                     "--no-autostart")
                                   :directory project-b)
               (assert-eql 0 rc)
               (assert-not-contains stdout "project-a-only"))
             (format t "  project session isolation OK~%"))
        (stop-daemon project-a)
        (stop-daemon project-b)
        (loop for i from 0 below 30
              while (or (and thread-a (sb-thread:thread-alive-p thread-a))
                        (and thread-b (sb-thread:thread-alive-p thread-b)))
              do (sleep 0.1))
        (when (and thread-a (sb-thread:thread-alive-p thread-a))
          (ignore-errors (sb-thread:terminate-thread thread-a)))
        (when (and thread-b (sb-thread:thread-alive-p thread-b))
          (ignore-errors (sb-thread:terminate-thread thread-b)))))))

(sb-posix:chdir (namestring *repo-root*))

(with-short-temp-dir (tmp)
  (let* ((workspace-root (merge-pathnames "workspace/" tmp))
         (app-root (merge-pathnames "app/" workspace-root))
         (lib-root (merge-pathnames "lib/" workspace-root))
         (sock-app (namestring (merge-pathnames ".clpm/repl.sock" app-root))))
    (write-workspace workspace-root '("app" "lib"))
    (write-minimal-project app-root "app")
    (write-minimal-project lib-root "lib")
    (format t "Test: foreground workspace member daemon uses member cwd~%")
    (let ((thread (start-daemon-thread
                   workspace-root
                   "workspace-app"
                   '("-p" "app" "repl" "daemon"))))
      (sleep 0.05)
      (wait-for-socket sock-app)
      (unwind-protect
           (progn
             (let ((resp (clpm.repl:send-request
                          sock-app
                          "eval"
                          :params (list :object
                                        (list
                                         (cons "project_root"
                                               (namestring
                                                (uiop:ensure-directory-pathname
                                                 (truename app-root))))
                                         (cons "form"
                                               "(namestring (uiop:getcwd))"))))))
               (assert-contains (prin1-to-string resp)
                                (namestring (truename app-root))))
             (multiple-value-bind (rc stdout stderr)
                 (run-cli-captured
                  '("-p" "app" "repl" "eval"
                    "(namestring *default-pathname-defaults*)"
                    "--no-autostart")
                  :directory workspace-root)
               (unless (zerop rc)
                 (fail "workspace app cwd eval failed: ~D~%stdout:~%~A~%stderr:~%~A"
                       rc stdout stderr))
               (assert-contains stdout (format nil "=> ~S"
                                               (namestring (truename app-root)))))
             (format t "  workspace foreground cwd isolation OK~%"))
        (stop-daemon app-root)
        (loop for i from 0 below 30
              while (and thread (sb-thread:thread-alive-p thread))
              do (sleep 0.1))
        (when (and thread (sb-thread:thread-alive-p thread))
          (ignore-errors (sb-thread:terminate-thread thread)))))))

(format t "~%REPL isolation tests PASSED!~%")
(sb-ext:exit :code 0)
