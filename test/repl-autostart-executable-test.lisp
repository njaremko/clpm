;;;; test/repl-autostart-executable-test.lisp - saved executable autostart.

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

(defun assert-contains (haystack needle)
  (unless (and (stringp haystack) (search needle haystack))
    (fail "expected ~S in:~%~A" needle haystack)))

(defun write-pidfile (project-root pid)
  (let ((pidfile (merge-pathnames ".clpm/repl.pid" project-root)))
    (ensure-directories-exist pidfile)
    (with-open-file (s pidfile :direction :output :if-exists :supersede
                               :if-does-not-exist :create
                               :external-format :utf-8)
      (format s "~D~%" pid))))

(defun point-socket-at (project-root target)
  (let ((socket (merge-pathnames ".clpm/repl.sock" project-root)))
    (ensure-directories-exist socket)
    (ignore-errors (delete-file socket))
    (sb-posix:symlink target (namestring socket))))

(defun make-short-temp-dir ()
  "Create a short temp directory so Unix-domain socket paths stay portable."
  (let* ((template (namestring
                    (merge-pathnames "raXXXXXX" (uiop:temporary-directory))))
         (dir (sb-posix:mkdtemp template)))
    (uiop:ensure-directory-pathname dir)))

(defmacro with-short-temp-dir ((var) &body body)
  `(let ((,var (make-short-temp-dir)))
     (unwind-protect
          (progn ,@body)
       (ignore-errors
         (uiop:delete-directory-tree ,var :validate t)))))

(defun run-program-captured (argv &key directory)
  (multiple-value-bind (stdout stderr code)
      (uiop:run-program argv
                        :output :string
                        :error-output :string
                        :directory directory
                        :ignore-error-status t)
    (values code stdout stderr)))

(defun build-clpm-executable (repo-root output-path)
  (let ((sbcl (or (uiop:getenv "SBCL") "sbcl")))
    (multiple-value-bind (rc stdout stderr)
        (run-program-captured
         (list sbcl
               "--noinform"
               "--non-interactive"
               "--eval" "(require :asdf)"
               "--eval" (format nil "(push #P~S asdf:*central-registry*)"
                                 (namestring repo-root))
               "--eval" "(asdf:load-system :clpm :verbose nil)"
               "--eval" (format nil "(clpm::build-executable ~S)"
                                 (namestring output-path))))
      (unless (zerop rc)
        (fail "could not build clpm executable~%stdout:~%~A~%stderr:~%~A"
              stdout stderr)))))

(defun write-minimal-project (project-root)
  (let ((manifest (merge-pathnames "clpm.project" project-root))
        (clpm-dir (merge-pathnames ".clpm/" project-root)))
    (ensure-directories-exist clpm-dir)
    (with-open-file (s manifest :direction :output :if-exists :supersede
                                :external-format :utf-8)
      (write-string "(:project :name \"autostart\" :version \"0.1.0\" :systems () :registries ())" s))
    (with-open-file (s (merge-pathnames "asdf-config.lisp" clpm-dir)
                       :direction :output :if-exists :supersede
                       :external-format :utf-8)
      (write-string ";; empty for test~%" s))))

(defun write-workspace (workspace-root members)
  (ensure-directories-exist workspace-root)
  (clpm.workspace:write-workspace-file
   (clpm.workspace:make-workspace :format 1 :members members)
   (merge-pathnames "clpm.workspace" workspace-root)))

(let* ((this-file (or *load-truename* *load-pathname*))
       (test-dir (uiop:pathname-directory-pathname this-file))
       (repo-root (uiop:pathname-parent-directory-pathname test-dir)))
  (with-short-temp-dir (tmp)
    (let* ((exe (merge-pathnames "clpm" tmp))
           (proj-a (merge-pathnames "proj-a/" tmp))
           (proj-b (merge-pathnames "proj-b/" tmp))
           (sock-a (merge-pathnames ".clpm/repl.sock" proj-a))
           (sock-b (merge-pathnames ".clpm/repl.sock" proj-b))
           (ws-root (merge-pathnames "workspace/" tmp))
           (ws-a (merge-pathnames "app/" ws-root))
           (ws-b (merge-pathnames "lib/" ws-root))
           (ws-sock-a (merge-pathnames ".clpm/repl.sock" ws-a))
           (ws-sock-b (merge-pathnames ".clpm/repl.sock" ws-b)))
      (format t "Test: saved executable autostarts repl daemon~%")
      (build-clpm-executable repo-root exe)
      (write-minimal-project proj-a)
      (write-minimal-project proj-b)
      (write-workspace ws-root '("app" "lib"))
      (write-minimal-project ws-a)
      (write-minimal-project ws-b)
      (flet ((clpm-eval (project form)
               (run-program-captured
                (list (namestring exe) "repl" "eval" form)
                :directory project))
             (clpm-workspace-eval (member form)
               (run-program-captured
                (list (namestring exe) "-p" member "repl" "eval" form)
                :directory ws-root))
             (clpm-call (project method)
               (run-program-captured
                (list (namestring exe) "repl" "call" method)
                :directory project))
             (stop-daemon (project)
               (ignore-errors
                 (run-program-captured
                  (list (namestring exe) "repl" "daemon" "--stop")
                  :directory project))))
        (unwind-protect
             (progn
               (format t "Test: repl call does not autostart daemon~%")
               (multiple-value-bind (rc stdout stderr)
                   (clpm-call proj-a "methods")
                 (when (zerop rc)
                   (stop-daemon proj-a)
                   (fail "expected call without daemon to fail~%stdout:~%~A~%stderr:~%~A"
                         stdout stderr))
                 (assert-eql 2 rc)
                 (when (probe-file sock-a)
                   (stop-daemon proj-a)
                   (fail "repl call created daemon socket: ~A" sock-a))
                 (assert-contains stderr "No daemon running"))
               (format t "  call lifecycle isolation OK~%")

               (multiple-value-bind (rc stdout stderr)
                   (clpm-eval proj-a "(+ 1 2)")
                 (unless (zerop rc)
                   (fail "expected eval autostart to succeed, got rc ~D~%stdout:~%~A~%stderr:~%~A"
                         rc stdout stderr))
                 (assert-contains stdout "=> 3")
                 (format t "  saved executable autostart OK~%"))

               (format t "Test: project repl daemons are isolated~%")
               (multiple-value-bind (rc stdout stderr)
                   (clpm-eval proj-a
                              "(defparameter *clpm-repl-isolation-token* :project-a)")
                 (unless (zerop rc)
                   (fail "project A token definition failed: ~D~%stdout:~%~A~%stderr:~%~A"
                         rc stdout stderr))
                 (assert-contains stdout "=> *CLPM-REPL-ISOLATION-TOKEN*"))
               (multiple-value-bind (rc stdout stderr)
                   (clpm-eval proj-a "*clpm-repl-isolation-token*")
                 (unless (zerop rc)
                   (fail "project A token lookup failed: ~D~%stdout:~%~A~%stderr:~%~A"
                         rc stdout stderr))
                 (assert-contains stdout "=> :PROJECT-A"))
               (write-pidfile proj-b (sb-posix:getpid))
               (point-socket-at proj-b (namestring sock-a))
               (multiple-value-bind (rc stdout stderr)
                   (clpm-eval proj-b
                              "(boundp '*clpm-repl-isolation-token*)")
                 (unless (zerop rc)
                   (fail "project B isolation check failed: ~D~%stdout:~%~A~%stderr:~%~A"
                         rc stdout stderr))
                 (assert-contains stdout "=> NIL"))
               (multiple-value-bind (rc stdout stderr)
                   (clpm-eval proj-b
                              "(defparameter *clpm-repl-isolation-token* :project-b)")
                 (unless (zerop rc)
                   (fail "project B token definition failed: ~D~%stdout:~%~A~%stderr:~%~A"
                         rc stdout stderr))
                 (assert-contains stdout "=> *CLPM-REPL-ISOLATION-TOKEN*"))
               (multiple-value-bind (rc stdout stderr)
                   (clpm-eval proj-a "*clpm-repl-isolation-token*")
                 (unless (zerop rc)
                   (fail "project A token lookup failed: ~D~%stdout:~%~A~%stderr:~%~A"
                         rc stdout stderr))
                 (assert-contains stdout "=> :PROJECT-A"))
               (format t "  project repl isolation OK~%")

               (format t "Test: workspace member repl autostart is isolated~%")
               (multiple-value-bind (rc stdout stderr)
                   (clpm-workspace-eval "app"
                                        "(defparameter *clpm-workspace-token* :app)")
                 (unless (zerop rc)
                   (fail "workspace app eval failed: ~D~%stdout:~%~A~%stderr:~%~A"
                         rc stdout stderr))
                 (assert-contains stdout "=> *CLPM-WORKSPACE-TOKEN*"))
               (multiple-value-bind (rc stdout stderr)
                   (clpm-workspace-eval "app" "*clpm-workspace-token*")
                 (unless (zerop rc)
                   (fail "workspace app token lookup failed: ~D~%stdout:~%~A~%stderr:~%~A"
                         rc stdout stderr))
                 (assert-contains stdout "=> :APP"))
               (multiple-value-bind (rc stdout stderr)
                   (clpm-workspace-eval "lib"
                                        "(boundp '*clpm-workspace-token*)")
                 (unless (zerop rc)
                   (fail "workspace lib isolation check failed: ~D~%stdout:~%~A~%stderr:~%~A"
                         rc stdout stderr))
                 (assert-contains stdout "=> NIL"))
               (format t "  workspace member repl isolation OK~%"))
          (stop-daemon proj-a)
          (stop-daemon proj-b)
          (stop-daemon ws-a)
          (stop-daemon ws-b)
          (loop for i from 0 below 30
                while (or (probe-file sock-a)
                          (probe-file sock-b)
                          (probe-file ws-sock-a)
                          (probe-file ws-sock-b))
                do (sleep 0.1)))))))

(format t "~%REPL executable autostart test PASSED!~%")
(sb-ext:exit :code 0)
