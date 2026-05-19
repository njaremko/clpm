;;;; concurrent-state-test.lisp - Verify locked read-modify-write of global state
;;;;
;;;; Tests two layers of concurrency: multiple threads inside one SBCL process
;;;; and multiple SBCL processes against the same global state files. Without
;;;; locking, both lose entries under contention.

(require :asdf)
(require :sb-posix)
(push (truename "./") asdf:*central-registry*)
(handler-case
    (asdf:load-system :clpm :verbose nil)
  (error (c)
    (format *error-output* "Load failed: ~A~%" c)
    (sb-ext:exit :code 1)))

(format t "Loading CLPM... ok~%")

(defun fail (fmt &rest args)
  (apply #'format *error-output* (concatenate 'string "FAIL: " fmt "~%") args)
  (sb-ext:exit :code 1))

;; Isolate global state. The parent picks a fresh CLPM_HOME and exports it so
;; child workers (spawned via sb-ext:run-program inheriting posix-environ) all
;; agree on a per-run-private projects.sxp / config.sxp. Without this the test
;; reads the real ~/.local/share/clpm/projects.sxp and any prior CLPM usage on
;; the machine trips the absolute-count assertions.
(let ((argv (uiop:command-line-arguments)))
  (unless (and argv (string= (first argv) "--worker"))
    ;; Reseed RANDOM so the tmpdir varies between invocations (without this,
    ;; SBCL's default state would pick the same path every run and pollute it).
    (setf *random-state* (make-random-state t))
    (let ((tmp (format nil "/tmp/clpm-conc-home-~A-~A/"
                       (sb-posix:getpid)
                       (random (expt 2 32)))))
      (ensure-directories-exist tmp)
      (sb-posix:setenv "CLPM_HOME" tmp 1))))

;;; ---- Worker mode: invoked by child SBCL processes ---------------------------

(let ((argv (uiop:command-line-arguments)))
  (when (and argv (string= (first argv) "--worker"))
    ;; --worker <kind> <id> <count>
    (let* ((kind (second argv))
           (id (parse-integer (third argv)))
           (count (parse-integer (fourth argv))))
      (cond
        ((string= kind "projects")
         (loop for j below count do
           (let ((dir (format nil "/tmp/clpm-conc-proj/proj-~A-~A/" id j)))
             (ensure-directories-exist (uiop:ensure-directory-pathname dir))
             (clpm.store:upsert-project-index-root dir))))
        ((string= kind "config")
         (loop for j below count do
           (clpm.config:update-config
            (let ((name (format nil "reg-~A-~A" id j)))
              (lambda (cfg)
                (push (clpm.project::make-registry-ref
                       :kind :git
                       :name name
                       :url (format nil "https://example.invalid/~A.git" name)
                       :trust nil)
                      (clpm.config:config-registries cfg))
                cfg)))))
        (t
         (fail "unknown worker kind: ~A" kind))))
    (sb-ext:exit :code 0)))

;;; ---- Driver helpers ---------------------------------------------------------

(defun spawn-worker (kind id count)
  "Fork an SBCL process that runs the worker for KIND. Returns the PROCESS."
  (sb-ext:run-program
   "sbcl"
   (list "--script" "test/concurrent-state-test.lisp"
         "--worker" kind
         (princ-to-string id)
         (princ-to-string count))
   :search t
   :wait nil
   :output nil
   :error nil
   :environment (sb-ext:posix-environ)))

(defun await-procs (processes)
  (let ((all-ok t))
    (dolist (p processes)
      (sb-ext:process-wait p)
      (let ((rc (sb-ext:process-exit-code p)))
        (unless (zerop rc)
          (format *error-output* "worker exited rc=~D~%" rc)
          (setf all-ok nil))))
    all-ok))

;;; ---- Test 1: concurrent inter-process upsert into projects.sxp -------------

(defun test-concurrent-projects-index ()
  (format t "Concurrent projects index upsert (inter-process)... ")
  (ignore-errors (uiop:delete-directory-tree
                  (uiop:ensure-directory-pathname "/tmp/clpm-conc-proj/")
                  :validate t :if-does-not-exist :ignore))
  (let* ((n-procs 8)
         (per-proc 12)
         (expected (* n-procs per-proc))
         (procs (loop for i below n-procs
                      collect (spawn-worker "projects" i per-proc))))
    (unless (await-procs procs)
      (fail "one or more projects-index workers failed"))
    (multiple-value-bind (roots found)
        (clpm.store:read-project-index-roots)
      (unless found (fail "projects index missing after upserts"))
      (let ((got (length roots)))
        (unless (= got expected)
          (fail "expected ~D roots, got ~D" expected got)))))
  (format t "ok~%"))

;;; ---- Test 2: concurrent inter-process update-config ------------------------

(defun test-concurrent-config-updates ()
  (format t "Concurrent config update (inter-process)... ")
  ;; Start fresh.
  (let ((cfg-path (merge-pathnames "config.sxp" (clpm.platform:config-dir))))
    (when (uiop:file-exists-p cfg-path)
      (delete-file cfg-path)))
  (let* ((n-procs 6)
         (per-proc 8)
         (expected (* n-procs per-proc))
         (procs (loop for i below n-procs
                      collect (spawn-worker "config" i per-proc))))
    (unless (await-procs procs)
      (fail "one or more config workers failed"))
    (let* ((cfg (clpm.config:read-config))
           (regs (clpm.config:config-registries cfg))
           (got (length regs)))
      (unless (= got expected)
        (fail "expected ~D registries, got ~D" expected got))))
  (format t "ok~%"))

;;; ---- Test 3: intra-process thread-mutex layer ------------------------------

(defun test-intra-process-projects-index ()
  (format t "Concurrent projects index upsert (intra-process threads)... ")
  ;; Use a different parent dir than test 1 so we don't carry over entries.
  (let* ((tmp-root (uiop:ensure-directory-pathname
                    (format nil "/tmp/clpm-conc-threads-~A/"
                            (random (expt 2 32)))))
         (n-threads 6)
         (per-thread 10)
         (expected (* n-threads per-thread)))
    (unwind-protect
         (progn
           (ensure-directories-exist tmp-root)
           ;; Pre-create directories so `truename` succeeds.
           (loop for tid below n-threads do
             (loop for j below per-thread do
               (ensure-directories-exist
                (uiop:ensure-directory-pathname
                 (format nil "~Aproj-~A-~A/" tmp-root tid j)))))
           (let* ((before-count
                    (multiple-value-bind (r found)
                        (clpm.store:read-project-index-roots)
                      (if found (length r) 0)))
                  (threads
                    (loop for tid below n-threads
                          collect (sb-thread:make-thread
                                   (let ((my-tid tid))
                                     (lambda ()
                                       (loop for j below per-thread do
                                         (clpm.store:upsert-project-index-root
                                          (format nil "~Aproj-~A-~A/"
                                                  tmp-root my-tid j)))))
                                   :name "clpm.test.thread-upsert"))))
             (dolist (th threads) (sb-thread:join-thread th))
             (multiple-value-bind (roots found)
                 (clpm.store:read-project-index-roots)
               (unless found (fail "projects index missing"))
               (let ((added (- (length roots) before-count)))
                 (unless (= added expected)
                   (fail "intra-process: expected ~D new roots, got ~D"
                         expected added))))))
      (ignore-errors (uiop:delete-directory-tree tmp-root :validate t)))
    (format t "ok~%")))

(test-concurrent-projects-index)
(test-concurrent-config-updates)
(test-intra-process-projects-index)
(format t "~%Concurrent state tests PASSED!~%")
