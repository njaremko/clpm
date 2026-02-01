;;;; build/orchestrator.lisp - Build orchestration

(in-package #:clpm.build)

;;; Build plan

(defstruct build-task
  "A task in the build plan."
  (id nil :type (or null string))
  (system-id nil :type (or null string))
  (release-name nil :type (or null string))
  (release-version nil :type (or null string))
  (source-path nil :type (or null pathname))
  (tree-sha256 nil :type (or null string))
  (systems nil :type list)           ; systems to build
  (depends-on nil :type list)        ; task IDs this depends on
  (status :pending :type keyword)    ; :pending :building :done :failed
  (build-id nil :type (or null string)))

(defstruct build-plan
  "A complete build plan."
  (tasks nil :type list)             ; list of build-task
  (task-map nil)                     ; hash table id -> task
  (order nil :type list))            ; topologically sorted task IDs

;;; Create build plan from resolution

(defun create-build-plan (resolution lockfile source-paths)
  "Create a build plan from RESOLUTION.
SOURCE-PATHS is alist of (system-id . source-path)."
  (let ((plan (make-build-plan
               :task-map (make-hash-table :test 'equal)))
        (system-to-task (make-hash-table :test 'equal)))
    ;; Create tasks for each unique release
    (let ((seen-releases (make-hash-table :test 'equal)))
      (dolist (sys (clpm.solver:resolution-systems resolution))
        (let* ((release-ref (getf sys :release-ref))
               (system-id (getf sys :system)))
          (unless (gethash release-ref seen-releases)
            (setf (gethash release-ref seen-releases) t)
            ;; Find source path
            (let* ((source-entry (assoc system-id source-paths :test #'string=))
                   (source-path (when source-entry (cdr source-entry)))
                   ;; Find locked info
                   (locked-sys (find system-id
                                     (clpm.project:lockfile-resolved lockfile)
                                     :key #'clpm.project:locked-system-id
                                     :test #'string=))
                   (locked-rel (when locked-sys
                                 (clpm.project:locked-system-release locked-sys)))
                   (task (make-build-task
                          :id release-ref
                          :system-id system-id
                          :release-name (getf sys :name)
                          :release-version (getf sys :version)
                          :source-path source-path
                          :tree-sha256 (when locked-rel
                                         (clpm.project:locked-release-tree-sha256
                                          locked-rel))
                          :systems (list system-id))))
              (push task (build-plan-tasks plan))
              (setf (gethash release-ref (build-plan-task-map plan)) task)
              (setf (gethash system-id system-to-task) task))))))
    ;; Set up dependencies
    (dolist (task (build-plan-tasks plan))
      (let ((system-id (build-task-system-id task)))
        ;; Find dependencies from resolution graph
        (let ((deps (cdr (assoc system-id
                                (clpm.solver:resolution-graph resolution)
                                :test #'string=))))
          (dolist (dep deps)
            (let ((dep-task (gethash dep system-to-task)))
              (when (and dep-task
                         (not (string= (build-task-id dep-task)
                                       (build-task-id task))))
                (pushnew (build-task-id dep-task)
                         (build-task-depends-on task)
                         :test #'string=)))))))
    ;; Compute topological order
    (setf (build-plan-order plan) (topological-sort-plan plan))
    plan))

(defun topological-sort-plan (plan)
  "Return task IDs in topological order (dependencies first)."
  (let ((visited (make-hash-table :test 'equal))
        (result '()))
    (labels ((visit (task-id)
               (unless (gethash task-id visited)
                 (setf (gethash task-id visited) :in-progress)
                 (let ((task (gethash task-id (build-plan-task-map plan))))
                   (when task
                     (dolist (dep (build-task-depends-on task))
                       (visit dep))))
                 (setf (gethash task-id visited) t)
                 (push task-id result))))
      (dolist (task (build-plan-tasks plan))
        (visit (build-task-id task))))
    (nreverse result)))

;;; Execute build plan

(defun build-all (resolution lockfile source-paths
                  &key (jobs 1) compile-options)
  "Build all releases in RESOLUTION.
SOURCE-PATHS is alist of (system-id . source-path).
JOBS is number of parallel build jobs (currently sequential only).
Returns list of (system-id . build-id) pairs."
  (declare (ignore jobs))  ; TODO: implement parallel builds
  (let ((plan (create-build-plan resolution lockfile source-paths))
        (results '())
        (built-sources (make-hash-table :test 'equal)))  ; tree-sha256 -> build-id
    ;; Build in order
    (dolist (task-id (build-plan-order plan))
      (let ((task (gethash task-id (build-plan-task-map plan))))
        (when task
          ;; Check if already built (same source)
          (let* ((tree-sha256 (build-task-tree-sha256 task))
                 (existing (when tree-sha256
                             (gethash tree-sha256 built-sources))))
            (if existing
                ;; Reuse existing build
                (progn
                  (setf (build-task-build-id task) existing
                        (build-task-status task) :done)
                  (push (cons (build-task-system-id task) existing) results))
                ;; Need to build
                (when (build-task-source-path task)
                  ;; Collect dependency source paths
                  (let ((dep-source-dirs '()))
                    (dolist (dep-id (build-task-depends-on task))
                      (let ((dep-task (gethash dep-id (build-plan-task-map plan))))
                        (when (and dep-task (build-task-source-path dep-task))
                          (pushnew (build-task-source-path dep-task)
                                   dep-source-dirs
                                   :test #'equal))))
                    ;; Run build
                    (setf (build-task-status task) :building)
                    (handler-case
                        (let ((build-id
                                (build-release (build-task-source-path task)
                                               (build-task-systems task)
                                               tree-sha256
                                               dep-source-dirs
                                               :compile-options compile-options)))
                          (setf (build-task-build-id task) build-id
                                (build-task-status task) :done)
                          (when tree-sha256
                            (setf (gethash tree-sha256 built-sources) build-id))
                          (push (cons (build-task-system-id task) build-id) results))
                      (error (c)
                        (setf (build-task-status task) :failed)
                        (error c))))))))))
    (nreverse results)))

;;; Check native dependencies

(defun check-native-deps (lockfile)
  "Check that native dependencies are available.
Signals clpm-missing-native-dep-error if any are missing."
  (dolist (locked (clpm.project:lockfile-resolved lockfile))
    (let* ((system-id (clpm.project:locked-system-id locked))
           ;; Native deps would be in release metadata
           ;; For now this is a placeholder
           (native-deps nil))
      (declare (ignore system-id))
      (dolist (dep native-deps)
        (let ((kind (car dep))
              (name (cadr dep)))
          (unless (check-native-dep kind name)
            (error 'clpm.errors:clpm-missing-native-dep-error
                   :native-dep (format nil "~A:~A" kind name)
                   :required-by system-id
                   :install-hints (native-dep-install-hints kind name))))))))

(defun check-native-dep (kind name)
  "Check if native dependency is available."
  (case kind
    (:pkg-config
     (multiple-value-bind (output error-output exit-code)
         (clpm.platform:run-program
          (list "pkg-config" "--exists" name))
       (declare (ignore output error-output))
       (zerop exit-code)))
    (:brew
     (when (clpm.platform:which "brew")
       (multiple-value-bind (output error-output exit-code)
           (clpm.platform:run-program
            (list "brew" "list" name))
         (declare (ignore output error-output))
         (zerop exit-code))))
    (:apt
     (when (clpm.platform:which "dpkg")
       (multiple-value-bind (output error-output exit-code)
           (clpm.platform:run-program
            (list "dpkg" "-s" name))
         (declare (ignore output error-output))
         (zerop exit-code))))
    (t nil)))

(defun native-dep-install-hints (kind name)
  "Return install hints for native dependency."
  (case kind
    (:pkg-config
     (list (format nil "# Install ~A development package" name)
           #+linux (format nil "sudo apt install lib~A-dev  # Debian/Ubuntu" name)
           #+darwin (format nil "brew install ~A  # macOS" name)))
    (:brew
     (list (format nil "brew install ~A" name)))
    (:apt
     (list (format nil "sudo apt install ~A" name)))
    (t nil)))
