;;;; solver/pubgrub.lisp - PubGrub-style dependency solver

(in-package #:clpm.solver)

;;; Resolution result

(defstruct resolution
  "Result of dependency resolution."
  (systems nil :type list)  ; alist of (system-id . selected-release)
  (graph nil :type list))   ; dependency graph for topological ordering

(defstruct conflict-explanation
  "Explanation of a resolution conflict."
  (system nil :type (or null string))
  (constraint nil)
  (candidates nil :type list)
  (cause nil :type (or null string))
  (chain nil :type list))   ; list of (system constraint) pairs leading here

(defun conflict-explanation (resolve-error)
  "Return a structured conflict explanation for RESOLVE-ERROR."
  (check-type resolve-error clpm.errors:clpm-resolve-error)
  (or (clpm.errors:clpm-resolve-error-explanation resolve-error)
      (make-conflict-explanation
       :system (first (clpm.errors:clpm-resolve-error-systems resolve-error))
       :constraint nil
       :candidates nil
       :cause (clpm.errors:clpm-error-message resolve-error)
       :chain (clpm.errors:clpm-resolve-error-conflict-chain resolve-error))))

;;; Solver state

(defstruct solver-state
  "Internal state of the solver."
  (index nil)                ; registry index
  (lockfile nil)             ; existing lockfile for preferences
  (decisions nil :type list) ; alist of (system-id . release-ref)
  (constraints nil :type list) ; alist of (system-id . accumulated-constraint)
  (pending nil :type list)   ; list of system-ids to process
  (incompatibilities nil :type list) ; learned conflict clauses
  (decision-level 0 :type integer)
  (decision-stack nil :type list) ; stack of (level system-id . release-ref)
  (reasons (make-hash-table :test 'equal)) ; system-id -> list of reasons
  (extra-release-metadata nil))   ; hash-table release-ref -> release-metadata

;;; Main solver entry point

(defun solve (project registries &key lockfile)
  "Resolve dependencies for PROJECT using REGISTRIES.
If LOCKFILE is provided, prefer those selections.
Returns a resolution struct or signals clpm-resolve-error."
  (let ((index (clpm.registry:build-registry-index registries))
        (state (make-solver-state)))
    (setf (solver-state-index state) index
          (solver-state-lockfile state) lockfile
          (solver-state-reasons state) (make-hash-table :test 'equal)
          (solver-state-extra-release-metadata state)
          (make-hash-table :test 'equal))
    ;; Add root constraints from project
    (add-root-constraints state project)
    ;; Run solver
    (handler-case
        (solver-search state)
      (resolution-conflict (c)
        (error 'clpm.errors:clpm-resolve-error
               :message (resolution-conflict-message c)
               :systems (resolution-conflict-systems c)
               :conflict-chain (resolution-conflict-chain c)
               :explanation (resolution-conflict-explanation c))))
    ;; Build result
    (build-resolution state)))

(defun lookup-release-entry (state release-ref)
  "Look up RELEASE-REF and return (registry . release-metadata) or nil.

For pinned/local sources, registry is NIL."
  (or (clpm.registry:index-lookup-release (solver-state-index state) release-ref)
      (let ((meta (gethash release-ref (solver-state-extra-release-metadata state))))
        (when meta
          (cons nil meta)))))

;;; Root constraints

(defun add-root-constraints (state project)
  "Add initial constraints from project manifest."
  (dolist (dep (clpm.project:project-depends project))
    (add-dependency-constraint state dep :root))
  (dolist (dep (clpm.project:project-dev-depends project))
    (add-dependency-constraint state dep :dev))
  (dolist (dep (clpm.project:project-test-depends project))
    (add-dependency-constraint state dep :test)))

(defun record-constraint-reason (state system-id from-label constraint)
  (let* ((reasons (solver-state-reasons state))
         (entry (list :from from-label
                      :constraint (constraint-to-string constraint)))
         (existing (gethash system-id reasons)))
    (unless (member entry existing :test #'equal)
      (setf (gethash system-id reasons) (cons entry existing)))))

(defun add-dependency-constraint (state dep source)
  "Add a dependency constraint to solver state."
  (let ((system-id (clpm.project:dependency-system dep))
        (constraint (parse-constraint (clpm.project:dependency-constraint dep))))
    (record-constraint-reason
     state system-id
     (case source
       (:root "root")
       (:dev "root (dev)")
       (:test "root (test)")
       (t "root"))
     constraint)
    ;; Merge with existing constraint
    (let ((existing (assoc system-id (solver-state-constraints state) :test #'string=)))
      (if existing
          (let ((merged (constraint-intersect (cdr existing) constraint)))
            (when (constraint-empty-p merged)
              (signal-conflict state system-id constraint
                               "Constraint conflict at root level"))
            (setf (cdr existing) merged))
          (push (cons system-id constraint)
                (solver-state-constraints state))))
    ;; Add to pending if not already decided
    (unless (assoc system-id (solver-state-decisions state) :test #'string=)
      (pushnew system-id (solver-state-pending state) :test #'string=))))

;;; Resolution conflict condition

(define-condition resolution-conflict (error)
  ((message :initarg :message :reader resolution-conflict-message)
   (systems :initarg :systems :reader resolution-conflict-systems)
   (chain :initarg :chain :reader resolution-conflict-chain)
   (explanation :initarg :explanation :initform nil
                :reader resolution-conflict-explanation)))

(defun conflict-chain (state system-id)
  (let ((reasons (copy-list (gethash system-id (solver-state-reasons state)))))
    (mapcar (lambda (r)
              (format nil "~A requires ~A ~A"
                      (getf r :from)
                      system-id
                      (getf r :constraint)))
            (sort reasons #'string<
                  :key (lambda (r) (getf r :from))))))

(defun signal-conflict (state system-id constraint reason &key candidates)
  "Signal a resolution conflict."
  (let* ((chain (conflict-chain state system-id))
         (chain (if (and candidates (consp candidates))
                    (append chain
                            (list (format nil "Candidates considered: ~{~A~^, ~}"
                                          candidates)))
                    chain))
         (explanation (make-conflict-explanation
                       :system system-id
                       :constraint constraint
                       :candidates candidates
                       :cause reason
                       :chain chain)))
    (error 'resolution-conflict
           :message reason
           :systems (list system-id)
           :chain chain
           :explanation explanation)))

;;; Backtracking search solver

(defun next-pending-system (state)
  "Pick and remove the next pending system deterministically."
  (let ((pending (solver-state-pending state)))
    (when pending
      (let ((next (reduce (lambda (a b) (if (string< a b) a b))
                          pending)))
        (setf (solver-state-pending state)
              (remove next pending :test #'string= :count 1))
        next))))

(defun snapshot-state (state)
  "Create a snapshot of STATE sufficient for backtracking."
  (list :decisions (copy-list (solver-state-decisions state))
        :constraints (mapcar (lambda (c) (cons (car c) (cdr c)))
                             (solver-state-constraints state))
        :pending (copy-list (solver-state-pending state))
        :decision-level (solver-state-decision-level state)
        :decision-stack (copy-list (solver-state-decision-stack state))
        :reasons (let ((copy (make-hash-table :test 'equal)))
                   (maphash (lambda (k v)
                              (setf (gethash k copy) (copy-list v)))
                            (solver-state-reasons state))
                   copy)))

(defun restore-state (state snapshot)
  "Restore STATE from SNAPSHOT."
  (setf (solver-state-decisions state) (getf snapshot :decisions)
        (solver-state-constraints state) (getf snapshot :constraints)
        (solver-state-pending state) (getf snapshot :pending)
        (solver-state-decision-level state) (getf snapshot :decision-level)
        (solver-state-decision-stack state) (getf snapshot :decision-stack)
        (solver-state-reasons state) (getf snapshot :reasons)))

(defun decided-release-ref (state system-id)
  (cdr (assoc system-id (solver-state-decisions state) :test #'string=)))

(defun decided-version (state system-id)
  (let ((release-ref (decided-release-ref state system-id)))
    (when release-ref
      (let ((meta-entry (lookup-release-entry state release-ref)))
        (when meta-entry
          (clpm.registry:release-metadata-version (cdr meta-entry)))))))

(defun check-decision-satisfies-constraint (state system-id constraint)
  "Signal conflict if SYSTEM-ID is decided to a version that violates CONSTRAINT."
  (let ((version (decided-version state system-id)))
    (when (and version (not (constraint-satisfies-p constraint version)))
      (signal-conflict state system-id constraint
                       (format nil "Conflict: selected ~A does not satisfy ~A"
                               system-id (constraint-to-string constraint))))))

(defun ordered-candidate-refs (state system-id candidates)
  "Return candidate release refs in deterministic preference order."
  (let ((refs (mapcar #'car candidates))
        (preferred (when (solver-state-lockfile state)
                     (dolist (locked (clpm.project:lockfile-resolved
                                      (solver-state-lockfile state)))
                       (when (string= (clpm.project:locked-system-id locked) system-id)
                         (let* ((locked-rel (clpm.project:locked-system-release locked))
                                (locked-name (clpm.project:locked-release-name locked-rel))
                                (locked-version (clpm.project:locked-release-version locked-rel)))
                           (return (format nil "~A@~A" locked-name locked-version))))))))
    (if (and preferred (member preferred refs :test #'string=))
        (cons preferred (remove preferred refs :test #'string= :count 1))
        refs)))

(defun decide (state system-id release-ref)
  "Record decision that SYSTEM-ID is RELEASE-REF."
  (push (cons system-id release-ref) (solver-state-decisions state))
  (incf (solver-state-decision-level state))
  (push (list (solver-state-decision-level state) system-id release-ref)
        (solver-state-decision-stack state)))

(defun solver-search (state)
  "Depth-first backtracking search over candidate versions."
  (let ((system-id (next-pending-system state)))
    (cond
      ((null system-id)
       t)
      ;; Already decided (can happen if re-added to pending).
      ((assoc system-id (solver-state-decisions state) :test #'string=)
       (solver-search state))
      (t
       (let* ((constraint (or (cdr (assoc system-id (solver-state-constraints state)
                                          :test #'string=))
                              (any-constraint)))
              (candidates (find-candidates state system-id constraint)))
         (when (null candidates)
           (signal-conflict state system-id constraint
                            (format nil "No candidates found for ~A" system-id)))
         (let ((candidate-refs (ordered-candidate-refs state system-id candidates))
               (last-conflict nil))
           (dolist (release-ref candidate-refs)
             (let ((snapshot (snapshot-state state)))
               (handler-case
                   (progn
                     (decide state system-id release-ref)
                     (propagate-dependencies state system-id release-ref)
                     (when (solver-search state)
                       (return-from solver-search t)))
                 (resolution-conflict (c)
                   (setf last-conflict c)))
               (restore-state state snapshot)))
           (if last-conflict
               (error last-conflict)
               (signal-conflict state system-id constraint
                                (format nil "No viable candidates for ~A" system-id)
                                :candidates candidate-refs))))))))

(defun find-candidates (state system-id constraint)
  "Find release candidates for SYSTEM-ID satisfying CONSTRAINT."
  (let ((pinned-source (constraint-pinned-source constraint)))
    (when pinned-source
      (case (car pinned-source)
        (:path
         (let* ((raw (or (and (<= 2 (length pinned-source))
                              (stringp (second pinned-source))
                              (second pinned-source))
                         (getf (cdr pinned-source) :path)))
                (expanded (clpm.platform:expand-path raw))
                (pn (uiop:ensure-pathname expanded
                                          :defaults (uiop:getcwd)
                                          :want-existing nil))
                (abs (uiop:ensure-directory-pathname pn)))
           (unless (uiop:directory-exists-p abs)
             (signal-conflict state system-id constraint
                              (format nil "Path does not exist: ~A"
                                      (namestring abs))))
           (let* ((tru (uiop:ensure-directory-pathname (truename abs)))
                  (tree-sha256 (clpm.crypto.sha256:bytes-to-hex
                                (clpm.crypto.sha256:sha256-tree tru)))
                  (release-ref (format nil "local:~A@~A" system-id tree-sha256))
                  (meta (or (gethash release-ref (solver-state-extra-release-metadata state))
                            (setf (gethash release-ref (solver-state-extra-release-metadata state))
                                  (clpm.registry::make-release-metadata
                                   :name system-id
                                   :version (format nil "0.0.0+path.~A"
                                                    (subseq tree-sha256 0 12))
                                   :source (list :path
                                                 :path (namestring tru)
                                                 :tree-sha256 tree-sha256)
                                   :artifact-sha256 nil
                                   :systems (list system-id)
                                   :system-deps nil)))))
             (return-from find-candidates (list (cons release-ref meta))))))
        (:git
         (let* ((url (getf (cdr pinned-source) :url))
                (commit (getf (cdr pinned-source) :commit))
                (ref (or commit (getf (cdr pinned-source) :ref))))
           (unless url
             (signal-conflict state system-id constraint
                              "Missing :url for :git constraint"))
           (unless ref
             (signal-conflict state system-id constraint
                              "Missing :ref for :git constraint"))
           (let* ((commit (or commit (clpm.fetch:resolve-git-ref url ref)))
                  (short (subseq commit 0 (min 12 (length commit))))
                  (release-ref (format nil "git:~A@~A" system-id commit))
                  (meta (or (gethash release-ref (solver-state-extra-release-metadata state))
                            (setf (gethash release-ref (solver-state-extra-release-metadata state))
                                  (clpm.registry::make-release-metadata
                                   :name system-id
                                   :version (format nil "0.0.0+git.~A" short)
                                   :source (list :git
                                                 :url url
                                                 :commit commit)
                                   :artifact-sha256 nil
                                   :systems (list system-id)
                                   :system-deps nil)))))
             (return-from find-candidates (list (cons release-ref meta))))))
        (t
         (signal-conflict state system-id constraint
                          (format nil "Unknown pinned source: ~S" pinned-source))))))

  (let ((index (solver-state-index state))
        (candidates '()))
    ;; Get candidates from index
    (let ((entries (clpm.registry:index-lookup-system index system-id)))
      (dolist (entry entries)
        (let* ((release-ref (cdr entry))
               (meta-entry (clpm.registry:index-lookup-release index release-ref)))
          (when meta-entry
            (let* ((meta (cdr meta-entry))
                   (version (clpm.registry:release-metadata-version meta)))
              ;; Check constraint
              (when (constraint-satisfies-p constraint version)
                (push (cons release-ref meta) candidates)))))))
    ;; Sort by version descending
    (sort candidates #'version>
          :key (lambda (c) (parse-version
                            (clpm.registry:release-metadata-version (cdr c)))))))

(defun choose-candidate (state system-id candidates)
  "Choose best candidate for SYSTEM-ID from CANDIDATES.
Prefers lockfile selection, then highest version."
  (let ((lockfile (solver-state-lockfile state)))
    ;; Check lockfile preference
    (when lockfile
      (dolist (locked (clpm.project:lockfile-resolved lockfile))
        (when (string= (clpm.project:locked-system-id locked) system-id)
          (let* ((locked-rel (clpm.project:locked-system-release locked))
                 (locked-name (clpm.project:locked-release-name locked-rel))
                 (locked-version (clpm.project:locked-release-version locked-rel))
                 (locked-ref (format nil "~A@~A" locked-name locked-version)))
            ;; Find matching candidate
            (dolist (cand candidates)
              (when (string= (car cand) locked-ref)
                (return-from choose-candidate (car cand))))))))
    ;; Otherwise take highest version (first after sort)
    (caar candidates)))

(defun propagate-dependencies (state system-id release-ref)
  "Propagate dependencies from chosen release."
  (let* ((meta-entry (lookup-release-entry state release-ref)))
    (when meta-entry
      (let* ((meta (cdr meta-entry))
             (system-deps (clpm.registry:release-metadata-system-deps meta)))
        ;; Find deps for this specific system
        (let ((deps-entry (assoc system-id system-deps :test #'string=)))
          (when deps-entry
            (dolist (dep (cdr deps-entry))
              (let ((dep-system (car dep))
                    (dep-constraint-form (cdr dep)))
                ;; Add constraint
                (let ((constraint (parse-constraint dep-constraint-form))
                      (existing (assoc dep-system (solver-state-constraints state)
                                       :test #'string=)))
                  (record-constraint-reason state dep-system release-ref constraint)
                  (if existing
                      (let ((merged (constraint-intersect (cdr existing) constraint)))
                        (when (constraint-empty-p merged)
                          (signal-conflict state dep-system constraint
                                           (format nil "Conflict: ~A requires ~A ~A"
                                                   release-ref dep-system
                                                   (constraint-to-string constraint))))
                        (setf (cdr existing) merged))
                      (push (cons dep-system constraint)
                            (solver-state-constraints state))))
                ;; If DEP-SYSTEM is already decided, ensure the selected version
                ;; still satisfies the tightened constraint.
                (let ((decided (assoc dep-system (solver-state-decisions state)
                                      :test #'string=)))
                  (when decided
                    (let ((current (cdr (assoc dep-system (solver-state-constraints state)
                                               :test #'string=))))
                      (when current
                        (check-decision-satisfies-constraint state dep-system current)))))
                ;; Add to pending
                (unless (assoc dep-system (solver-state-decisions state)
                               :test #'string=)
                  (pushnew dep-system (solver-state-pending state)
                           :test #'string=))))))))))

;;; Build final resolution

(defun build-resolution (state)
  "Build resolution result from solver state."
  (let ((systems '())
        (graph '()))
    ;; Collect decisions
    (dolist (decision (solver-state-decisions state))
      (let* ((system-id (car decision))
             (release-ref (cdr decision))
             (meta-entry (lookup-release-entry state release-ref)))
        (when meta-entry
          (let* ((registry (car meta-entry))
                 (meta (cdr meta-entry))
                 (source (clpm.registry:release-metadata-source meta)))
            (push (list :system system-id
                        :release-ref release-ref
                        :registry (when registry (clpm.registry:registry-name registry))
                        :name (clpm.registry:release-metadata-name meta)
                        :version (clpm.registry:release-metadata-version meta)
                        :source source
                        :artifact-sha256 (clpm.registry:release-metadata-artifact-sha256 meta)
                        :tree-sha256 (and (consp source)
                                          (getf (cdr source) :tree-sha256)))
                  systems)
            ;; Add to graph
            (let ((deps-entry (assoc system-id
                                     (clpm.registry:release-metadata-system-deps meta)
                                     :test #'string=)))
              (push (cons system-id (mapcar #'car (cdr deps-entry)))
                    graph))))))
    (make-resolution :systems (nreverse systems)
                     :graph (nreverse graph))))

;;; Topological sort for build order

(defun topological-sort (resolution)
  "Return systems from RESOLUTION in topological order (dependencies first)."
  (let* ((graph (resolution-graph resolution))
         (systems (mapcar #'car graph))
         (visited (make-hash-table :test 'equal))
         (result '()))
    (labels ((visit (node)
               (unless (gethash node visited)
                 (setf (gethash node visited) :in-progress)
                 (let ((deps (cdr (assoc node graph :test #'string=))))
                   (dolist (dep deps)
                     (visit dep)))
                 (setf (gethash node visited) t)
                 (push node result))))
      (dolist (sys systems)
        (visit sys)))
    (nreverse result)))

;;; Generate lockfile from resolution

(defun resolution-to-lockfile (resolution project registries)
  "Convert RESOLUTION to a lockfile struct."
  (let ((lock (clpm.project:make-lockfile
               :format 1
               :generated-at (clpm.project:rfc3339-timestamp)
               :project-name (clpm.project:project-name project)
               :clpm-version "0.1.0")))
    ;; Add registries
    (dolist (reg registries)
      (push (clpm.project:make-locked-registry
             :name (clpm.registry:registry-name reg)
             :kind :git
             :url (clpm.registry:registry-url reg)
             :trust (clpm.registry:registry-trust-key reg)
             :commit (clpm.registry:git-rev-parse
                      (clpm.registry:registry-local-path
                       (clpm.registry:registry-name reg)))
             :signature (clpm.registry:registry-snapshot-sig-sha256 reg))
            (clpm.project:lockfile-registries lock)))
    ;; Add resolved systems
    (dolist (sys (resolution-systems resolution))
      (let* ((source (getf sys :source))
             (kind (car source))
             (source-plist (cdr source))
             (tree-sha256 (or (getf sys :tree-sha256)
                              (and (consp source)
                                   (getf source-plist :tree-sha256))))
             (locked-source
               (case kind
                 (:tarball
                  (clpm.project:make-locked-source
                   :kind :tarball
                   :url (getf source-plist :url)
                   :sha256 (getf source-plist :sha256)))
                 (:git
                  (clpm.project:make-locked-source
                   :kind :git
                   :url (getf source-plist :url)
                   :commit (getf source-plist :commit)))
                 (:path
                  (let ((path (or (and (stringp (second source)) (second source))
                                  (getf source-plist :path))))
                    (clpm.project:make-locked-source
                     :kind :path
                     :path path
                     :sha256 tree-sha256)))
                 (t
                  (error "Unknown source kind: ~S" kind)))))
        (push (clpm.project:make-locked-system
               :id (getf sys :system)
               :release (clpm.project:make-locked-release
                         :name (getf sys :name)
                         :version (getf sys :version)
                         :source locked-source
                         :artifact-sha256 (getf sys :artifact-sha256)
                         :tree-sha256 tree-sha256)
               :deps (cdr (assoc (getf sys :system)
                                 (resolution-graph resolution)
                                 :test #'string=)))
              (clpm.project:lockfile-resolved lock))))
    lock))
