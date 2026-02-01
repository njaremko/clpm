;;;; solver/constraint.lisp - Version constraint handling

(in-package #:clpm.solver.constraint)

;;; Range structure - represents a continuous version range

(defstruct range
  "A version range [lo, hi) or [lo, hi]."
  (lo nil :type (or null version))           ; lower bound
  (lo-inclusive-p t :type boolean)           ; include lower bound?
  (hi nil :type (or null version))           ; upper bound
  (hi-inclusive-p nil :type boolean))        ; include upper bound?

;;; Constraint structure - disjunction of ranges

(defstruct constraint
  "A version constraint (union of ranges)."
  (ranges nil :type list)                    ; list of ranges (OR'd together)
  (pinned-source nil))                       ; for :git/:path overrides

;;; Predefined constraints

(defun any-constraint ()
  "Return a constraint that matches any version."
  (make-constraint :ranges (list (make-range))))

(defun none-constraint ()
  "Return a constraint that matches no version."
  (make-constraint :ranges nil))

(defun exact-constraint (version)
  "Return a constraint that matches exactly VERSION."
  (let ((v (if (version-p version) version (parse-version version))))
    (make-constraint
     :ranges (list (make-range :lo v :lo-inclusive-p t
                               :hi v :hi-inclusive-p t)))))

;;; Range operations

(defun range-contains-p (range version)
  "Check if RANGE contains VERSION."
  (let ((v (if (version-p version) version (parse-version version))))
    (and (or (null (range-lo range))
             (if (range-lo-inclusive-p range)
                 (version>= v (range-lo range))
                 (version> v (range-lo range))))
         (or (null (range-hi range))
             (if (range-hi-inclusive-p range)
                 (version<= v (range-hi range))
                 (version< v (range-hi range)))))))

(defun range-empty-p (range)
  "Check if RANGE is empty."
  (and (range-lo range)
       (range-hi range)
       (or (version> (range-lo range) (range-hi range))
           (and (version= (range-lo range) (range-hi range))
                (not (and (range-lo-inclusive-p range)
                          (range-hi-inclusive-p range)))))))

(defun range-intersect (a b)
  "Compute intersection of ranges A and B."
  ;; Find the tighter lower bound
  (let ((lo nil)
        (lo-inc t)
        (hi nil)
        (hi-inc nil))
    ;; Lower bound
    (cond
      ((null (range-lo a))
       (setf lo (range-lo b)
             lo-inc (range-lo-inclusive-p b)))
      ((null (range-lo b))
       (setf lo (range-lo a)
             lo-inc (range-lo-inclusive-p a)))
      ((version> (range-lo a) (range-lo b))
       (setf lo (range-lo a)
             lo-inc (range-lo-inclusive-p a)))
      ((version< (range-lo a) (range-lo b))
       (setf lo (range-lo b)
             lo-inc (range-lo-inclusive-p b)))
      (t ; equal
       (setf lo (range-lo a)
             lo-inc (and (range-lo-inclusive-p a)
                         (range-lo-inclusive-p b)))))
    ;; Upper bound
    (cond
      ((null (range-hi a))
       (setf hi (range-hi b)
             hi-inc (range-hi-inclusive-p b)))
      ((null (range-hi b))
       (setf hi (range-hi a)
             hi-inc (range-hi-inclusive-p a)))
      ((version< (range-hi a) (range-hi b))
       (setf hi (range-hi a)
             hi-inc (range-hi-inclusive-p a)))
      ((version> (range-hi a) (range-hi b))
       (setf hi (range-hi b)
             hi-inc (range-hi-inclusive-p b)))
      (t ; equal
       (setf hi (range-hi a)
             hi-inc (and (range-hi-inclusive-p a)
                         (range-hi-inclusive-p b)))))
    (let ((result (make-range :lo lo :lo-inclusive-p lo-inc
                              :hi hi :hi-inclusive-p hi-inc)))
      (if (range-empty-p result)
          nil
          result))))

;;; Constraint operations

(defun constraint-satisfies-p (constraint version)
  "Check if CONSTRAINT allows VERSION."
  (when (constraint-pinned-source constraint)
    ;; Pinned sources always satisfy (version is determined by source)
    (return-from constraint-satisfies-p t))
  (let ((v (if (version-p version) version (parse-version version))))
    (some (lambda (r) (range-contains-p r v))
          (constraint-ranges constraint))))

(defun constraint-empty-p (constraint)
  "Check if CONSTRAINT is unsatisfiable."
  (and (null (constraint-pinned-source constraint))
       (null (constraint-ranges constraint))))

(defun constraint-intersect (a b)
  "Compute intersection of constraints A and B."
  ;; If either is pinned, they must match
  (when (constraint-pinned-source a)
    (return-from constraint-intersect a))
  (when (constraint-pinned-source b)
    (return-from constraint-intersect b))
  ;; Intersect all range pairs
  (let ((result-ranges '()))
    (dolist (ra (constraint-ranges a))
      (dolist (rb (constraint-ranges b))
        (let ((intersection (range-intersect ra rb)))
          (when intersection
            (push intersection result-ranges)))))
    (make-constraint :ranges (nreverse result-ranges))))

(defun constraint-union (a b)
  "Compute union of constraints A and B."
  ;; Simplified: just append ranges (could be merged for optimization)
  (make-constraint
   :ranges (append (constraint-ranges a)
                   (constraint-ranges b))
   :pinned-source (or (constraint-pinned-source a)
                      (constraint-pinned-source b))))

;;; Constraint parsing

(defun parse-constraint (form)
  "Parse a constraint form from manifest.
Forms:
  (:semver \"^1.2.3\")    - caret range
  (:semver \"~1.2.3\")    - tilde range
  (:semver \">=1.2 <2\")  - comparison range
  (:semver \"1.2.3\")     - exact
  (:exact \"1.2.3\")      - exact
  (:git :url ... :ref ...)- pinned git source
  (:path \"...\")         - pinned path source
  nil                     - any version"
  (cond
    ((null form)
     (any-constraint))
    ((not (consp form))
     (exact-constraint (princ-to-string form)))
    (t
     (case (car form)
       (:semver
        (parse-semver-constraint (cadr form)))
       (:exact
        (exact-constraint (cadr form)))
       (:git
        (make-constraint :pinned-source form))
        (:path
        (let* ((raw (cadr form))
               (expanded (clpm.platform:expand-path raw))
               (pn (uiop:ensure-pathname expanded
                                         :defaults (uiop:getcwd)
                                         :want-existing nil))
               (abs (uiop:ensure-directory-pathname pn))
               (tru (uiop:ensure-directory-pathname (truename abs))))
          (make-constraint :pinned-source (list :path (namestring tru)))))
        (t
         (error "Unknown constraint form: ~S" form))))))

(defun parse-semver-constraint (spec)
  "Parse a semver constraint specification string."
  (let ((spec (string-trim '(#\Space) spec)))
    (cond
      ;; Caret: ^1.2.3 allows >=1.2.3 <2.0.0
      ((and (plusp (length spec)) (char= (char spec 0) #\^))
       (parse-caret-range (subseq spec 1)))
      ;; Tilde: ~1.2.3 allows >=1.2.3 <1.3.0
      ((and (plusp (length spec)) (char= (char spec 0) #\~))
       (parse-tilde-range (subseq spec 1)))
      ;; Comparison operators
      ((or (search ">=" spec) (search "<=" spec)
           (search ">" spec) (search "<" spec)
           (search "=" spec))
       (parse-comparison-constraint spec))
      ;; Simple version = exact
      (t
       (exact-constraint spec)))))

(defun parse-caret-range (version-str)
  "Parse caret range ^X.Y.Z.
^1.2.3 => >=1.2.3 <2.0.0
^0.2.3 => >=0.2.3 <0.3.0
^0.0.3 => >=0.0.3 <0.0.4"
  (let* ((v (parse-version version-str))
         (major (version-major v))
         (minor (version-minor v))
         (patch (version-patch v))
         (upper (cond
                  ((plusp major)
                   (make-version :major (1+ major) :minor 0 :patch 0))
                  ((plusp minor)
                   (make-version :major 0 :minor (1+ minor) :patch 0))
                  (t
                   (make-version :major 0 :minor 0 :patch (1+ patch))))))
    (make-constraint
     :ranges (list (make-range :lo v :lo-inclusive-p t
                               :hi upper :hi-inclusive-p nil)))))

(defun parse-tilde-range (version-str)
  "Parse tilde range ~X.Y.Z.
~1.2.3 => >=1.2.3 <1.3.0"
  (let* ((v (parse-version version-str))
         (upper (make-version :major (version-major v)
                              :minor (1+ (version-minor v))
                              :patch 0)))
    (make-constraint
     :ranges (list (make-range :lo v :lo-inclusive-p t
                               :hi upper :hi-inclusive-p nil)))))

(defun parse-comparison-constraint (spec)
  "Parse comparison constraint like '>=1.2.0 <2.0.0'."
  (let ((parts (split-constraint-parts spec))
        (ranges '()))
    (if (= (length parts) 1)
        ;; Single comparison
        (setf ranges (list (parse-single-comparison (first parts))))
        ;; Multiple comparisons - intersect them
        (let ((range (make-range)))
          (dolist (part parts)
            (let* ((parsed (parse-single-comparison part))
                   (intersection (range-intersect range parsed)))
              (if intersection
                  (setf range intersection)
                  (return-from parse-comparison-constraint (none-constraint)))))
          (setf ranges (list range))))
    (make-constraint :ranges ranges)))

(defun split-constraint-parts (spec)
  "Split constraint spec into individual parts."
  (let ((parts '())
        (current "")
        (in-op nil))
    (loop for c across spec do
      (cond
        ((member c '(#\Space #\Tab #\,))
         (when (plusp (length current))
           (push current parts)
           (setf current "")))
        ((member c '(#\> #\< #\= #\!))
         (when (and (plusp (length current))
                    (not in-op))
           (push current parts)
           (setf current ""))
         (setf current (concatenate 'string current (string c)))
         (setf in-op t))
        (t
         (setf current (concatenate 'string current (string c)))
         (setf in-op nil))))
    (when (plusp (length current))
      (push current parts))
    (nreverse parts)))

(defun parse-single-comparison (part)
  "Parse a single comparison like '>=1.2.0'."
  (let ((op nil)
        (ver-start 0))
    ;; Extract operator
    (cond
      ((and (>= (length part) 2)
            (string= (subseq part 0 2) ">="))
       (setf op :>= ver-start 2))
      ((and (>= (length part) 2)
            (string= (subseq part 0 2) "<="))
       (setf op :<= ver-start 2))
      ((and (>= (length part) 2)
            (string= (subseq part 0 2) "!="))
       ;; != not directly supported in single range
       (setf op := ver-start 2))
      ((char= (char part 0) #\>)
       (setf op :> ver-start 1))
      ((char= (char part 0) #\<)
       (setf op :< ver-start 1))
      ((char= (char part 0) #\=)
       (setf op := ver-start 1))
      (t
       ;; No operator = exact
       (setf op := ver-start 0)))
    (let ((v (parse-version (subseq part ver-start))))
      (case op
        (:>= (make-range :lo v :lo-inclusive-p t))
        (:>  (make-range :lo v :lo-inclusive-p nil))
        (:<= (make-range :hi v :hi-inclusive-p t))
        (:<  (make-range :hi v :hi-inclusive-p nil))
        (:=  (make-range :lo v :lo-inclusive-p t
                         :hi v :hi-inclusive-p t))))))

;;; Constraint display

(defun constraint-to-string (constraint)
  "Convert constraint to human-readable string."
  (cond
    ((constraint-pinned-source constraint)
     (let ((src (constraint-pinned-source constraint)))
       (case (car src)
         (:git (format nil "git:~A@~A"
                       (getf (cdr src) :url)
                       (getf (cdr src) :ref)))
         (:path (format nil "path:~A" (cadr src)))
         (t (format nil "~S" src)))))
    ((null (constraint-ranges constraint))
     "<none>")
    (t
     (format nil "~{~A~^ || ~}"
             (mapcar #'range-to-string (constraint-ranges constraint))))))

(defun range-to-string (range)
  "Convert range to human-readable string."
  (cond
    ((and (null (range-lo range)) (null (range-hi range)))
     "*")
    ((and (range-lo range) (range-hi range)
          (version= (range-lo range) (range-hi range)))
     (version-to-string (range-lo range)))
    (t
     (format nil "~A~A"
             (if (range-lo range)
                 (format nil "~A~A"
                         (if (range-lo-inclusive-p range) ">=" ">")
                         (version-to-string (range-lo range)))
                 "")
             (if (range-hi range)
                 (format nil "~A~A~A"
                         (if (range-lo range) " " "")
                         (if (range-hi-inclusive-p range) "<=" "<")
                         (version-to-string (range-hi range)))
                 "")))))
