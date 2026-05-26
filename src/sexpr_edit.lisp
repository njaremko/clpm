;;;; sexpr_edit.lisp - Source-preserving structural Lisp inspection

(in-package #:clpm.sexpr-edit)

(define-condition source-path-error (error)
  ((message :initarg :message :reader source-path-error-message))
  (:report (lambda (c s)
             (format s "~A" (source-path-error-message c)))))

(define-condition source-reader-error (error)
  ((diagnostic :initarg :diagnostic :reader source-reader-error-diagnostic))
  (:report (lambda (c s)
             (format s "~A"
                     (source-diagnostic-message
                      (source-reader-error-diagnostic c))))))

(define-condition source-edit-error (error)
  ((message :initarg :message :reader source-edit-error-message)
   (diagnostics :initarg :diagnostics :initform nil
                :reader source-edit-error-diagnostics))
  (:report (lambda (c s)
             (format s "~A" (source-edit-error-message c)))))

(defstruct source-diagnostic
  phase
  message
  position
  line
  column)

(defstruct source-form
  ordinal
  start
  end
  line
  column
  package
  kind
  name
  operator
  children-count
  text
  form)

(defstruct source-document
  pathname
  text
  forms
  diagnostics)

(defstruct edit-result
  file
  operation
  form
  before-text
  after-text
  structural-diff
  diagnostics)

(defstruct edit-change
  kind
  operation
  before-form
  after-forms
  operator
  added-argument-texts
  removed-argument-texts)

(defstruct defpackage-update-result
  file
  operation
  package
  symbol
  from-package
  changed-p
  duplicate-p
  before-text
  after-text
  diagnostics)

(defun %read-file-string (pathname)
  (with-open-file (s pathname :direction :input :external-format :utf-8)
    (let ((out (make-string-output-stream)))
      (loop for ch = (read-char s nil nil)
            while ch
            do (write-char ch out))
      (get-output-stream-string out))))

(defun %line-column (text position)
  (let ((line 1)
        (column 1))
    (loop for i from 0 below (min position (length text))
          for ch = (char text i)
          do (cond
               ((char= ch #\Newline)
                (incf line)
                (setf column 1))
               (t
                (incf column))))
    (values line column)))

(defun %diagnostic (text phase message position)
  (multiple-value-bind (line column)
      (%line-column text (or position 0))
    (make-source-diagnostic :phase phase
                            :message message
                            :position position
                            :line line
                            :column column)))

(defun %whitespace-char-p (ch)
  (member ch '(#\Space #\Tab #\Newline #\Return #\Page) :test #'char=))

(defun %skip-line-comment (text position)
  (let ((n (length text))
        (i position))
    (loop while (< i n)
          for ch = (char text i)
          do (incf i)
          when (char= ch #\Newline)
            return i
          finally (return i))))

(defun %skip-block-comment (text position)
  (let ((n (length text))
        (i (+ position 2))
        (depth 1))
    (loop while (< i n)
          do (cond
               ((and (< (1+ i) n)
                     (char= (char text i) #\#)
                     (char= (char text (1+ i)) #\|))
                (incf depth)
                (incf i 2))
               ((and (< (1+ i) n)
                     (char= (char text i) #\|)
                     (char= (char text (1+ i)) #\#))
                (decf depth)
                (incf i 2)
                (when (zerop depth)
                  (return-from %skip-block-comment (values i nil))))
               (t
                (incf i))))
    (values i "unterminated block comment")))

(defun %skip-sexpr-comment (text position package)
  (let ((eof (gensym "EOF-")))
    (handler-case
        (let ((*read-eval* nil)
              (*package* package))
          (with-input-from-string (s text :start (+ position 2))
            (let ((form (read s nil eof)))
              (declare (ignore form))
              (if (eq form eof)
                  (values (length text) "unterminated #; comment")
                  (values (+ position 2 (file-position s)) nil)))))
      (error (c)
        (values position (format nil "reader error in #; comment: ~A" c))))))

(defun %skip-trivia (text position package)
  (let ((n (length text))
        (i position))
    (loop
      (cond
        ((>= i n)
         (return (values i nil)))
        ((%whitespace-char-p (char text i))
         (incf i))
        ((char= (char text i) #\;)
         (setf i (%skip-line-comment text i)))
        ((and (< (1+ i) n)
              (char= (char text i) #\#)
              (char= (char text (1+ i)) #\|))
         (multiple-value-bind (next error)
             (%skip-block-comment text i)
           (setf i next)
           (when error
             (return (values i error)))))
        ((and (< (1+ i) n)
              (char= (char text i) #\#)
              (char= (char text (1+ i)) #\;))
         (multiple-value-bind (next error)
             (%skip-sexpr-comment text i package)
           (when error
             (return (values next error)))
           (setf i next)))
        (t
         (return (values i nil)))))))

(defun %safe-token (value &optional package)
  (cond
    ((null value) nil)
    ((symbolp value) (symbol-name value))
    ((stringp value) value)
    (t
     (let ((*package* (or package *package*))
           (*print-circle* t)
           (*print-pretty* nil))
       (prin1-to-string value)))))

(defun %operator-token (form)
  (when (and (consp form) (symbolp (first form)))
    (string-downcase (symbol-name (first form)))))

(defun %defstruct-name (name package)
  (cond
    ((symbolp name) (symbol-name name))
    ((consp name) (%safe-token (first name) package))
    (t (%safe-token name package))))

(defun %definition-name (operator form package)
  (when (consp form)
    (cond
      ((member operator
               '("defun" "defmacro" "defgeneric" "defclass"
                 "define-condition" "defvar" "defparameter" "defconstant"
                 "defpackage" "in-package")
               :test #'string=)
       (%safe-token (second form) package))
      ((string= operator "defmethod")
       (%safe-token (second form) package))
      ((string= operator "defstruct")
       (%defstruct-name (second form) package))
      (t nil))))

(defun %children-count (form)
  (let ((seen (make-hash-table :test 'eq))
        (count 0)
        (tail form))
    (loop while (consp tail)
          do (when (gethash tail seen)
               (return count))
             (setf (gethash tail seen) t)
             (incf count)
             (setf tail (cdr tail))
          finally (return count))))

(defun %package-designator-name (designator package)
  (cond
    ((stringp designator) designator)
    ((symbolp designator) (symbol-name designator))
    (t (%safe-token designator package))))

(defun %in-package-target (form package)
  (when (and (consp form)
             (symbolp (first form))
             (string= "IN-PACKAGE" (symbol-name (first form)))
             (second form))
    (%package-designator-name (second form) package)))

(defun %make-source-form (text ordinal start end package-name package form)
  (multiple-value-bind (line column)
      (%line-column text start)
    (let* ((operator (%operator-token form))
           (name (and operator (%definition-name operator form package))))
      (make-source-form :ordinal ordinal
                        :start start
                        :end end
                        :line line
                        :column column
                        :package package-name
                        :kind operator
                        :name name
                        :operator operator
                        :children-count (%children-count form)
                        :text (subseq text start end)
                        :form form))))

(defun %trim-form-end (text start end)
  (let ((i end))
    (loop while (and (> i start)
                     (%whitespace-char-p (char text (1- i))))
          do (decf i))
    i))

(defun %resolve-source-pathname (file root)
  (unless (and (stringp file) (plusp (length file)))
    (error 'source-path-error :message "source file must be a non-empty string"))
  (let* ((base (and root
                    (uiop:ensure-directory-pathname
                     (truename (uiop:ensure-directory-pathname root)))))
         (candidate (uiop:ensure-pathname file
                                          :defaults (or base (uiop:getcwd))
                                          :want-file t))
         (actual (handler-case (truename candidate)
                   (error ()
                     (error 'source-path-error
                            :message (format nil "source file does not exist: ~A"
                                             file))))))
    (when base
      (let ((base-name (namestring base))
            (actual-name (namestring actual)))
        (unless (and (>= (length actual-name) (length base-name))
                     (string= base-name actual-name
                              :end1 (length base-name)
                              :end2 (length base-name)))
          (error 'source-path-error
                 :message (format nil "source file is outside project root: ~A"
                                  file)))))
    actual))

(defun %read-source-text (pathname text initial-package-name)
  (let* ((package (or (and initial-package-name
                           (find-package initial-package-name))
                      (find-package "COMMON-LISP-USER")))
         (package-name (package-name package))
         (forms '())
         (diagnostics '())
         (position 0)
         (ordinal 0)
         (eof (gensym "EOF-")))
    (loop
      (multiple-value-bind (start trivia-error)
          (%skip-trivia text position package)
        (when trivia-error
          (push (%diagnostic text "read" trivia-error start) diagnostics)
          (return))
        (when (>= start (length text))
          (return))
        (handler-case
            (let ((*read-eval* nil)
                  (*package* package))
              (with-input-from-string (s text :start start)
                (let ((form (read s nil eof)))
                  (when (eq form eof)
                    (return))
                  (let* ((raw-end (+ start (file-position s)))
                         (form-end (%trim-form-end text start raw-end))
                         (source-form (%make-source-form text ordinal start form-end
                                                         package-name package
                                                         form)))
                    (push source-form forms)
                    (incf ordinal)
                    (setf position raw-end)
                    (let ((target (%in-package-target form package)))
                      (when target
                        (setf package-name target)
                        (let ((target-package (find-package target)))
                          (when target-package
                            (setf package target-package)))))))))
          (reader-error (c)
            (push (%diagnostic text "read"
                               (format nil "~A" c)
                               start)
                  diagnostics)
            (return))
          (error (c)
            (push (%diagnostic text "read"
                               (format nil "~A" c)
                               start)
                  diagnostics)
            (return)))))
    (make-source-document :pathname pathname
                          :text text
                          :forms (nreverse forms)
                          :diagnostics (nreverse diagnostics))))

(defun read-source-document (file &key root initial-package-name)
  "Read FILE into a source document of top-level concrete forms.

The reader binds *READ-EVAL* to NIL. This is an inspection operation, not
a load operation."
  (let* ((pathname (%resolve-source-pathname file root))
         (text (%read-file-string pathname)))
    (%read-source-text pathname text initial-package-name)))

(defun find-source-forms (document &key top-level kind name)
  "Return forms in DOCUMENT selected by TOP-LEVEL or KIND/NAME."
  (let ((forms (source-document-forms document)))
    (cond
      ((integerp top-level)
       (let ((form (find top-level forms :key #'source-form-ordinal)))
         (if form (list form) nil)))
      ((or kind name)
       (remove-if-not
        (lambda (form)
          (and (or (null kind)
                   (and (source-form-kind form)
                        (string= kind (source-form-kind form))))
               (or (null name)
                   (and (source-form-name form)
                        (string-equal name (source-form-name form))))))
        forms))
      (t
       forms))))

(defun %trim-edit-text (text)
  (string-trim '(#\Space #\Tab #\Newline #\Return #\Page) text))

(defun %read-edit-forms (text package)
  (let ((forms '())
        (eof (gensym "EOF-")))
    (handler-case
        (let ((*read-eval* nil)
              (*package* package))
          (with-input-from-string (s text)
            (loop for form = (read s nil eof)
                  until (eq form eof)
                  do (push form forms)))
          (values (nreverse forms) nil))
      (error (c)
        (values nil (format nil "~A" c))))))

(defun %require-edit-form-count (operation text package min-count max-count)
  (unless (stringp text)
    (error 'source-edit-error
           :message (format nil "~A requires source text" operation)))
  (multiple-value-bind (forms error)
      (%read-edit-forms text package)
    (when error
      (error 'source-edit-error
             :message (format nil "~A text is not readable: ~A"
                              operation error)))
    (let ((count (length forms)))
      (unless (and (<= min-count count)
                   (or (null max-count) (<= count max-count)))
        (error 'source-edit-error
               :message (format nil "~A expected ~A, got ~D"
                                operation
                                (cond
                                  ((and max-count
                                        (= min-count 1)
                                        (= max-count 1))
                                   "exactly one form")
                                  ((null max-count)
                                   (format nil "at least ~D form(s)"
                                           min-count))
                                  (t
                                   (format nil "~D to ~D form(s)"
                                           min-count max-count)))
                                count))))
    forms))

(defun %print-edit-form (form package)
  (let ((*package* package)
        (*print-case* :downcase)
        (*print-pretty* t)
        (*print-circle* t))
    (prin1-to-string form)))

(defun %placeholder-symbol-p (value)
  (and (symbolp value)
       (string= "%" (symbol-name value))))

(defun %replace-placeholder (template old-form)
  (cond
    ((%placeholder-symbol-p template)
     (values old-form 1))
    ((consp template)
     (multiple-value-bind (new-car car-count)
         (%replace-placeholder (car template) old-form)
       (multiple-value-bind (new-cdr cdr-count)
           (%replace-placeholder (cdr template) old-form)
         (values (cons new-car new-cdr)
                 (+ car-count cdr-count)))))
    (t
     (values template 0))))

(defun %wrapped-form-text (template-text old-form package)
  (let* ((clean-template (%trim-edit-text template-text))
         (templates (%require-edit-form-count "wrap" clean-template
                                              package 1 1)))
    (multiple-value-bind (wrapped count)
        (%replace-placeholder (first templates) old-form)
      (unless (= count 1)
        (error 'source-edit-error
               :message "wrap template must contain exactly one % placeholder"))
      (%print-edit-form wrapped package))))

(defun %replace-range (text start end replacement)
  (concatenate 'string
               (subseq text 0 start)
               replacement
               (subseq text end)))

(defun %insert-before-range (text start insertion)
  (concatenate 'string
               (subseq text 0 start)
               insertion
               (unless (and (plusp (length insertion))
                            (char= (char insertion (1- (length insertion)))
                                   #\Newline))
                 (string #\Newline))
               (subseq text start)))

(defun %insert-after-range (text end insertion)
  (concatenate 'string
               (subseq text 0 end)
               (string #\Newline)
               insertion
               (subseq text end)))

(defun %write-file-string (pathname text)
  (with-open-file (s pathname :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string text s)))

(defun %document-form-at-ordinal (document ordinal)
  (find ordinal (source-document-forms document)
        :key #'source-form-ordinal))

(defun %document-forms-from-ordinal (document ordinal count)
  (loop for i from ordinal
        repeat count
        for form = (%document-form-at-ordinal document i)
        when form
          collect form))

(defun %proper-list-elements (value)
  (let ((seen (make-hash-table :test 'eq))
        (tail value)
        (elements '()))
    (loop
      (cond
        ((null tail)
         (return (nreverse elements)))
        ((consp tail)
         (when (gethash tail seen)
           (return nil))
         (setf (gethash tail seen) t)
         (push (car tail) elements)
         (setf tail (cdr tail)))
        (t
         (return nil))))))

(defun %definition-operator-token-p (operator)
  (member operator
          '("defun" "defmacro" "defgeneric" "defmethod" "defclass"
            "define-condition" "defvar" "defparameter" "defconstant"
            "defstruct" "defpackage" "in-package")
          :test #'string=))

(defun %operator-name (form)
  (when (and (consp form) (symbolp (first form)))
    (string-downcase (symbol-name (first form)))))

(defun %edit-argument-texts (forms package)
  (mapcar (lambda (form) (%print-edit-form form package)) forms))

(defun %prefix-equal-p (prefix values)
  (and (<= (length prefix) (length values))
       (loop for expected in prefix
             for actual in values
             always (equal expected actual))))

(defun %call-argument-diff (old-args new-args package)
  (cond
    ((%prefix-equal-p old-args new-args)
     (values (%edit-argument-texts (nthcdr (length old-args) new-args)
                                   package)
             nil))
    ((%prefix-equal-p new-args old-args)
     (values nil
             (%edit-argument-texts (nthcdr (length new-args) old-args)
                                   package)))
    (t
     (values nil nil))))

(defun %changed-call-edit-change (operation before-form after-forms package)
  (when (and (= 1 (length after-forms))
             before-form)
    (let* ((after-form (first after-forms))
           (old-form (source-form-form before-form))
           (new-form (source-form-form after-form))
           (old-elements (%proper-list-elements old-form))
           (new-elements (%proper-list-elements new-form))
           (old-operator (%operator-name old-form))
           (new-operator (%operator-name new-form)))
      (when (and old-elements new-elements
                 old-operator
                 (string= old-operator new-operator)
                 (not (%definition-operator-token-p old-operator))
                 (not (equal (rest old-elements) (rest new-elements))))
        (multiple-value-bind (added removed)
            (%call-argument-diff (rest old-elements) (rest new-elements)
                                 package)
          (make-edit-change :kind :changed-call
                            :operation operation
                            :before-form before-form
                            :after-forms after-forms
                            :operator old-operator
                            :added-argument-texts added
                            :removed-argument-texts removed))))))

(defun %top-level-edit-change (operation before-form after-forms)
  (let ((insert-p (member operation '("insert-before" "insert-after")
                          :test #'string=)))
    (make-edit-change
     :kind (cond
             (insert-p
              :inserted-top-level)
             ((string= operation "delete")
              :deleted-top-level)
             ((string= operation "splice")
              :spliced-top-level)
             (t
              :changed-top-level))
     :operation operation
     :before-form (and (not insert-p) before-form)
     :after-forms after-forms)))

(defun %edit-structural-diff (operation before-form after-forms package)
  (let ((top-level (%top-level-edit-change operation before-form after-forms))
        (changed-call (%changed-call-edit-change operation before-form
                                                 after-forms package)))
    (remove nil (list top-level changed-call))))

(defun %select-unique-form (document top-level kind name)
  (let ((forms (find-source-forms document
                                  :top-level top-level
                                  :kind kind
                                  :name name)))
    (cond
      ((null forms)
       (error 'source-edit-error :message "no form matched source path"))
      ((rest forms)
       (error 'source-edit-error
              :message (format nil "source path is ambiguous: ~D forms match"
                               (length forms))))
      (t
       (first forms)))))

(defun %source-document-readable-or-error (pathname text initial-package-name)
  (let ((document (%read-source-text pathname text initial-package-name)))
    (when (source-document-diagnostics document)
      (error 'source-edit-error
             :message "edited file is not readable"
             :diagnostics (source-document-diagnostics document)))
    document))

(defun %source-edit-result (file operation write-p
                            &key root initial-package-name top-level kind name
                            text)
  (let* ((pathname (%resolve-source-pathname file root))
         (before (%read-file-string pathname))
         (document (%read-source-text pathname before initial-package-name)))
    (when (source-document-diagnostics document)
      (error 'source-edit-error
             :message "source file is not readable before edit"
             :diagnostics (source-document-diagnostics document)))
    (let* ((package (or (and initial-package-name
                             (find-package initial-package-name))
                        (find-package "COMMON-LISP-USER")))
           (op (etypecase operation
                 (keyword operation)
                 (string (intern (string-upcase operation) :keyword))))
           (target (%select-unique-form document top-level kind name))
           (clean-text (and text (%trim-edit-text text)))
           (start (source-form-start target))
           (end (source-form-end target))
           (after-form-count 0)
           (after
             (case op
               (:replace
                (%require-edit-form-count "replace" clean-text package 1 1)
                (setf after-form-count 1)
                (%replace-range before start end clean-text))
               (:insert-before
                (%require-edit-form-count "insert-before" clean-text
                                          package 1 1)
                (setf after-form-count 1)
                (%insert-before-range before start clean-text))
               (:insert-after
                (%require-edit-form-count "insert-after" clean-text
                                          package 1 1)
                (setf after-form-count 1)
                (%insert-after-range before end clean-text))
               (:delete
                (%replace-range before start end ""))
               (:wrap
                (setf after-form-count 1)
                (%replace-range
                 before start end
                 (%wrapped-form-text clean-text (source-form-form target)
                                     package)))
               (:splice
                (setf after-form-count
                      (length (%require-edit-form-count "splice" clean-text
                                                        package 1 nil)))
                (%replace-range before start end clean-text))
               (t
                (error 'source-edit-error
                       :message (format nil "unknown sexpr edit operation: ~A"
                                        operation))))))
      (let ((new-document
              (%source-document-readable-or-error pathname after
                                                  initial-package-name)))
        (when write-p
          (%write-file-string pathname after))
        (let* ((operation-token (string-downcase (symbol-name op)))
               (after-start-ordinal
                 (case op
                   (:insert-after (1+ (source-form-ordinal target)))
                   (t (source-form-ordinal target))))
               (after-forms (%document-forms-from-ordinal
                             new-document after-start-ordinal after-form-count)))
          (make-edit-result :file (namestring pathname)
                            :operation operation-token
                            :form target
                            :before-text (source-form-text target)
                            :after-text after
                            :structural-diff
                            (%edit-structural-diff operation-token target
                                                   after-forms package)
                            :diagnostics (source-document-diagnostics
                                          new-document)))))))

(defun plan-source-edit (file operation
                         &key root initial-package-name top-level kind name text)
  "Plan one top-level structural edit to FILE without writing it."
  (%source-edit-result file operation nil
                       :root root
                       :initial-package-name initial-package-name
                       :top-level top-level
                       :kind kind
                       :name name
                       :text text))

(defun apply-source-edit (file operation
                          &key root initial-package-name top-level kind name text)
  "Apply one top-level structural edit to FILE.

This is transactional at file granularity: the file is written only after
the resulting source reads successfully."
  (%source-edit-result file operation t
                       :root root
                       :initial-package-name initial-package-name
                       :top-level top-level
                       :kind kind
                       :name name
                       :text text))

(defstruct source-child-span
  start
  end
  form)

(defun %read-source-form-span (text position package)
  (multiple-value-bind (start trivia-error)
      (%skip-trivia text position package)
    (when trivia-error
      (error 'source-edit-error :message trivia-error))
    (when (>= start (length text))
      (return-from %read-source-form-span (values nil start start)))
    (when (char= (char text start) #\))
      (return-from %read-source-form-span (values nil start start)))
    (let ((eof (gensym "EOF-")))
      (handler-case
          (let ((*read-eval* nil)
                (*package* package))
            (with-input-from-string (s text :start start)
              (let ((form (read s nil eof)))
                (if (eq form eof)
                    (values nil start start)
                    (let ((raw-end (+ start (file-position s))))
                      (values form start (%trim-form-end text start
                                                         raw-end)))))))
        (error (c)
          (error 'source-edit-error
                 :message (format nil "unable to read defpackage form: ~A"
                                  c)))))))

(defun %list-child-spans (text package)
  (unless (and (plusp (length text))
               (char= (char text 0) #\())
    (error 'source-edit-error
           :message "defpackage source is not a list form"))
  (let ((position 1)
        (spans '()))
    (loop
      (multiple-value-bind (form start end)
          (%read-source-form-span text position package)
        (when (or (null form)
                  (>= start (length text))
                  (char= (char text start) #\)))
          (return (nreverse spans)))
        (push (make-source-child-span :start start
                                      :end end
                                      :form form)
              spans)
        (setf position end)))))

(defun %normalize-defpackage-operation (operation)
  (let ((token (etypecase operation
                 (keyword (string-downcase (symbol-name operation)))
                 (string (string-downcase operation)))))
    (cond
      ((string= token "export") "export")
      ((or (string= token "import")
           (string= token "import-from"))
       "import-from")
      ((or (string= token "shadowing-import")
           (string= token "shadowing-import-from"))
       "shadowing-import-from")
      (t
       (error 'source-edit-error
              :message (format nil "unknown defpackage operation: ~A"
                               operation))))))

(defun %defpackage-clause-key (operation)
  (cond
    ((string= operation "export") "EXPORT")
    ((string= operation "import-from") "IMPORT-FROM")
    ((string= operation "shadowing-import-from") "SHADOWING-IMPORT-FROM")
    (t operation)))

(defun %defpackage-clause-token (operation)
  (cond
    ((string= operation "export") ":export")
    ((string= operation "import-from") ":import-from")
    ((string= operation "shadowing-import-from") ":shadowing-import-from")
    (t (format nil ":~A" operation))))

(defun %defpackage-designator-token (name)
  (format nil "#:~A" (string-downcase name)))

(defun %defpackage-clause-key-p (clause key)
  (and (consp clause)
       (symbolp (first clause))
       (string= key (symbol-name (first clause)))))

(defun %defpackage-import-clause-package-p (clause from-package package)
  (and (consp clause)
       (second clause)
       (string-equal from-package
                     (%package-designator-name (second clause) package))))

(defun %defpackage-operation-clause-p (clause operation from-package package)
  (let ((key (%defpackage-clause-key operation)))
    (and (%defpackage-clause-key-p clause key)
         (or (string= operation "export")
             (%defpackage-import-clause-package-p clause from-package
                                                 package)))))

(defun %defpackage-clause-symbols (clause operation)
  (if (string= operation "export")
      (rest clause)
      (cddr clause)))

(defun %defpackage-clause-has-symbol-p (clause operation symbol-name package)
  (member symbol-name
          (mapcar (lambda (designator)
                    (%package-designator-name designator package))
                  (%defpackage-clause-symbols clause operation))
          :test #'string-equal))

(defun %defpackage-symbol-indent (text clause-span operation package)
  (let* ((clause-text (subseq text
                              (source-child-span-start clause-span)
                              (source-child-span-end clause-span)))
         (children (%list-child-spans clause-text package))
         (symbol-children (if (string= operation "export")
                              (rest children)
                              (cddr children))))
    (if symbol-children
        (multiple-value-bind (line column)
            (%line-column
             text
             (+ (source-child-span-start clause-span)
                (source-child-span-start (first symbol-children))))
          (declare (ignore line))
          (make-string (1- column) :initial-element #\Space))
        (multiple-value-bind (line column)
            (%line-column text (source-child-span-start clause-span))
          (declare (ignore line))
          (make-string (+ column 1) :initial-element #\Space)))))

(defun %insert-defpackage-clause-symbol (text clause-span operation token package)
  (let ((insert-position (1- (source-child-span-end clause-span))))
    (%replace-range
     text insert-position insert-position
     (format nil "~%~A~A"
             (%defpackage-symbol-indent text clause-span operation package)
             token))))

(defun %defpackage-new-clause (operation token from-package)
  (cond
    ((string= operation "export")
     (format nil "  (~A ~A)" (%defpackage-clause-token operation) token))
    (t
     (format nil "  (~A ~A ~A)"
             (%defpackage-clause-token operation)
             (%defpackage-designator-token from-package)
             token))))

(defun %insert-defpackage-new-clause (text operation token from-package)
  (let ((insert-position (1- (length text))))
    (%replace-range
     text insert-position insert-position
     (format nil "~%~A"
             (%defpackage-new-clause operation token from-package)))))

(defun %select-defpackage-form (document package-name)
  (let ((forms (find-source-forms document
                                  :kind "defpackage"
                                  :name package-name)))
    (cond
      ((null forms)
       (error 'source-edit-error
              :message (format nil "no defpackage form for ~A"
                               package-name)))
      ((rest forms)
       (error 'source-edit-error
              :message (format nil
                               "defpackage selector is ambiguous: ~D forms match"
                               (length forms))))
      (t
       (first forms)))))

(defun %defpackage-update-result (file operation write-p
                                  &key root initial-package-name package
                                  symbol from-package)
  (unless (and (stringp package) (plusp (length package)))
    (error 'source-edit-error :message "`package' must be a non-empty string"))
  (unless (and (stringp symbol) (plusp (length symbol)))
    (error 'source-edit-error :message "`symbol' must be a non-empty string"))
  (let* ((operation-token (%normalize-defpackage-operation operation))
         (pathname (%resolve-source-pathname file root))
         (before (%read-file-string pathname))
         (document (%read-source-text pathname before initial-package-name)))
    (when (source-document-diagnostics document)
      (error 'source-edit-error
             :message "source file is not readable before defpackage update"
             :diagnostics (source-document-diagnostics document)))
    (when (and (not (string= operation-token "export"))
               (not (and (stringp from-package)
                         (plusp (length from-package)))))
      (error 'source-edit-error
             :message "`from_package' is required for import operations"))
    (let* ((target (%select-defpackage-form document package))
           (reader-package (or (and (source-form-package target)
                                    (find-package
                                     (source-form-package target)))
                               (find-package "COMMON-LISP-USER")))
           (top-text (source-form-text target))
           (clauses (cddr (%list-child-spans top-text reader-package)))
           (clause
             (find-if (lambda (span)
                        (%defpackage-operation-clause-p
                         (source-child-span-form span)
                         operation-token from-package reader-package))
                      clauses))
           (duplicate-p
             (and clause
                  (%defpackage-clause-has-symbol-p
                   (source-child-span-form clause)
                   operation-token symbol reader-package))))
      (if duplicate-p
          (make-defpackage-update-result
           :file (namestring pathname)
           :operation operation-token
           :package package
           :symbol symbol
           :from-package from-package
           :changed-p nil
           :duplicate-p t
           :before-text top-text
           :after-text top-text
           :diagnostics nil)
          (let* ((token (%defpackage-designator-token symbol))
                 (new-top-text
                   (if clause
                       (%insert-defpackage-clause-symbol
                        top-text clause operation-token token reader-package)
                       (%insert-defpackage-new-clause
                        top-text operation-token token from-package)))
                 (after (%replace-range before
                                        (source-form-start target)
                                        (source-form-end target)
                                        new-top-text))
                 (new-document
                   (%source-document-readable-or-error
                    pathname after initial-package-name)))
            (when write-p
              (%write-file-string pathname after))
            (make-defpackage-update-result
             :file (namestring pathname)
             :operation operation-token
             :package package
             :symbol symbol
             :from-package from-package
             :changed-p t
             :duplicate-p nil
             :before-text top-text
             :after-text new-top-text
             :diagnostics (source-document-diagnostics new-document)))))))

(defun plan-defpackage-update (file operation
                               &key root initial-package-name package
                               symbol from-package)
  "Plan a DEFPACKAGE edit without writing FILE."
  (%defpackage-update-result file operation nil
                             :root root
                             :initial-package-name initial-package-name
                             :package package
                             :symbol symbol
                             :from-package from-package))

(defun apply-defpackage-update (file operation
                                &key root initial-package-name package
                                symbol from-package)
  "Apply a source-preserving DEFPACKAGE edit to FILE."
  (%defpackage-update-result file operation t
                             :root root
                             :initial-package-name initial-package-name
                             :package package
                             :symbol symbol
                             :from-package from-package))
