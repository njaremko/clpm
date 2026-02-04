;;;; errors.lisp - Error conditions for CLPM

(in-package #:clpm.errors)

;;; Base condition

(define-condition clpm-error (error)
  ((message :initarg :message
            :initform nil
            :reader clpm-error-message))
  (:report (lambda (condition stream)
             (format stream "~A" (clpm-error-message condition)))))

;;; Specific error types

(define-condition clpm-user-error (clpm-error)
  ()
  (:documentation "Error caused by user input or configuration"))

(define-condition clpm-missing-tool-error (clpm-user-error)
  ((tool :initarg :tool :initform nil :reader clpm-missing-tool-error-tool)
   (install-hints :initarg :install-hints :initform nil
                  :reader clpm-missing-tool-error-install-hints))
  (:report (lambda (condition stream)
             (format stream "Missing tool: ~A~@[~%Install with:~%~{  ~A~%~}~]"
                     (or (clpm-missing-tool-error-tool condition) "?")
                     (clpm-missing-tool-error-install-hints condition)))))

(define-condition clpm-parse-error (clpm-error)
  ((file :initarg :file :initform nil :reader clpm-parse-error-file)
   (line :initarg :line :initform nil :reader clpm-parse-error-line)
   (detail :initarg :detail :initform nil :reader clpm-parse-error-detail))
  (:report (lambda (condition stream)
             (format stream "Parse error~@[ in ~A~]~@[ at line ~D~]: ~A"
                     (clpm-parse-error-file condition)
                     (clpm-parse-error-line condition)
                     (or (clpm-parse-error-detail condition)
                         (clpm-error-message condition))))))

(define-condition clpm-fetch-error (clpm-error)
  ((url :initarg :url :initform nil :reader clpm-fetch-error-url)
   (status :initarg :status :initform nil :reader clpm-fetch-error-status))
  (:report (lambda (condition stream)
             (format stream "Failed to fetch~@[ ~A~]~@[ (status ~A)~]: ~A"
                     (clpm-fetch-error-url condition)
                     (clpm-fetch-error-status condition)
                     (clpm-error-message condition)))))

(define-condition clpm-hash-mismatch-error (clpm-error)
  ((expected :initarg :expected :reader clpm-hash-mismatch-expected)
   (actual :initarg :actual :reader clpm-hash-mismatch-actual)
   (artifact :initarg :artifact :initform nil :reader clpm-hash-mismatch-artifact))
  (:report (lambda (condition stream)
             (format stream "Hash mismatch~@[ for ~A~]:~%  Expected: ~A~%  Actual:   ~A"
                     (clpm-hash-mismatch-artifact condition)
                     (clpm-hash-mismatch-expected condition)
                     (clpm-hash-mismatch-actual condition)))))

(define-condition clpm-signature-error (clpm-error)
  ((key-id :initarg :key-id :initform nil :reader clpm-signature-error-key-id)
   (file :initarg :file :initform nil :reader clpm-signature-error-file))
  (:report (lambda (condition stream)
             (format stream "Signature verification failed~@[ for ~A~]~@[ with key ~A~]: ~A"
                     (clpm-signature-error-file condition)
                     (clpm-signature-error-key-id condition)
                     (clpm-error-message condition)))))

(define-condition clpm-resolve-error (clpm-error)
  ((systems :initarg :systems :initform nil :reader clpm-resolve-error-systems)
   (conflict-chain :initarg :conflict-chain :initform nil
                   :reader clpm-resolve-error-conflict-chain)
   (explanation :initarg :explanation :initform nil
                :reader clpm-resolve-error-explanation))
  (:report (lambda (condition stream)
             (format stream "Failed to resolve dependencies: ~A~@[~%Conflict chain:~%~{  ~A~%~}~]"
                     (clpm-error-message condition)
                     (clpm-resolve-error-conflict-chain condition)))))

(define-condition clpm-build-error (clpm-error)
  ((system :initarg :system :initform nil :reader clpm-build-error-system)
   (log-file :initarg :log-file :initform nil :reader clpm-build-error-log-file)
   (exit-code :initarg :exit-code :initform nil :reader clpm-build-error-exit-code))
  (:report (lambda (condition stream)
             (format stream "Build failed~@[ for system ~A~]~@[ (exit code ~D)~]: ~A~@[~%See log: ~A~]"
                     (clpm-build-error-system condition)
                     (clpm-build-error-exit-code condition)
                     (clpm-error-message condition)
                     (clpm-build-error-log-file condition)))))

(define-condition clpm-missing-native-dep-error (clpm-error)
  ((native-dep :initarg :native-dep :reader clpm-missing-native-dep)
   (required-by :initarg :required-by :initform nil :reader clpm-missing-native-dep-required-by)
   (install-hints :initarg :install-hints :initform nil :reader clpm-missing-native-dep-hints))
  (:report (lambda (condition stream)
             (format stream "Missing native dependency: ~A~@[~%Required by: ~A~]~@[~%Install with:~%~{  ~A~%~}~]"
                     (clpm-missing-native-dep condition)
                     (clpm-missing-native-dep-required-by condition)
                     (clpm-missing-native-dep-hints condition)))))

;;; Helper functions

(defun signal-error (condition-type message &rest args)
  "Signal a CLPM error with a formatted message."
  (error condition-type :message (apply #'format nil message args)))

(defun format-error (condition &optional stream)
  "Format an error condition for user display."
  (let ((stream (or stream *error-output*)))
    (format stream "~&error: ~A~%" condition)))
