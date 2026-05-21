;;;; log.lisp - User-facing logging helpers
;;;;
;;;; These live in their own package so lower-layer modules (e.g.
;;;; `clpm.registry`) can call them without taking a dependency on the
;;;; CLI-layer `clpm.commands` package.

(in-package #:clpm.log)

(defvar *verbose* nil
  "If non-NIL, `log-verbose` prints. Bound globally by the CLI driver.")

(defun log-info (format-string &rest args)
  "Print an info message to *standard-output*."
  (format t "~&~?~%" format-string args))

(defun log-verbose (format-string &rest args)
  "Print a verbose message to *standard-output* only when `*verbose*` is set."
  (when *verbose*
    (format t "~&  ~?~%" format-string args)))

(defun log-warning (format-string &rest args)
  "Print a warning message to *error-output*."
  (format *error-output* "~&WARNING: ~?~%" format-string args))

(defun log-error (format-string &rest args)
  "Print an error message to *error-output*."
  (format *error-output* "~&error: ~?~%" format-string args))
