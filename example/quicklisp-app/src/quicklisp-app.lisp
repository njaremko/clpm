;;;; quicklisp-app.lisp

(defpackage #:quicklisp-app
  (:use #:cl)
  (:export #:main))

(in-package #:quicklisp-app)

(defun main (&optional (args nil))
  (declare (ignore args))
  ;; Use Alexandria to prove it's loaded from Quicklisp.
  (let ((xs (alexandria:flatten '((1 2) (3) (4 5)))))
    (format t "Flattened: ~S~%" xs))
  0)

