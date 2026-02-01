;;;; app.lisp

(defpackage #:app
  (:use #:cl)
  (:export #:main))

(in-package #:app)

(defun main (&optional (args nil))
  (format t "Example app running.~%")
  (format t "Message: ~A~%" (lib-a:message))
  (when args
    (format t "Args: ~{~A~^ ~}~%" args))
  0)

