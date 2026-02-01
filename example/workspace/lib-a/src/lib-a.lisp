;;;; lib-a.lisp

(defpackage #:lib-a
  (:use #:cl)
  (:export #:message))

(in-package #:lib-a)

(defun message ()
  (format nil "lib-a + ~A" (lib-b:message)))

