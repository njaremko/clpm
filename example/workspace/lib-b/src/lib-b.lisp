;;;; lib-b.lisp

(defpackage #:lib-b
  (:use #:cl)
  (:export #:message))

(in-package #:lib-b)

(defun message ()
  "Return a short message."
  "lib-b")

