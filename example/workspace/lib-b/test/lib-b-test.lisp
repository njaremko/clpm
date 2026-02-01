;;;; lib-b-test.lisp

(defpackage #:lib-b-test
  (:use #:cl)
  (:export #:run))

(in-package #:lib-b-test)

(defun run ()
  (format t "Running lib-b tests...~%")
  (assert (string= "lib-b" (lib-b:message)))
  (format t "lib-b tests passed.~%")
  :ok)

