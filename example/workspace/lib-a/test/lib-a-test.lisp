;;;; lib-a-test.lisp

(defpackage #:lib-a-test
  (:use #:cl)
  (:export #:run))

(in-package #:lib-a-test)

(defun run ()
  (format t "Running lib-a tests...~%")
  (assert (string= "lib-a + lib-b" (lib-a:message)))
  (format t "lib-a tests passed.~%")
  :ok)

