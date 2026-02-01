;;;; app-test.lisp

(defpackage #:app-test
  (:use #:cl)
  (:export #:run))

(in-package #:app-test)

(defun run ()
  (format t "Running app tests...~%")
  (assert (string= "lib-a + lib-b" (lib-a:message)))
  (assert (eql 0 (app:main)))
  (format t "App tests passed.~%")
  :ok)

