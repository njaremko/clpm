;;;; quicklisp-app-test.lisp

(defpackage #:quicklisp-app-test
  (:use #:cl #:fiveam)
  (:shadow #:run)
  (:export #:run))

(in-package #:quicklisp-app-test)

(defun run ()
  (format t "Running quicklisp-app tests...~%")
  (assert (eql 0 (quicklisp-app:main)))
  (format t "quicklisp-app tests passed.~%")
  :ok)
