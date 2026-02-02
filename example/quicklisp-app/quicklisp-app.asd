;;;; quicklisp-app.asd

(asdf:defsystem "quicklisp-app"
  :version "0.1.0"
  :serial t
  :pathname "src"
  :components ((:file "quicklisp-app"))
  :in-order-to ((asdf:test-op (asdf:test-op "quicklisp-app/test"))))

(asdf:defsystem "quicklisp-app/test"
  :depends-on ("quicklisp-app")
  :serial t
  :pathname "test"
  :components ((:file "quicklisp-app-test"))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (uiop:symbol-call :quicklisp-app-test :run)))

