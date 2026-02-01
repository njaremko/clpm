;;;; app.asd

(asdf:defsystem "app"
  :version "0.1.0"
  :depends-on ("lib-a")
  :serial t
  :pathname "src"
  :components ((:file "app"))
  :in-order-to ((asdf:test-op (asdf:test-op "app/test"))))

(asdf:defsystem "app/test"
  :depends-on ("app")
  :serial t
  :pathname "test"
  :components ((:file "app-test"))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (uiop:symbol-call :app-test :run)))

