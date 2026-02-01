;;;; lib-a.asd

(asdf:defsystem "lib-a"
  :version "0.1.0"
  :depends-on ("lib-b")
  :serial t
  :pathname "src"
  :components ((:file "lib-a"))
  :in-order-to ((asdf:test-op (asdf:test-op "lib-a/test"))))

(asdf:defsystem "lib-a/test"
  :depends-on ("lib-a")
  :serial t
  :pathname "test"
  :components ((:file "lib-a-test"))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (uiop:symbol-call :lib-a-test :run)))

