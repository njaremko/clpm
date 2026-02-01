;;;; lib-b.asd

(asdf:defsystem "lib-b"
  :version "0.1.0"
  :serial t
  :pathname "src"
  :components ((:file "lib-b"))
  :in-order-to ((asdf:test-op (asdf:test-op "lib-b/test"))))

(asdf:defsystem "lib-b/test"
  :depends-on ("lib-b")
  :serial t
  :pathname "test"
  :components ((:file "lib-b-test"))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (uiop:symbol-call :lib-b-test :run)))

