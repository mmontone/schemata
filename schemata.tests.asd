(asdf:defsystem #:schemata.tests
  :description "Tests for Schemata"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:schemata :stefil :generic-serializer)
  :components ((:module "tests"
                :components
                ((:file "tests"))))
  :perform (asdf:test-op (op c)
-                         (uiop:symbol-call :schemata.tests :run-tests)))
