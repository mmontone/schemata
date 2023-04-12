(asdf:defsystem #:schemata-tests
  :license "MIT"
  :description "Schema validation library"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :maintainer "Mariano Montone <marianomontone@gmail.com>"
  :depends-on (#:schemata #:stefil)
  :components ((:file "tests"))
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :schemata.tests :run-tests)))
