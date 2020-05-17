(asdf:defsystem #:schemata.tests
  :description "Tests for Schemata"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:schemata :fiveam)
  :components ((:file "tests")))
