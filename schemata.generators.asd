(asdf:defsystem #:schemata.generators
  :description "Random data generation from Schemata schemas."
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:schemata :check-it)
  :components ((:file "generators")))
