(asdf:defsystem #:schemata.json-schema
  :description "JSON Schema support for Schemata"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:schemata)
  :components ((:file "json-schema")))
