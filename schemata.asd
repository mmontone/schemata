(asdf:defsystem #:schemata
  :description "Schemas validation, serialization and parsing for Common Lisp."
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:alexandria
               :cl-json
               :local-time
               :chronicity
               :net-telent-date
               :access
               :trivial-types
               :closer-mop)
  :components ((:file "package")
               (:file "schema")
               (:file "schema-class")
               (:file "validation")
               (:file "parsing")
               (:file "schemata"))
  :in-order-to ((asdf:test-op (asdf:test-op :schemata.tests))))
