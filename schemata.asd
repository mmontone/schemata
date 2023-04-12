(asdf:defsystem #:schemata
  :description "Schemas validation, serialization and parsing for Common Lisp"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:alexandria
               :cl-json
               :cxml
               :local-time
               :chronicity
               :net-telent-date
               :access
               :generic-serializer)
  :components ((:file "package")
               (:file "schema")
               (:file "validation")
               (:file "serialization")
               (:file "parsing")
               (:file "serializable-class")
               (:file "schemata"))
  :in-order-to ((asdf:test-op (asdf:test-op :schemata-tests))))
