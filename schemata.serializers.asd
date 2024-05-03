(asdf:defsystem :schemata.serializers
  :description "Schema serialization and unserialization."
  :author "Mariano Montone <marianomontone@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:schemata :generic-serializer)
  :components ((:file "serialization")
               (:file "unserialization")))
