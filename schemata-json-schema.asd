(asdf:defsystem #:schemata-json-schema
  :license "MIT"
  :description "Schema validation library"
  :author "Mariano Montone <marianomontone@gmail.com>"
  :maintainer "Mariano Montone <marianomontone@gmail.com>"
  :depends-on (#:schemata)
  :components ((:file "json-schema")))
