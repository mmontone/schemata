;;;; package.lisp

(defpackage #:schemata
  (:use #:cl)
  (:export #:serialize-with-schema
           #:find-schema
           #:schema
           #:define-schema
           #:schema-type
           #:validation
           #:validation-error
           #:validate-with-schema
           #:parse-with-schema
           #:unserialize-with-schema
           #:populate-with-schema
           #:patch-with-schema
           #:object
           #:ref
           #:list-of
           #:object-name
           #:object-attributes
           #:object-options
           #:object-option
           #:find-object-attribute
           #:object-documentation
           #:object-class
           #:attribute-name
           #:attribute-type
           #:attribute-type-name
           #:attribute-option
           #:attribute-optional-p
           #:attribute-accessor
           #:attribute-validator
           #:attribute-add-validator
           #:attribute-writer
           #:attribute-reader
           #:attribute-parser
           #:attribute-formatter
           #:attribute-documentation
           #:attribute-external-name

           #:serializable-class
           #:serializable-class-schema))
