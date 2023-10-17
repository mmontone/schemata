;;;; package.lisp

(defpackage #:schemata
  (:use #:cl)
  (:export #:serialize-with-schema
           #:find-schema
           #:schema
           #:defschema
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
           #:alist
           #:alist-of
           #:plist
           #:plist-of
           #:const
           #:member-of
           #:object-name
           #:object-attributes
           #:object-options
           #:object-option
           #:find-object-attribute
           #:schema-documentation
           #:schema-generator
           #:object-class
           #:attribute-name
           #:attribute-type
           #:attribute-type-name
           #:attribute-option
           #:attribute-required-p
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

           #:object-schema
           #:type-schema
           #:list-of-schema
           #:list-schema
           #:cons-schema
           #:alist-schema
           #:alist-of-schema
           #:plist-schema
           #:plist-of-schema
           #:attribute
           #:attribute-properties
           #:schema-reference-schema
           #:schema-spec
           #:satisfies-schema

           #:schema-object
           #:def-schema-class
           #:schema-class
           #:schema-class-schema

           #:generate-schema-from-class))
