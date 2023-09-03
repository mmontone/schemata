(defpackage :schemata-generators
  (:use :cl :schemata :check-it))

(in-package :schemata-generators)

;; check-it patch
;; Allow functions as generators
(defmethod check-it::generate ((generator function))
  (funcall generator))

(defgeneric generator-for-schema (schema))
(defgeneric generator-for-type (type type-schema))

(defmethod generator-for-schema ((schema type-schema))
  (generator-for-type (schema-type schema) schema))

(defmethod generator-for-type ((type (eql 'integer)) type-schema)
  (generator (integer)))

(defmethod generator-for-type ((type (eql 'boolean)) type-schema)
  (generator (boolean)))

(defmethod generator-for-type ((type (eql 'string)) type-schema)
  (generator (string)))

(defun attribute-generator (attribute)
  (let ((assoc-generator
          (lambda ()
            (cons (attribute-name attribute)
                  (generate (generator-for-schema (attribute-type attribute)))))))
    (if (attribute-required-p attribute)
        assoc-generator
        (generator
         (chain ((gen assoc-generator))
                (generator (maybe gen)))))))

(defmethod generator-for-schema ((schema object-schema))
  (lambda ()
    (remove nil
            (mapcar #'generate
                    (loop for attribute in (object-attributes schema)
                          collect (attribute-generator attribute))))))

(defmethod generator-for-schema ((schema schema-reference-schema))
  (schemata::referenced-schema schema))

(defmethod generator-for-schema ((schema list-schema))
  (let ((element-generator (generator-for-schema (schemata::elements-schema schema))))
    (generator (list element-generator))))

;; Plug into check-it
;; Allows to call check-it:generate with a schema directly to generate random data.
(defmethod check-it::generate ((generator schemata:schema))
  (generate (generator-for-schema generator)))
