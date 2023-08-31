;;;; schemata.lisp

(in-package #:schemata)

(defun populate-with-schema (schema object data &key exclude)
  "Populate CLOS objects from data + schema.
Attributes members of EXCLUDE parameter are not populated."
  (loop for attribute in (object-attributes schema)
        unless (member (attribute-name attribute) exclude)
          do
             (multiple-value-bind (value found)
                 (access:access data (attribute-name attribute) :skip-call? t) ;; skip calls, as data should be plain data
               (when found
                 (let ((attribute-writer (attribute-writer attribute)))
                   (funcall attribute-writer
                            value
                            object))))))

(defun patch-with-schema (schema object data)
  "Populate CLOS objects from data + schema.
Only populates attributes available in DATA, validating them.
Useful for PATCH rest api operations implementations.
DATA should be an association list."
  (loop :for data-attribute :in data
        :for schema-attribute := (or (find (string (car data-attribute))
                                           (object-attributes schema)
                                           :key (alexandria:compose 'string 'attribute-name)
                                           :test 'equalp)
                                     (validation-error "Attribute not found: ~a" (car data-attribute)))
        :do
           (schema-validate (attribute-type schema-attribute)
                            (cdr data-attribute))
           (let ((attribute-writer (attribute-writer schema-attribute)))
             (funcall attribute-writer
                      (cdr data-attribute)
                      object))))
