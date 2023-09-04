(defpackage :schemata-generators
  (:use :cl :schemata :check-it))

(in-package :schemata-generators)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-genex-macro maybe (form)
    `(or nil ,form))

  ;; Doesn't work because it caches the call func
  ;; (def-generator func (func)
  ;;   (generator (funcall func)))

  (defclass func (check-it::custom-generator)
    ((check-it::bias :initform 1.0
                     :accessor check-it::bias
                     :allocation :class)
     (func :initarg :func)))
  (setf (get 'func 'check-it::genex-type) 'generator)
  (setf (get 'func 'check-it::generator-form)
        `(lambda ,'(func) (make-instance ','func ,@'(:func func))))

  (defmethod generate ((generator func))
    (let ((func (slot-value generator 'func)))
      (funcall func)))
  )

;; check-it patch
;; Allow functions as generators
(defmethod check-it::generate ((generator function))
  (funcall generator))

(defgeneric generator-for-schema (schema))
(defgeneric generator-for-type (type type-schema))

(defmethod generator-for-schema :around ((schema schema))
  (if (schema-generator schema)
      (schema-generator schema)
      (call-next-method)))

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
                  (generate
                   (if (schema-generator attribute)
                       (schema-generator attribute)
                       (attribute-type attribute)))))))
    (if (attribute-required-p attribute)
        (generator assoc-generator)
        (generator (maybe assoc-generator)))))

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
