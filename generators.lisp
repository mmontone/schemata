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

;;(generate (generator (func (lambda () 22))))

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
          (generator (func (lambda ()
                             (cons (attribute-name attribute)
                                   (generate (generator-for-schema (attribute-type attribute)))))))))
    (if (attribute-required-p attribute)
        assoc-generator
        (generator
         (chain ((gen assoc-generator))
                (generator (maybe gen)))))))

(defmethod generator-for-schema ((schema object-schema))
  (generator
   (func (lambda ()
           (remove nil
                   (mapcar #'generate
                           (loop for attribute in (object-attributes schema)
                                 collect (attribute-generator attribute))))))))

(defmethod generator-for-schema ((schema schema-reference-schema))
  (find-schema (schemata::referenced-schema schema)))

;; Plug into check-it
;; Allows to call check-it:generate with a schema directly to generate random data.
(defmethod check-it::generate ((generator schemata:schema))
  (generate (generator-for-schema generator)))
