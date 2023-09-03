(defpackage :schemata-generators
  (:use :cl :schemata :check-it))

(in-package :schemata-generators)

(def-genex-macro maybe (form)
  `(or nil ,form))

(generate (generator (list (maybe (integer)))))

;; Doesn't work because it caches the call func
;; (def-generator func (func)
;;   (generator (funcall func)))

(DEFCLASS FUNC (CHECK-IT::CUSTOM-GENERATOR)
            ((CHECK-IT::BIAS :INITFORM 1.0 :ACCESSOR CHECK-IT::BIAS :ALLOCATION
                             :CLASS)
             (FUNC :INITARG :FUNC)))
(SETF (GET 'FUNC 'CHECK-IT::GENEX-TYPE) 'GENERATOR)
(SETF (GET 'FUNC 'CHECK-IT::GENERATOR-FORM)
      `(LAMBDA ,'(FUNC) (MAKE-INSTANCE ','FUNC ,@'(:FUNC FUNC))))
(DEFMETHOD GENERATE ((GENERATOR FUNC))
  (LET ((FUNC (SLOT-VALUE GENERATOR 'FUNC)))
    (FUNCALL FUNC)))

;;(generate (generator (func (lambda () 22))))

(defparameter *g* (generator (func (lambda ()
                                     (list
                                      (generate (generator (maybe (integer))))
                                      (generate (generator (maybe (string)))))))))

(generate *g*)

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
        (generator (maybe assoc-generator)))))

(defmethod generator-for-schema ((schema object-schema))
  (generator
   (func (lambda ()
           (mapcar #'generate
                   (remove nil
                           (loop for attribute in (object-attributes schema)
                                 collect (attribute-generator attribute))))))))
