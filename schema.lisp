(in-package :schemata)

(defvar *schemas* (make-hash-table)
  "Table with defined schemas.")
(defvar *ignore-unknown-object-attributes* nil
  "If T, unknown object attributes are ignored when validating using a schema. Default is NIL.")
(defvar *null-values* (list nil)
  "The list of values considered null. Attribute with these values are not
serialized when optional. Useful for treatment of special values, like :null in Postmodern library")

(defun null-value (value)
  (member value *null-values*))

(defun register-schema (name definition)
  (setf (gethash name *schemas*)
        definition))

(defmacro define-schema (name schema)
  "Define a schema"
  `(register-schema ',name
                    (schema ,schema)))

(defmacro schema (schema-def)
  `(quote ,schema-def))

(defun find-schema (name &optional (errorp t))
  "Find a schema definition by name"
  (multiple-value-bind (schema foundp)
      (gethash name *schemas*)
    (if (not foundp)
        (if errorp
            (error "Schema ~a not found" name)
            nil)
        schema)))

(defun object-name (object)
  (second object))

(defun object-attributes (object)
  (third object))

(defun object-options (object)
  (cdddr object))

(defun object-option (option object)
  (cadr (find option (object-options object) :key 'car)))

(defun find-object-attribute (object attribute-name &key (error-p t))
  (loop for attribute in (object-attributes object)
        when (equalp (string (attribute-name attribute))
                     (string attribute-name))
          do (return-from find-object-attribute attribute))
  (when error-p
    (error "Attribute ~A not found in ~A" attribute-name object)))

(defun object-documentation (object)
  (object-option :documentation object))

(defun object-class (object)
  "Returns the CLOS class associated with an object. May be null."
  (object-option :class object))

(defun schema-type (schema)
  (if (listp schema)
      (first schema)
      schema))

(defun attribute-name (attribute)
  (first attribute))

(defun attribute-type (attribute)
  (second attribute))

(defun attribute-type-name (attribute)
  (let ((attribute-type (attribute-type attribute)))
    (if (listp attribute-type)
        (first attribute-type)
        attribute-type)))

(defun attribute-options (attribute)
  (cddr attribute))

(defun attribute-option (option attribute)
  (getf (attribute-options attribute) option))

(defun attribute-optional-p (attribute)
  (attribute-option :optional attribute))

(defun attribute-accessor (attribute)
  (attribute-option :accessor attribute))

(defun attribute-validator (attribute)
  (attribute-option :validator attribute))

(defun attribute-add-validator (attribute)
  (attribute-option :add-validator attribute))

(defun attribute-writer (attribute)
  (or (and (attribute-option :writer attribute)
           (alexandria:ensure-function (attribute-option :writer attribute)))
      (and (attribute-accessor attribute)
           (alexandria:ensure-function
            `(setf
              ,(attribute-accessor attribute))))
      (lambda (value obj)
        (setf (access:access obj (attribute-name attribute)) value))))

(defun attribute-reader (attribute)
  (or
   (and (attribute-option :reader attribute)
        (alexandria:ensure-function (attribute-option :reader attribute)))
   (and (attribute-accessor attribute)
        (alexandria:ensure-function (attribute-accessor attribute)))
   (lambda (obj)
     (access:access obj (attribute-name attribute)))))

(defun attribute-parser (attribute)
  (attribute-option :parser attribute))

(defun attribute-formatter (attribute)
  (attribute-option :formatter attribute))

(defun attribute-documentation (attribute)
  (attribute-option :documentation attribute))

(defun attribute-external-name (attribute)
  "Name of the field that is shown in error messages (and in serialization?)"
  (attribute-option :external-name attribute))
