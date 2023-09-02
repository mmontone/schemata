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

(defun schema-validation-function-name (schema-name)
  "The name of the function used by SATISFIES-SCHEMA type."
  (intern (format nil "VALID-~a-SCHEMA-P" schema-name)))

(defun register-schema (name schema)
  "Register SCHEMA under NAME."
  (setf (gethash name *schemas*) schema)
  ;; Create the function for SATISFIES-SCHEMA type.
  (setf (symbol-function (schema-validation-function-name name))
        (lambda (data)
          (null (validate-with-schema (find-schema name) data :error-p nil)))))

(defmacro define-schema (name schema)
  "Register SCHEMA under NAME.
The schema can then be accessed via FIND-SCHEMA."
  `(register-schema ',name (schema ,schema)))

(defmacro schema (schema-def)
  "Wrapper macro for schema definitions."
  `(parse-schema ',schema-def))

(deftype satisfies-schema (schema-name)
  "Common Lisp type for schemas."
  `(satisfies ,(schema-validation-function-name schema-name)))

(defclass schema ()
  ((documentation :initarg :documentation
                  :accessor schema-documentation
                  :initform nil)))

(defclass schema-reference-schema (schema)
  ((name :initarg :schema-name
         :accessor schema-name
         :type symbol)))

(defmethod print-object ((object schema-reference-schema) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (schema-name object) stream)))

(defun referenced-schema (schema)
  (find-schema (schema-name schema)))

(defclass type-schema (schema)
  ((type :initarg :type
         :accessor schema-type)))

(defmethod print-object ((object type-schema) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (schema-type object) stream)))

(defclass list-schema (schema)
  ((elements-schema :initarg :elements-schema
                    :accessor elements-schema
                    :type (not null)
                    :documentation "Schema of the elements of the list")))

(defclass object-schema (schema)
  ((name :initarg :name
         :accessor object-name
         :type (or string symbol)
         :documentation "The name of the object.")
   (attributes :initarg :attributes
               :accessor object-attributes
               :type list
               :initform nil)
   (class :initarg :class
          :accessor object-class
          :type (or null symbol)
          :initform nil)
   (ignore-unknown-attributes
    :initarg :ignore-unknown-attributes
    :accessor ignore-unknown-attributes
    :initform nil
    :type boolean)
   (serializer :initarg :serializer
               :accessor object-serializer
               :type (or null trivial-types:function-designator)
               :initform nil)
   (unserializer :initarg :unserializer
                 :accessor object-unserializer
                 :type (or null trivial-types:function-designator)
                 :initform nil)))

(defclass attribute-properties ()
  ((required :initarg :required
             :accessor attribute-required-p
             :initform t
             :type boolean)
   (required-message :initarg :required-message
                     :accessor attribute-required-message
                     :initform nil
                     :type (or string null))
   (default :initarg :default
            :accessor attribute-default
            :initform nil)
   (validator :initarg :validator
              :accessor attribute-validator
              :initform nil
              :type (or null trivial-types:function-designator))
   (add-validator :initarg :add-validator
                  :accessor attribute-add-validator
                  :initform nil
                  :type (or null trivial-types:function-designator))
   (parser :initarg :parser
           :accessor attribute-parser
           :initform nil
           :type (or null trivial-types:function-designator))
   (formatter :initarg :formatter
              :accessor attribute-formatter
              :initform nil
              :type (or null trivial-types:function-designator))
   (external-name :initarg :external-name
                  :accessor attribute-external-name
                  :type (or string null)
                  :initform nil)
   (serializer :initarg :serializer
               :accessor attribute-serializer
               :initform nil)
   (unserializer :initarg :unserializer
                 :accessor attribute-unserializer
                 :initform nil
                 :type (or null trivial-types:function-designator))))

(defclass attribute (schema attribute-properties)
  ((name :initarg :name
         :type symbol
         :accessor attribute-name)
   (type :initarg :type
         :accessor attribute-type
         :type schema)
   (accessor :initarg :accessor
             :initform nil
             :accessor attribute-accessor
             :type (or null symbol))
   (writer :initarg :writer
           :initform nil
           :type (or null trivial-types:function-designator))
   (reader :initarg :reader
           :initform nil
           :type (or null trivial-types:function-designator))
   (slot :initarg :slot
         :accessor attribute-slot
         :initform nil
         :type (or null symbol))))

(defun attribute-optional-p (attribute)
  (not (attribute-required-p attribute)))

(defun parse-schema (schema)
  (cond
    ((listp schema)
     (parse-schema-type (car schema) schema))

    ((find-schema schema nil)
     (make-instance 'schema-reference-schema :schema-name schema))
    ((trivial-types:type-specifier-p schema)
     (make-instance 'type-schema :type schema))
    (t (error "Cannot parse schema: ~S" schema))))

(defgeneric parse-schema-type (schema-type schema))

(defmethod parse-schema-type ((schema-type (eql 'object)) schema)
  (destructuring-bind (name attributes &optional options)
      (rest schema)
    (apply #'make-instance 'object-schema
           :name name
           :attributes (mapcar #'parse-attribute attributes)
           options)))

(defmethod parse-schema-type ((schema-type (eql 'list-of)) schema)
  (destructuring-bind (elements-schema &rest args)
      (rest schema)
    (apply #'make-instance 'list-schema
           :elements-schema (parse-schema elements-schema)
           args)))

(defmethod parse-schema-type ((schema-type (eql 'schema)) schema)
  (make-instance 'schema-reference-schema :schema-name (cadr schema)))

(defmethod parse-schema-type ((schema-type (eql 'ref)) schema)
  (make-instance 'schema-reference-schema :schema-name (cadr schema)))

(defmethod parse-schema-type ((schema-type t) schema)
  "If the other cases fail, just create a TYPE-SCHEMA."
  (assert (trivial-types:type-specifier-p schema)
          nil
          "Not a type specifier: ~s" schema)
  (make-instance 'type-schema :type schema))

(defun parse-attribute (attr)
  (destructuring-bind (name type &rest options)
      attr
    (apply #'make-instance 'attribute
           :name name
           :type (parse-schema type)
           options)))

(defun find-schema (name &optional (errorp t))
  "Find a schema definition by name"
  (multiple-value-bind (schema foundp)
      (gethash name *schemas*)
    (if (not foundp)
        (if errorp
            (error "Schema ~a not found" name)
            nil)
        schema)))

(defun find-object-attribute (object attribute-name &key (error-p t))
  (or (find attribute-name (object-attributes object) :key #'attribute-name)
      (when error-p
        (error "Attribute ~A not found in ~A" attribute-name object))))

(defun attribute-type-name (attribute)
  (let ((attribute-type (attribute-type attribute)))
    (if (listp attribute-type)
        (first attribute-type)
        attribute-type)))

(defun attribute-writer (attribute)
  (or (and (slot-value attribute 'writer)
           (alexandria:ensure-function (slot-value attribute 'writer)))
      (and (attribute-accessor attribute)
           (alexandria:ensure-function
            `(setf
              ,(attribute-accessor attribute))))
      (lambda (value obj)
        (setf (access:access obj (attribute-name attribute)) value))))

(defun attribute-reader (attribute)
  (or
   (and (slot-value attribute 'reader)
        (alexandria:ensure-function (slot-value attribute 'reader)))
   (and (attribute-accessor attribute)
        (alexandria:ensure-function (attribute-accessor attribute)))
   (lambda (obj)
     (access:access obj (attribute-name attribute)))))

(defun attribute-spec (attribute)
  (list (attribute-name attribute)
        (schema-spec (attribute-type attribute))
        :required (attribute-required-p attribute)
        :required-message (attribute-required-message attribute)
        :default (attribute-default attribute)
        :accessor (slot-value attribute 'accessor)
        :writer (slot-value attribute 'writer)
        :reader (slot-value attribute 'reader)
        :validator (slot-value attribute 'validator)
        :add-validator (slot-value attribute 'add-validator)
        :parser (slot-value attribute 'parser)
        :formatter (slot-value attribute 'formatter)
        :external-name (slot-value attribute 'external-name)
        :serializer (slot-value attribute 'serializer)
        :unserializer (slot-value attribute 'unserializer)
        :slot (attribute-slot attribute)))

(defun schema-spec (schema)
  (typecase schema
    (type-schema
     (schema-type schema))
    (schema-reference-schema
     (list 'ref (schema-name schema)))
    (object-schema
     (list 'object (object-name schema)
           (mapcar #'attribute-spec (object-attributes schema))
           (list :class (object-class schema)
                 :serializer (object-serializer schema)
                 :unserializer (object-unserializer schema)
                 :ignore-unknown-attributes (ignore-unknown-attributes schema))))
    (list-schema
     (list 'list-of (schema-spec (elements-schema schema))))))
