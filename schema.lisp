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

(defparameter *satisfies-function-names*
  (make-hash-table :test 'equalp))

(defun schema-satisfies-function-name (schema-spec)
  "The name of the function used by SATISFIES-SCHEMA type."
  (alexandria:if-let (func-name (gethash schema-spec *satisfies-function-names*))
    func-name
    (let ((func-name (intern (format nil "SATISFIES-~a-SCHEMA-P" (gensym)))))
      (setf (gethash schema-spec *satisfies-function-names*) func-name)
      (setf (symbol-function func-name)
            (lambda (data)
              (null (validate-with-schema (parse-schema schema-spec) data :error-p nil))))
      func-name)))

(defun register-schema (name schema)
  "Register SCHEMA under NAME."
  (setf (gethash name *schemas*) schema))

(defmacro defschema (name schema)
  "Register SCHEMA under NAME.
The schema can then be accessed via FIND-SCHEMA."
  `(let ((schema (schema ,schema)))
     (register-schema ',name schema)
     schema))

(defmacro schema (schema-def)
  "Wrapper macro for schema definitions."
  `(parse-schema ',schema-def))

(deftype satisfies-schema (schema-spec)
  "Common Lisp type for schemas."
  `(satisfies ,(schema-satisfies-function-name schema-spec)))

(defclass schema ()
  ((documentation :initarg :documentation
                  :accessor schema-documentation
                  :type (or null string)
                  :initform nil)
   (generator :initarg :generator
              :accessor schema-generator
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

(defclass or-schema (schema)
  ((schemas :initarg :schemas
            :accessor schemas-of)))

(defclass and-schema (schema)
  ((schemas :initarg :schemas
            :accessor schemas-of)))

(defclass type-schema (schema)
  ((type :initarg :type
         :accessor schema-type))
  (:documentation "Schema for a Common Lisp type.

Syntax: (schema type)

Examples:

    (schema string)
    (schema integer)"))

(defmethod print-object ((object type-schema) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (schema-type object) stream)))

(defclass cons-schema (schema)
  ((car-schema :initarg :car-schema
               :accessor car-schema
               :documentation "The schema of CAR.")
   (cdr-schema :initarg :cdr-schema
               :accessor cdr-schema
               :documentation "The schema of CDR."))
  (:documentation "Schema for CONSes.

Syntax: (cons car-schema cdr-schema)

Examples:

    (schema (cons symbol string))"))

(defmethod print-object ((schema cons-schema) stream)
  (print-unreadable-object (schema stream :type t :identity t)
    (format stream "~a ~a" (car-schema schema) (cdr-schema schema))))

(defclass list-schema (schema)
  ((schemas :initarg :schemas
            :accessor list-schemas
            :initform nil))
  (:documentation "Schema for lists.

Syntax: (list &rest schemas)

Examples:

    (schema (list string number))
    (schema (list symbol number boolean))

Data matches when it is a list of the same size and the list schemas match.
For instance, for the schema: (list symbol number symbol),
'(foo 33 bar) matches, but '(foo 33) does not."))

(defclass list-of-schema (schema)
  ((elements-schema :initarg :elements-schema
                    :accessor elements-schema
                    :type (not null)
                    :documentation "Schema of the elements of the list"))
  (:documentation "Schema for list with elements of certain type/schema.

Syntax: (list-of schema)

Examples:

    (schema (list-of string))
    (schema (list-of (or string number)))"))

(defmethod print-object ((schema list-of-schema) stream)
  (print-unreadable-object (schema stream :type t :identity t)
    (print-object (elements-schema schema) stream)))

(defclass alist-of-schema (schema)
  ((key-schema :initarg :key-schema
               :accessor key-schema)
   (value-schema :initarg :value-schema
                 :accessor value-schema))
  (:documentation "Schema for association lists with certain type of keys and values.

Syntax: (alist-of (key-schema . value-schema) &rest options)

Examples:

    (schema (alist-of (keyword . string)))"))

(defclass alist-schema (schema)
  ((members :initarg :members
            :accessor alist-members)
   (required-keys :initarg :required-keys
                  :initform t
                  :accessor required-keys
                  :type (or boolean list)
                  :documentation "If T (default), all keys are considered required.
If a list, only those listed are considered required.")
   (optional-keys :initarg :optional-keys
                  :initform nil
                  :type (or boolean list)
                  :accessor optional-keys
                  :documentation "If T, then all keys are considered optional.
If a list, then the keys listed are considered optional.")
   (allow-other-keys :initarg :allow-other-keys
                     :accessor allow-other-keys-p
                     :initform t
                     :documentation "Whether other keys than the specified are allowed in the data being checked."))
  (:documentation "Schema for association lists with certain keys and values.

Syntax: (alist alist &rest options)

where alist is a list of conses with key and schema.

Examples:

    (schema (alist ((:x . string)(:y . number))))
    (schema (alist ((:x . string)(:y . number)) :optional-keys (:y)))
"))

(defclass plist-of-schema (schema)
  ((key-schema :initarg :key-schema
               :accessor key-schema)
   (value-schema :initarg :value-schema
                 :accessor value-schema)))

(defclass plist-schema (schema)
  ((members :initarg :members
            :accessor plist-members)
   (required-keys :initarg :required-keys
                  :initform t
                  :accessor required-keys)
   (optional-keys :initarg :optional-keys
                  :initform nil
                  :accessor optional-keys)
   (allow-other-keys :initarg :allow-other-keys
                     :accessor allow-other-keys-p
                     :initform t)))

(defclass hash-table-of-schema (schema)
  ((key-schema :initarg :key-schema
               :accessor key-schema)
   (value-schema :initarg :value-schema
                 :accessor value-schema)))

(defclass hash-table-schema (schema)
  ((members :initarg :members
            :accessor plist-members)
   (required-keys :initarg :required-keys
                  :initform t
                  :accessor required-keys)
   (optional-keys :initarg :optional-keys
                  :initform nil
                  :accessor optional-keys)
   (allow-other-keys :initarg :allow-other-keys
                     :accessor allow-other-keys-p
                     :initform t)))

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

(defmethod print-object ((attr attribute) stream)
  (print-unreadable-object (attr stream :type t :identity t)
    (prin1 (attribute-name attr) stream)))

(defun attribute-optional-p (attribute)
  (not (attribute-required-p attribute)))

(defun parse-schema (schema-spec)
  (cond
    ;; Schema specs with a list form dispach using PARSE-SCHEMA-TYPE
    ((listp schema-spec)
     (parse-schema-type (car schema-spec) schema-spec))
    ;; When schema is not a list spec, try to find a registered schema first
    ((find-schema schema-spec nil)
     (make-instance 'schema-reference-schema :schema-name schema-spec))
    ;; If not found, and it is a valid type specifier, then build a type-schema
    ((trivial-types:type-specifier-p schema-spec)
     (make-instance 'type-schema :type schema-spec))
    (t (error "Cannot parse schema: ~S" schema-spec))))

(defgeneric parse-schema-type (schema-type schema-spec))

(defmethod parse-schema-type ((schema-type (eql 'object)) schema-spec)
  (destructuring-bind (name attributes &optional options)
      (rest schema-spec)
    (apply #'make-instance 'object-schema
           :name name
           :attributes (mapcar #'parse-attribute attributes)
           options)))

(defmethod parse-schema-type ((schema-type (eql 'or)) schema-spec)
  (make-instance 'or-schema :schemas (mapcar #'parse-schema (rest schema-spec))))

(defmethod parse-schema-type ((schema-type (eql 'and)) schema-spec)
  (make-instance 'and-schema :schemas (mapcar #'parse-schema (rest schema-spec))))

(defmethod parse-schema-type ((schema-type (eql 'cons)) schema-spec)
  (destructuring-bind (car-schema cdr-schema) (rest schema-spec)
    (make-instance 'cons-schema
                   :car-schema (parse-schema car-schema)
                   :cdr-schema (parse-schema cdr-schema))))

(defmethod parse-schema-type ((schema-type (eql 'list)) schema-spec)
  (make-instance 'list-schema :schemas (mapcar #'parse-schema (rest schema-spec))))

(defmethod parse-schema-type ((schema-type (eql 'list-of)) schema-spec)
  (destructuring-bind (elements-schema &rest args)
      (rest schema-spec)
    (apply #'make-instance 'list-of-schema
           :elements-schema (parse-schema elements-schema)
           args)))

(defmethod parse-schema-type ((schema-type (eql 'alist-of)) schema-spec)
  (destructuring-bind (key-schema . value-schema) (second schema-spec)
    (make-instance 'alist-of-schema
                   :key-schema (parse-schema key-schema)
                   :value-schema (parse-schema value-schema))))

(defmethod parse-schema-type ((schema-type (eql 'alist)) schema-spec)
  (destructuring-bind (alist &rest options) (rest schema-spec)
    (apply #'make-instance 'alist-schema
           :members (mapcar (lambda (member)
                              (check-type member cons)
                              (check-type (car member) (or symbol string))
                              (cons (car member) (parse-schema (cdr member))))
                            alist)
           options)))

(defmethod parse-schema-type ((schema-type (eql 'plist-of)) schema-spec)
  (destructuring-bind (key-schema value-schema) (rest schema-spec)
    (make-instance 'plist-of-schema
                   :key-schema (parse-schema key-schema)
                   :value-schema (parse-schema value-schema))))

(defmethod parse-schema-type ((schema-type (eql 'plist)) schema-spec)
  (destructuring-bind (plist &rest options) (rest schema-spec)
    (apply #'make-instance 'plist-schema
           :members (loop for key in plist by #'cddr
                          for value in (rest plist) by #'cddr
                          collect
                          (cons (the (or symbol string) key)
                                (parse-schema value)))
           options)))

(defmethod parse-schema-type ((schema-type (eql 'schema)) schema-spec)
  (make-instance 'schema-reference-schema :schema-name (cadr schema-spec)))

(defmethod parse-schema-type ((schema-type (eql 'ref)) schema-spec)
  (make-instance 'schema-reference-schema :schema-name (cadr schema-spec)))

(defmethod parse-schema-type ((schema-type t) schema-spec)
  "If the other cases fail, just create a TYPE-SCHEMA."
  (assert (trivial-types:type-specifier-p schema-spec)
          nil
          "Not a type specifier: ~s" schema-spec)
  (make-instance 'type-schema :type schema-spec))

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
  (etypecase schema
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
    (list-of-schema
     (list 'list-of (schema-spec (elements-schema schema))))
    (or-schema
     (list* 'or (mapcar #'schema-spec (schemas-of schema))))))

(defun generate-schema-from-class (class)
  "Generate a schema from CLASS, using reflection."
  (let ((attributes
          (loop for slot in (c2mop:class-slots class)
                if (null (c2mop:slot-definition-type slot))
                  do (warn "Cannot create a schema attribute for ~a because it has no type." (c2mop:slot-definition-name slot))
                else
                  collect (make-instance 'attribute
                                         :name (c2mop:slot-definition-name slot)
                                         :required (not (typep nil (c2mop:slot-definition-type slot)))
                                         ;; TODO: use accessors, writers, readers specificed in slots
                                         :writer (lambda (value obj) (setf (slot-value obj (c2mop:slot-definition-name slot)) value))
                                         :reader (lambda (obj) (slot-value obj (c2mop:slot-definition-name slot)))
                                         ;; TODO. FIXME.
                                         ;;:documentation (c2mop:slot-definition-documentation slot)
                                         :type (make-instance 'type-schema :type (c2mop:slot-definition-type slot))))))
    (make-instance 'object-schema
                   :name (class-name class)
                   :documentation (documentation class t)
                   :attributes attributes
                   :class (class-name class))))
