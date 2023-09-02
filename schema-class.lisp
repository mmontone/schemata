(in-package :schemata)

(defclass schema-object ()
  ())

(defclass schema-class (standard-class)
  ((schema-name :initarg :schema-name
                :accessor schema-name
                :type (or null string symbol)
                :initform nil))
  (:documentation "Metaclass for schema objects"))

(defclass schema-slot-definition (closer-mop:standard-slot-definition attribute-properties)
  ((schema
    :initform nil
    :accessor slot-schema
    :initarg :schema)
   (schema-slot-p
    :initform t
    :type boolean
    :initarg :schema-slot-p
    :accessor schema-slot-p)
   (schema-name
    :initform nil
    :type (or null string symbol)
    :accessor schema-name
    :initarg :schema-name)))

;; Slots

(defclass schema-direct-slot-definition (schema-slot-definition closer-mop:standard-direct-slot-definition)
  ())

(defclass schema-effective-slot-definition (schema-slot-definition closer-mop:standard-effective-slot-definition)
  ())

(defmethod closer-mop:direct-slot-definition-class ((class schema-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'schema-direct-slot-definition))

(defmethod closer-mop:effective-slot-definition-class ((class schema-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'schema-effective-slot-definition))

(defmethod initialize-instance :after ((schema-slot schema-direct-slot-definition) &rest initargs)
  (declare (ignore initargs))
  (assert (or (not (schema-slot-p schema-slot))
              (c2mop:slot-definition-type schema-slot)
              (slot-schema schema-slot))
          nil
          "Provide the type or schema for slot ~A" schema-slot))

(defmethod closer-mop:compute-effective-slot-definition ((class schema-class)
                                                         slot-name direct-slots)
  (declare (ignore slot-name))
  (let ((effective-slot (call-next-method))
        (direct-slots (remove-if-not (lambda (slot)
                                       (typep slot 'schema-direct-slot-definition))
                                     direct-slots)))
    (unless (null (cdr direct-slots))
      (error "More than one :schema specifier"))
    (macrolet ((set-effective-slot (&rest accessors)
                 `(setf ,@(loop for accessor in accessors
                                collect `(,accessor effective-slot)
                                collect `(,accessor direct-slot)))))
      (let ((direct-slot (car direct-slots)))
        (set-effective-slot schema-slot-p schema-name
                            slot-schema attribute-required-p
                            attribute-required-message attribute-default
                            attribute-validator attribute-add-validator
                            attribute-parser attribute-formatter
                            attribute-external-name
                            attribute-serializer attribute-unserializer)
        effective-slot))))

;; Inheritance

(defmethod closer-mop:validate-superclass ((sub schema-class)
                                           (sup standard-class))
  (declare (ignore sub sup))
  t)

(defmethod initialize-instance :around ((class schema-class) &rest initargs
                                        &key direct-superclasses)
  (declare (dynamic-extent initargs))
  (if (loop for class in direct-superclasses
              thereis (subtypep class (find-class 'schema-object)))
      ;; 'schema-object is already one of the (indirect) superclasses
      (call-next-method)
      ;; 'schema-object is not one of the superclasses, so we have to add it
      (apply #'call-next-method class :direct-superclasses
             (append direct-superclasses
                     (list (find-class 'schema-object)))
             initargs))
  ;; Register the schema
  ;; Hack: this shouldn't be here, but the class needs to be finalized in order
  ;; to be able to call closer-mop:class-slots needed for the schema definition
  (closer-mop:finalize-inheritance class)

  (register-class-schema class))

(defmethod reinitialize-instance :around ((class schema-class)
                                          &rest initargs
                                          &key (direct-superclasses '() direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (if direct-superclasses-p
      ;; if direct superclasses are explicitly passed
      ;; this is exactly like above
      (progn
        (if (loop for class in direct-superclasses
                    thereis (subtypep class (find-class 'schema-object)))
            (call-next-method)
            (apply #'call-next-method class :direct-superclasses
                   (append direct-superclasses
                           (list (find-class 'schema-object)))
                   initargs))
        ;; Register a schema
        ;; Hack: this shouldn't be here, but the class needs to be finalized in order
        ;; to be able to call closer-mop:class-slots needed for the schema definition
        (closer-mop:finalize-inheritance class)

        (register-class-schema class))
      ;; if direct superclasses are not explicitly passed
      ;; we _must_ not change anything
      (call-next-method)))

(defun superclass-member-p (class superclasses)
  "Searches superclass list for class"
  (some (lambda (superclass)
          (or (eq class superclass)
              (let ((supers (closer-mop:class-direct-superclasses superclass)))
                (when supers
                  (superclass-member-p class supers)))))
        superclasses))

(defun ensure-class-inherits-from (class from-classnames direct-superclasses)
  (let* ((from-classes (mapcar #'find-class from-classnames))
         (has-persistent-objects
           (every (lambda (class) (superclass-member-p class direct-superclasses))
                  from-classes)))
    (if (not (or (member class from-classes) has-persistent-objects))
        (progn
          (dolist (class from-classes)
            (setf direct-superclasses (remove class direct-superclasses)))
          (append direct-superclasses from-classes))
        direct-superclasses)))

(defgeneric schema-slots (object)
  (declare (optimize speed))
  (:documentation
   "Return a list of slot-definitions part of the schema. The default
    is to call schema-slots-using-class with the object
    and the objects class")
  (:method ((object standard-object))
    (schema-slots-using-class object (class-of object)))
  #+(or sbcl cmu openmcl allegro)
  (:method ((object structure-object))
    (schema-slots-using-class object (class-of object)))
  (:method ((object condition))
    (schema-slots-using-class object (class-of object))))

;; unfortunately the metaclass of conditions in sbcl and cmu
;; are not standard-class

(defgeneric schema-slots-using-class (object class)
  (declare (optimize speed))
  (:documentation "Return a list of slot-definitions to serialize.
   The default calls compute slots with class")
  (:method ((object t) (class schema-class))
    (closer-mop:class-slots class)))

(defmacro define-schema-class (name direct-superclasses direct-slots &rest options)
  "Helper macro to define schema classes"
  `(defclass ,name ,direct-superclasses
     ,direct-slots
     (:metaclass schema-class)
     ,@options))

(defun schema-class-schema (schema-class)
  "Generate a schema using the schema class meta info"
  (let ((schema-name
          (or (schema-name schema-class)
              (class-name schema-class))))
    (find-schema schema-name)))

(defun register-class-schema (schema-class)
  (let ((schema-name
          (or (schema-name schema-class)
              (class-name schema-class))))
    (register-schema schema-name 
                     (eval
                      (list 'schema
                            (list 'object schema-name
                                  (loop for slot in (closer-mop:class-slots schema-class)
                                        when (and (typep slot 'schema-effective-slot-definition)
                                                  (schema-slot-p slot))
                                          collect
                                          (let ((slot-schema-name (or (schema-name slot)
                                                                      (closer-mop:slot-definition-name slot))))
                               
                                            (list slot-schema-name
                                                  (or (slot-schema slot)
                                                      (c2mop:slot-definition-type slot))
                                                  :slot (c2mop:slot-definition-name slot)
                                                  :required (attribute-required-p slot)
                                                  :required-message (attribute-required-message slot)
                                                  :default (or (attribute-default slot)
                                                               (c2mop:slot-definition-initform slot))
                                                  :validator (attribute-validator slot)
                                                  :add-validator (attribute-add-validator slot)
                                                  :parser (attribute-parser slot)
                                                  :formatter (attribute-formatter slot)
                                                  :external-name (attribute-external-name slot)
                                                  :serializer (attribute-serializer slot)
                                                  :unserializer (attribute-unserializer slot))))
                                  (list :class (class-name schema-class))))))))

(defmethod generic-serializer::serialize ((object schema-object)
                                          &optional
                                            (serializer generic-serializer::*serializer*)
                                            (stream generic-serializer::*serializer-output*)
                                          &rest args)
  (declare (ignore args))
  (%serialize-with-schema (schema-class-schema (class-of object))
                          serializer
                          object
                          stream))
