(in-package :schemata)

(defvar *collect-validation-errors* nil
  "When enabled, validation errors are handled and returned in a list.")
(defvar *signal-validation-errors* t)
(defvar *validation-errors-collection*)

(define-condition validation-error (simple-error)
  ())

(defmethod print-object ((validation-error validation-error) stream)
  (print-unreadable-object (validation-error stream :type t :identity t)
    (apply #'format stream
           (simple-condition-format-control validation-error)
           (simple-condition-format-arguments validation-error))))

(define-condition validation-error-collection (validation-error)
  ((validation-errors :initarg :validation-errors
                      :initform (error "Provide the validation errors")
                      :accessor validation-errors))
  (:report (lambda (c s)
             (format s "Validation errors: ~{~A~^, ~}"
                     (validation-errors c)))))

(defun validation-error (message &rest args)
  (cerror "Continue"
          'validation-error
          :format-control message
          :format-arguments args))

(defun simple-condition-message (condition)
  (apply #'format
         (simple-condition-format-control condition)
         (simple-condition-format-arguments condition)))

(defgeneric schema-validate (schema data)
  (:documentation "Validate DATA using SCHEMA."))

(defun validate-with-schema (schema data
                             &key
                               (collect-errors *collect-validation-errors*)
                               (error-p *signal-validation-errors*))
  "Validate input using schema.
Useful for validating resource operations posted content (for :post and :put methods).
Input can be a string or an association list.

Args:
  - schema (symbol or schema): The schema
  - data (alist): The data to validate.
  - format (keyword): The data format.
  - collect-errors (boolean): If true, collect all the validation errors. If false, return the first validation error found. Default: true.
  - error-p (boolean): If true, when validation errors are found, a validation error is signaled. If false, the validation errors are returned as the function result and no error is signaled."
  (let ((*collect-validation-errors* collect-errors)
        (*signal-validation-errors* error-p)
        (*validation-errors-collection* nil))
    (let ((validation-error
            (handler-bind ((validation-error
                             (lambda (validation-error)
                               (cond
                                 (collect-errors
                                  (push validation-error *validation-errors-collection*)
                                  (invoke-restart (find-restart 'continue)))
                                 ((not error-p)
                                  (return-from validate-with-schema validation-error))
                                 (t
                                  (error validation-error))))))
              (schema-validate schema data))))
      (if collect-errors
          *validation-errors-collection*
          validation-error))))

(defmethod schema-validate ((schema symbol) data)
  (schema-validate (find-schema schema) data))

(defmethod schema-validate ((schema type-schema) data)
  (schema-type-validate (schema-type schema) data))

(defgeneric schema-type-validate (cl-type data)
  (:method (cl-type data)
    (unless (typep data cl-type)
      (validation-error "~s is not of type: ~a" data cl-type))))

(defmethod schema-validate ((schema attribute) data)
  ;; If present, the attribute-validator replaces completely the default schema validation. To avoid replacing it, but adding more validation use :add-validator
  (if (attribute-validator schema)
      ;; The validator function receives the data to validate and an "schema validator" function
      ;; it can use to validate the schema
      (funcall (attribute-validator schema) data
               (lambda () (schema-validate (attribute-type schema) data)))
      ;; else
      (schema-validate (attribute-type schema)
                       data)))

(defmethod schema-validate :after ((schema attribute) data)
  ;; After normal validation, :add-validator is evaluated if found
  (when (attribute-add-validator schema)
    (multiple-value-bind (valid-p error-message) (funcall (attribute-add-validator schema) data)
      (when (not valid-p)
        (validation-error (or error-message
                              (format nil "~A: is invalid"
                                      (or (attribute-external-name schema)
                                          (attribute-name schema)))))))))

(defmethod schema-validate ((schema object-schema) data)
  "Validate data using schema object. "

  (unless (trivial-types:association-list-p data)
    (validation-error "Not an object data: ~s" data))

  ;; Check unknown attributes first
  (unless (or *ignore-unknown-object-attributes*
              (ignore-unknown-attributes schema))
    (alexandria:when-let ((unknown-attributes
                           (set-difference (mapcar 'car data)
                                           (mapcar 'attribute-name (object-attributes schema))
                                           :test 'equalp
                                           :key 'string)))
      (validation-error "Attributes not part of schema: ~a" unknown-attributes)))

  ;; Validate each attribute of object
  (loop
    :for schema-attribute :in (object-attributes schema)
    :for data-attribute := (assoc (string (attribute-name schema-attribute))
                                  data
                                  :test #'equalp
                                  :key #'string)
    :do
       (cond
         ((and (not data-attribute)
               (not (attribute-optional-p schema-attribute)))
          (let ((error-msg (or (attribute-required-message schema-attribute)
                               (format nil "Attribute required: ~a"
                                       (or (attribute-external-name schema-attribute)
                                           (attribute-name schema-attribute))))))
            (validation-error error-msg)))
         ((not data-attribute)
          ;; Nothing to validate
          )
         ((not (and (attribute-optional-p schema-attribute)
                    (null data-attribute)))
          (schema-validate schema-attribute
                           (cdr data-attribute))))))

(defmethod schema-validate ((schema or-schema) data)
  (labels ((validate-or (or-schema)
             (if (schemas-of or-schema)
                 (handler-case (schema-validate (first (schemas-of or-schema)) data)
                   (validation-error ()
                     (validate-or (make-instance 'or-schema :schemas (rest (schemas-of or-schema))))))
                 (validation-error "~s does not conform to: ~a" data (schema-spec schema)))))
    (validate-or schema)))

(defmethod schema-validate ((schema and-schema) data)
  (loop for subschema in (schemas-of schema)
        do (schema-validate subschema data)))

(defmethod schema-validate ((schema cons-schema) data)
  (unless (typep data 'cons)
    (validation-error "~s is not a CONS" data))
  (schema-validate (car-schema schema) (car data))
  (schema-validate (cdr-schema schema) (cdr data)))

(defmethod schema-validate ((schema const-schema) data)
  (unless (equalp data (schema-value schema))
    (validation-error "~s is not equal to ~s" data (schema-value schema))))

(defmethod schema-validate ((schema member-schema) data)
  (unless (member data (schema-members schema) :test #'equalp)
    (validation-error "~s is not member of ~a" data (schema-members schema))))

(defmethod schema-validate ((schema list-schema) data)
  (unless (listp data)
    (validation-error "~s is not a list" data))
  (unless (= (length (list-schemas schema))
             (length data))
    (validation-error "~s has invalid number of elements (~a expected)" data (length (list-schemas schema))))
  (loop for elem-schema in (list-schemas schema)
        for elem in data
        do (schema-validate elem-schema elem)))

(defmethod schema-validate ((schema list-of-schema) data)
  (unless (listp data)
    (validation-error "~s is not a list" data))
  (dolist (val data)
    (schema-validate (elements-schema schema) val)))

(defmethod schema-validate ((schema alist-of-schema) data)
  (unless (trivial-types:association-list-p data)
    (validation-error "~s is not an association list" data))
  (dolist (elem data)
    (unless (consp elem)
      (validation-error "~s is not a cons" elem))
    (schema-validate (key-schema schema) (car elem))
    (schema-validate (value-schema schema) (cdr elem))))

(defmethod schema-validate ((schema alist-schema) data)
  (unless (trivial-types:association-list-p data)
    (validation-error "~s is not an association list" data))
  (dolist (member (alist-members schema))
    (let ((assoc (assoc (car member) data)))
      (if (null assoc)
          (unless (or (eql (optional-keys schema) t)
                      (eql (required-keys schema) nil)
                      (member (car member) (optional-keys schema)))
            (validation-error "~s is required" (car member)))
          (schema-validate (cdr member) (cdr assoc)))))
  (unless (allow-other-keys-p schema)
    (let* ((allowed-keys (mapcar #'car (alist-members schema)))
           (data-keys (mapcar #'car data))
           (disallowed-keys (set-difference data-keys allowed-keys)))
      (when disallowed-keys
        (validation-error "Keys not allowed: ~s" disallowed-keys)))))

(defmethod schema-validate ((schema plist-of-schema) data)
  (unless (trivial-types:property-list-p data)
    (validation-error "~s is not a property list" data))
  (alexandria:doplist (key val data)
    (schema-validate (key-schema schema) key)
    (schema-validate (value-schema schema) val)))

(defun plist-keys (plist)
  (loop for key in plist by #'cddr
        collect key))

(defmethod schema-validate ((schema plist-schema) data)
  (unless (trivial-types:property-list-p data)
    (validation-error "~s is not a property list" data))
  (dolist (member (plist-members schema))
    (let* ((no-value (gensym))
           (val (getf data (car member) no-value)))
      (when (and (eq val no-value)
                 (not (member (car member) (optional-keys schema))))
        (validation-error "~s is required" (car member)))
      (unless (and (member (car member) (optional-keys schema))
                   (eq val no-value))
        (schema-validate (cdr member) val))))
  (unless (allow-other-keys-p schema)
    (let* ((allowed-keys (mapcar #'car (plist-members schema)))
           (data-keys (plist-keys data))
           (disallowed-keys (set-difference data-keys allowed-keys)))
      (when disallowed-keys
        (validation-error "Keys not allowed: ~s" disallowed-keys)))))

(defmethod schema-validate ((schema hash-table-of-schema) data)
  (unless (hash-table-p data)
    (validation-error "~s is not a hash-table" data))
  (loop for key being the hash-keys in data
          using (hash-value val)
        do
           (schema-validate (key-schema schema) key)
           (schema-validate (value-schema schema) val)))

(defmethod schema-validate ((schema hash-table-schema) data)
  (unless (hash-table-p data)
    (validation-error "~s is not a hash-table" data))
  (dolist (member (hash-table-members schema))
    (multiple-value-bind (val foundp)
        (gethash (car member) data)
      (when (and (not foundp)
                 (not (member (car member) (optional-keys schema))))
        (validation-error "~s is required" (car member)))
      (unless (and (member (car member) (optional-keys schema))
                   (not foundp))
        (schema-validate (cdr member) val))))
  (unless (allow-other-keys-p schema)
    (let* ((allowed-keys (mapcar #'car (hash-table-members schema)))
           (data-keys (alexandria:hash-table-keys data))
           (disallowed-keys (set-difference data-keys allowed-keys)))
      (when disallowed-keys
        (validation-error "Keys not allowed: ~s" disallowed-keys)))))


(defmethod schema-validate ((schema schema-reference-schema) data)
  (schema-validate (referenced-schema schema) data))

(defmethod schema-validate ((schema schema-class) data)
  (schema-validate (schema-class-schema schema) data))

(defmethod schema-type-validate ((schema-type (eql 'local-time:timestamp)) data)
  (unless
      (or (typep data 'local-time:timestamp)
          (and (stringp data)
               (or (ignore-errors (local-time:parse-timestring data
                                                               :allow-missing-timezone-part t))
                   (chronicity:parse data))))
    (validation-error "~A is not a valid timestamp"
                      data)))
