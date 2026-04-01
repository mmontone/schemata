(in-package :schemata)

(defvar *collect-validation-errors* nil
  "When enabled, validation errors are handled and returned in a list.")
(defvar *signal-validation-errors* t)
(defvar *validation-errors-collection*)

(define-condition validation-error (simple-error)
  ()
  (:documentation "A schema validation error"))

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

(define-condition composite-validation-error (validation-error)
  ((validation-errors :initarg :validation-errors
                      :initform nil))
  (:documentation "Validation error from OR and AND schemas with intermediate validation errors kept in VALIDATION-ERRORS slots."))

(defun simple-condition-message (condition)
  (apply #'format
         (simple-condition-format-control condition)
         (simple-condition-format-arguments condition)))

(defgeneric schema-validate (schema data &rest options)
  (:documentation "Validate DATA using SCHEMA."))

(defun validate-with-schema (schema data
                             &key
                               (collect-errors *collect-validation-errors*)
                               (error-p *signal-validation-errors*)
                               (ignore-unknown-attributes *ignore-unknown-attributes*))
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
              (schema-validate schema data
                               :ignore-unknown-attributes
                               ignore-unknown-attributes))))
      (if collect-errors
          *validation-errors-collection*
          validation-error))))

(defmethod schema-validate ((schema symbol) data &rest options)
  (apply #'schema-validate (find-schema schema) data options))

(defmethod schema-validate ((schema type-schema) data &rest options)
  (apply #'schema-type-validate (schema-type schema) data options))

(defgeneric schema-type-validate (cl-type data &rest options)
  (:method (cl-type data &rest options)
    (declare (ignore options))
    (unless (typep data cl-type)
      (validation-error "~s is not of type: ~a" data cl-type))))

(defmethod schema-validate ((schema attribute) data &rest options)
  ;; If present, the attribute-validator replaces completely the default schema validation. To avoid replacing it, but adding more validation use :add-validator
  (if (attribute-validator schema)
      ;; The validator function receives the data to validate and an "schema validator" function
      ;; it can use to validate the schema
      (funcall (attribute-validator schema) data
               (lambda () (apply #'schema-validate (attribute-type schema) data options)))
      ;; else
      (apply #'schema-validate (attribute-type schema) data options)))

(defmethod schema-validate :after ((schema attribute) data &rest options)
  (declare (ignore options))
  ;; After normal validation, :add-validator is evaluated if found
  (when (attribute-add-validator schema)
    (multiple-value-bind (valid-p error-message) (funcall (attribute-add-validator schema) data)
      (when (not valid-p)
        (validation-error (or error-message
                              (format nil "~A: is invalid"
                                      (or (attribute-external-name schema)
                                          (attribute-name schema)))))))))

(defun object-data-keys (data)
  (etypecase data
    (hash-table (alexandria:hash-table-keys data))
    (list (mapcar #'car data))))

(defmethod schema-validate ((schema object-schema) data &rest options)
  "Validate data using schema object. "

  (unless (or (trivial-types:association-list-p data)
              (hash-table-p data))
    (validation-error "Not an object data: ~s" data))

  ;; Check unknown attributes first
  (unless (or (getf options :ignore-unknown-attributes)
              (ignore-unknown-attributes schema))
    (alexandria:when-let ((unknown-attributes
                           (set-difference (object-data-keys data)
                                           (mapcar 'attribute-name (object-attributes schema))
                                           :test 'equalp
                                           :key 'string)))
      (validation-error "Attributes not part of schema: ~a" unknown-attributes)))

  ;; Validate each attribute of object
  (loop
    :for attribute :in (object-attributes schema)
    :for attribute-name := (or (attribute-external-name attribute)
                               (attribute-name attribute))
    :do
       (multiple-value-bind (attribute-value accessed-p)
           (access:access data attribute-name)
         (cond
           ((and (not accessed-p)
                 (not (attribute-optional-p attribute)))
            (let ((error-msg (or (attribute-required-message attribute)
                                 (format nil "Attribute required: ~a" attribute-name))))
              (validation-error error-msg)))
           (accessed-p
            (schema-validate attribute attribute-value))))))

(defmethod schema-validate ((schema or-schema) data &rest options)
  ;; if there's a discriminator, use it
  (if (discriminator-of schema)
      (let* ((subschema-index (the integer (funcall (discriminator-of schema) data)))
             (subschema (nth subschema-index (schemas-of schema))))
        (apply #'schema-validate subschema data options))
      ;; otherwise, try with the subschemas
      (let ((validation-errors (list)))
        (labels ((validate-or (or-schema)
                   (if (schemas-of or-schema)
                       (handler-case
                           (apply #'schema-validate (first (schemas-of or-schema)) data options)
                         (validation-error (validation-error)
                           (push validation-error validation-errors)
                           (validate-or (make-instance 'or-schema :schemas (rest (schemas-of or-schema))))))
                       (cerror "Continue"
                               'composite-validation-error
                               :format-control "~s does not conform to: ~a"
                               :format-arguments (list data (schema-spec schema))
                               :validation-errors validation-errors))))
          (validate-or schema)))))

(defmethod schema-validate ((schema and-schema) data &rest options)
  (loop for subschema in (schemas-of schema)
        do (apply #'schema-validate subschema data options)))

(defmethod schema-validate ((schema cons-schema) data &rest options)
  (unless (typep data 'cons)
    (validation-error "~s is not a CONS" data))
  (apply #'schema-validate (car-schema schema) (car data) options)
  (apply #'schema-validate (cdr-schema schema) (cdr data) options))

(defmethod schema-validate ((schema const-schema) data &rest options)
  (declare (ignore options))
  (unless (equalp data (schema-value schema))
    (validation-error "~s is not equal to ~s" data (schema-value schema))))

(defmethod schema-validate ((schema member-schema) data &rest options)
  (declare (ignore options))
  (unless (member data (schema-members schema) :test #'equalp)
    (validation-error "~s is not member of ~a" data (schema-members schema))))

(defmethod schema-validate ((schema list-schema) data &rest options)
  (unless (listp data)
    (validation-error "~s is not a list" data))
  (unless (= (length (list-schemas schema))
             (length data))
    (validation-error "~s has invalid number of elements (~a expected)" data (length (list-schemas schema))))
  (loop for elem-schema in (list-schemas schema)
        for elem in data
        do (apply #'schema-validate elem-schema elem options)))

(defmethod schema-validate ((schema list-of-schema) data &rest options)
  (unless (listp data)
    (validation-error "~s is not a list" data))
  (dolist (val data)
    (apply #'schema-validate (elements-schema schema) val options)))

(defmethod schema-validate ((schema alist-of-schema) data &rest options)
  (unless (trivial-types:association-list-p data)
    (validation-error "~s is not an association list" data))
  (dolist (elem data)
    (unless (consp elem)
      (validation-error "~s is not a cons" elem))
    (apply #'schema-validate (key-schema schema) (car elem) options)
    (apply #'schema-validate (value-schema schema) (cdr elem) options)))

(defmethod schema-validate ((schema alist-schema) data &rest options)
  (unless (trivial-types:association-list-p data)
    (validation-error "~s is not an association list" data))
  (dolist (member (alist-members schema))
    (let ((assoc (assoc (car member) data)))
      (if (null assoc)
          (unless (or (eql (optional-keys schema) t)
                      (eql (required-keys schema) nil)
                      (member (car member) (optional-keys schema)))
            (validation-error "~s is required" (car member)))
          (apply #'schema-validate (cdr member) (cdr assoc) options))))
  (unless (allow-other-keys-p schema)
    (let* ((allowed-keys (mapcar #'car (alist-members schema)))
           (data-keys (mapcar #'car data))
           (disallowed-keys (set-difference data-keys allowed-keys)))
      (when disallowed-keys
        (validation-error "Keys not allowed: ~s" disallowed-keys)))))

(defmethod schema-validate ((schema plist-of-schema) data &rest options)
  (unless (trivial-types:property-list-p data)
    (validation-error "~s is not a property list" data))
  (alexandria:doplist (key val data)
    (apply #'schema-validate (key-schema schema) key options)
    (apply #'schema-validate (value-schema schema) val options)))

(defun plist-keys (plist)
  (loop for key in plist by #'cddr
        collect key))

(defmethod schema-validate ((schema plist-schema) data &rest options)
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
        (apply #'schema-validate (cdr member) val options))))
  (unless (allow-other-keys-p schema)
    (let* ((allowed-keys (mapcar #'car (plist-members schema)))
           (data-keys (plist-keys data))
           (disallowed-keys (set-difference data-keys allowed-keys)))
      (when disallowed-keys
        (validation-error "Keys not allowed: ~s" disallowed-keys)))))

(defmethod schema-validate ((schema vector-of-schema) data &rest options)
  (unless (vectorp data)
    (validation-error "~s is not a vector" data))
  (loop for val across data
        do (apply #'schema-validate (elements-schema schema) val options)))

(defmethod schema-validate ((schema hash-table-of-schema) data &rest options)
  (unless (hash-table-p data)
    (validation-error "~s is not a hash-table" data))
  (loop for key being the hash-keys in data
          using (hash-value val)
        do
           (apply #'schema-validate (key-schema schema) key options)
           (apply #'schema-validate (value-schema schema) val options)))

(defmethod schema-validate ((schema hash-table-schema) data &rest options)
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
        (apply #'schema-validate (cdr member) val options))))
  (unless (allow-other-keys-p schema)
    (let* ((allowed-keys (mapcar #'car (hash-table-members schema)))
           (data-keys (alexandria:hash-table-keys data))
           (disallowed-keys (set-difference data-keys allowed-keys)))
      (when disallowed-keys
        (validation-error "Keys not allowed: ~s" disallowed-keys)))))


(defmethod schema-validate ((schema schema-reference-schema) data &rest options)
  (apply #'schema-validate (referenced-schema schema) data options))

(defmethod schema-validate ((schema schema-class) data &rest options)
  (apply #'schema-validate (schema-class-schema schema) data options))

(defmethod schema-type-validate ((schema-type (eql 'local-time:timestamp)) data &rest options)
  (declare (ignore options))
  (unless
      (or (typep data 'local-time:timestamp)
          (and (stringp data)
               (or (ignore-errors (local-time:parse-timestring data
                                                               :allow-missing-timezone-part t))
                   (chronicity:parse data))))
    (validation-error "~A is not a valid timestamp"
                      data)))
