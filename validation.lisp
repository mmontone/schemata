(in-package :schemata)

(defvar *collect-validation-errors* nil
  "When enabled, validation errors are handled and returned in a list.")
(defvar *signal-validation-errors* t)
(defvar *validation-errors-collection*)

(define-condition validation-error (simple-error)
  ())

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

(defmethod schema-validate ((schema list-schema) data)
  (when (not (listp data))
    (validation-error "~A is not a list"
                      data))
  (dolist (val data)
    (schema-validate (elements-schema schema) val)))

(defmethod schema-validate ((schema schema-reference-schema) data)
  (schema-validate (referenced-schema schema) data))

(defmethod schema-type-validate ((schema-type (eql 'local-time:timestamp)) data)
  (unless
      (or (typep data 'local-time:timestamp)
          (and (stringp data)
               (or (ignore-errors (local-time:parse-timestring data
                                                               :allow-missing-timezone-part t))
                   (chronicity:parse data))))
    (validation-error "~A is not a valid timestamp"
                      data)))
