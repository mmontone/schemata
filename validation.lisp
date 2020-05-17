(in-package :schemata)

(defvar *collect-validation-errors* nil)
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

(defun validate-with-schema (schema data
                             &key
                               (format :json)
                               (collect-errors *collect-validation-errors*)
                               (error-p *signal-validation-errors*))
  "Validate input using schema.
Useful for validating resource operations posted content (for :post and :put methods).
Input can be a string or an association list.

Args:
  - schema (symbol or schema): The schema
  - data (alist): The data to validate.
  - format (keyword): The data format
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

(defgeneric schema-validate (schema data &optional attribute)
  )

(defmethod schema-validate (schema data &optional attribute)
  ;; If present, the attribute-validator replaces completely the default schema validation. To avoid replacing it, but adding more validation use :add-validator
  (flet ((schema-validator ()
           (let ((schema (or (and (symbolp schema) (not (keywordp schema))
                                  (find-schema schema))
                             schema)))
             (%schema-validate (schema-type schema) schema data attribute))))
    (if (and attribute (attribute-validator attribute))

        ;; The validator function receives the data to validate and an "schema validator" function
        ;; it can use to validate the schema
        (funcall (attribute-validator attribute) data #'schema-validator)
        ;; else
        (schema-validator))))

(defmethod schema-validate :after (schema data &optional attribute)
  ;; After normal validation, :add-validator is evaluated if found
  (when (and attribute (attribute-add-validator attribute))
    (multiple-value-bind (valid-p error-message) (funcall (attribute-add-validator attribute) data)
      (when (not valid-p)
        (validation-error (or error-message
                              (format nil "~A: is invalid"
                                      (or (attribute-external-name attribute)
                                          (attribute-name attribute)))))))))

(defmethod %schema-validate ((schema-type (eql :object)) schema data &optional attribute)
  (declare (ignore attribute))
  "Validate data using schema object. "

  ;; Check unknown attributes first
  (unless (or *ignore-unknown-object-attributes*
              (object-option :ignore-unknown-attributes schema))
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
        (let ((error-msg (or (attribute-option :attribute-required-message schema-attribute)
                             (format nil "Attribute required: ~a"
                                     (or (attribute-external-name schema-attribute)
                                         (attribute-name schema-attribute))))))
          (validation-error error-msg)))
       ((not data-attribute)
        ;; Nothing to validate
        )
       ((not (and (attribute-optional-p schema-attribute)
                     (null data-attribute)))
       (schema-validate (attribute-type schema-attribute)
                        (cdr data-attribute)
                        schema-attribute)))))

(defmethod %schema-validate ((schema-type (eql :list)) schema data &optional attribute)
  (when (not (listp data))
    (validation-error "~A: ~A is not of type ~A"
                      (or (attribute-external-name attribute)
                          (attribute-name attribute))
                      attribute
                      (attribute-type attribute)))
  (every (lambda (val)
           (schema-validate (second schema) val))
         data))

(defmethod %schema-validate ((schema-type (eql :option)) schema data &optional attribute)
  (when (not (member data (cdr schema) :test 'equalp))
    (validation-error "~s : should be one of ~s" data (cdr schema)))
  t)

(defmethod %schema-validate ((schema-type (eql :string)) schema data &optional attribute)
  (when (not (stringp data))
    (validation-error "~A: ~A is not a string"
                      (or (attribute-external-name attribute)
                          (attribute-name attribute))
                      data)))

(defmethod %schema-validate ((schema-type (eql :boolean)) schema data &optional attribute)
  (when (not (typep data 'boolean))
    (validation-error "~A: ~A is not a boolean"
                      (or (attribute-external-name attribute)
                          (attribute-name attribute))
                      data)))

(defmethod %schema-validate ((schema-type (eql :integer)) schema data &optional attribute)
  (when (not (integerp data))
    (validation-error "~A: ~A is not a number"
                      (or (attribute-external-name attribute)
                          (attribute-name attribute))
                      data)))

(defmethod %schema-validate ((schema-type (eql :float)) schema data &optional attribute)
  (when (not (floatp data))
    (validation-error "~A: ~A is not a float"
                      (or (attribute-external-name attribute)
                          (attribute-name attribute))
                      data)))

(defmethod %schema-validate ((schema-type (eql :timestamp)) schema data &optional attribute)
  (when (not
         (or (typep data 'local-time:timestamp)
             (and (stringp data)
                  (or (ignore-errors (local-time:parse-timestring data
                                                                  :allow-missing-timezone-part t))
                      (chronicity:parse data)))))
    (validation-error "~A: ~A is not a valid timestamp"
                      (or (attribute-external-name attribute)
                          (attribute-name attribute))
                      data)))

(defmethod %schema-validate ((schema-type (eql :datetime)) schema data &optional attribute)
  (when (not
         (or (typep data 'local-time:timestamp)
             (and (stringp data)
                  (or (ignore-errors (local-time:parse-timestring data
                                                                  :allow-missing-timezone-part t))
                      (chronicity:parse data)))))
    (validation-error "~A: ~A is not a valid timestamp"
                      (or (attribute-external-name attribute)
                          (attribute-name attribute))
                      data)))

(defmethod %schema-validate ((schema-type (eql :date)) schema data &optional attribute)
  (when (not
         (or (typep data 'local-time:timestamp)
             (and (stringp data)
                  (or (ignore-errors (local-time:parse-timestring data
                                                                  :allow-missing-time-part t
                                                                  :allow-missing-timezone-part t))
                      (chronicity:parse data)))))
    (validation-error "~A: ~A is not a valid date"
                      (or (attribute-external-name attribute)
                          (attribute-name attribute))
                      data)))

(defmethod %schema-validate ((schema-type (eql :keyword)) schema data &optional attribute)
  (when (not (stringp data))
    (validation-error "~A: ~A is not a keyword"
                      (or (attribute-external-name attribute)
                          (attribute-name attribute))
                      data)))
