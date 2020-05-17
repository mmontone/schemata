(in-package :schemata)

(defun object-unserializer (object)
  "Returns the unserializer of the object if any"
  (object-option :unserializer object))


;; Schemas may be used either to serialize objects or validate input

(defun serialize-with-schema (schema input
                              &optional (serializer generic-serializer::*serializer*)
                                (stream generic-serializer::*serializer-output*))
  (%serialize-with-schema schema serializer input stream))

(defmethod %serialize-with-schema (schema serializer input stream)
  (%%serialize-with-schema (schema-type schema) schema serializer input stream))

(defmethod %%serialize-with-schema (schema-type schema serializer input stream)
  (serialize-value serializer input stream))

(defmethod %%serialize-with-schema ((schema-type (eql :list))
                                    schema serializer input stream)
  (serialize-schema-list schema serializer input stream))

(defmethod %%serialize-with-schema ((schema-type (eql :option))
                                    schema serializer input stream)
  (serialize-value serializer input stream))

(defmethod %%serialize-with-schema ((schema-type (eql :object))
                                    schema serializer input stream)
  (serialize-schema-object schema serializer input stream))

(defmethod %%serialize-with-schema ((schema-type (eql :integer))
                                    schema serializer input stream)
  (serialize-value serializer input stream))

(defmethod %%serialize-with-schema ((schema-type (eql :boolean))
                                    schema serializer input stream)
  (serialize-value serializer input stream))

(defmethod %%serialize-with-schema ((schema-type (eql :string))
                                    schema serializer input stream)
  (serialize-value serializer input stream))

(defmethod %%serialize-with-schema ((schema-type (eql :timestamp))
                                    schema serializer input stream)
  (serialize-value serializer input stream))

(defmethod %%serialize-with-schema ((schema-type (eql :keyword))
                                    schema serializer input stream)
  (serialize-value serializer (string-downcase (string input)) stream))

(defun serialize-schema-object (schema-object serializer input stream)
  (destructuring-bind (_ object-name attributes &rest options) schema-object
    (declare (ignore _ options))
    (with-object ((or (and (stringp object-name)
                           object-name)
                      (symbol-name object-name))
                  :serializer serializer
                  :stream stream)
      (loop for attribute in attributes
         do
           (serialize-schema-attribute attribute serializer input stream)))))

(defun serialize-schema-attribute (schema-attribute serializer input stream)
  (destructuring-bind
        (attribute-name attribute-type &rest options)
      schema-attribute
    (let* ((reader (symbol-function (or (getf options :reader)
                                        (getf options :accessor)
                                        attribute-name)))
           (attribute-value (funcall reader input)))
      (when (and
             ;; Booleans are always serialized. We don't have a way of
             ;; distinguishing between "false" and "unset" boolean attributes (both are nil)
             (not (eql attribute-type :boolean))
             (not (and (getf options :optional) (null-value attribute-value))))
        (with-attribute (attribute-name :serializer serializer
                                        :stream stream)
          (cond
            ((getf options :serializer)
             (funcall (getf options :serializer) attribute-value))
            ((keywordp attribute-type)
             (serialize-attribute-value attribute-type attribute-value stream serializer))
            ((symbolp attribute-type)
             ;; It is a schema reference or a serializable class reference
             (let ((attribute-schema (find-schema attribute-type nil)))
               (if attribute-schema
                   (%serialize-with-schema attribute-schema serializer attribute-value stream)
                                        ; else, try with a serializable class reference
                   (let ((serializable-class (find-class attribute-type nil)))
                     (if (and serializable-class
                              (typep serializable-class 'serializable-class))
                         (%serialize-with-schema (serializable-class-schema serializable-class)
                                                 serializer attribute-value stream)
                                        ; else
                         (error "Could not resolve reference ~A when serializing" attribute-type))))))
            ((listp attribute-type)
             (%serialize-with-schema attribute-type
                                     serializer
                                     attribute-value
                                     stream))))))))

(defmethod serialize-attribute-value (attribute-type attribute-value stream &optional (serializer generic-serializer::*serializer*))
  (serialize attribute-value serializer stream))

(defmethod serialize-attribute-value ((attribute-type (eql :timestamp)) attribute-value stream &optional (serializer generic-serializer::*serializer*))
  (declare (ignore serializer))
  (if (integerp attribute-value)
      ;; Assume a universal time number
      (write (net.telent.date:universal-time-to-rfc-date attribute-value) :stream stream)
      ;; else, serialize whatever it is
      (call-next-method)))

(defmethod serialize ((thing local-time:timestamp)
                      &optional (serializer generic-serializer::*serializer*)
                        (stream generic-serializer::*serializer-output*) &rest args)
  (declare (ignore serializer args))
  (local-time:format-rfc1123-timestring stream thing))

(defun serialize-schema-list (schema-list serializer input stream)
  (destructuring-bind (_ list-type) schema-list
    (declare (ignore _))
    (with-list ("LIST" :serializer serializer
                       :stream stream)
      (cond
        ((keywordp list-type)
         ;; It is a primitive type like :string, :boolean, etc
         (loop for elem in (coerce input 'list)
            do
              (add-list-member "ITEM" elem
                               :serializer serializer
                               :stream stream)))
        ((symbolp list-type)
         ;; It is a reference to a schema like 'user-schema'
         (let ((schema (find-schema list-type)))
           (loop for elem in (coerce input 'list)
              do
                (with-list-member ("ITEM" :serializer serializer
                                          :stream stream)
                  (%serialize-with-schema schema serializer elem stream)))))
        ((listp list-type)
         ;; It is some compound type, like :object, :list, or :option
         (let ((schema list-type))
           (loop for elem in (coerce input 'list)
              do
                (with-list-member ("ITEM" :serializer serializer
                                          :stream stream)
                  (%serialize-with-schema schema serializer elem stream)))))))))

;; Unserialization

;; (object-unserializer '(:object user () (:unserializer unserialize-user)))

(defun unserialize-with-schema (schema data &optional (format :json))
  (unserialize-schema-object schema data))

(defun unserialize-schema-object (object input)
  "Unserializes an schema object

Args: - object (list) : An schema object
      - input (assoc-list) : An association list with values.
                             Probably obtained from parse-api-input.

See: parse-api-input (function)"

  (let ((unserializer (object-unserializer object))
        (object-class (object-class object)))
    (cond
      (unserializer (funcall unserializer input))
      (object-class (unserialize-schema-object-to-class object input object-class))
      (t input))))

(defun unserialize-schema-object-to-class (object input class)
  (let ((instance (allocate-instance (find-class class))))
    (loop for attribute in (object-attributes object)
       do (let ((attribute-input (assoc (string (attribute-name attribute))
                                        input
                                        :test #'equalp
                                        :key #'string)))
            (when (and (not attribute-input)
                       (not (attribute-optional-p attribute)))
              (validation-error "~A not provided" (attribute-name attribute)))
            (let ((attribute-value (unserialize-schema-attribute attribute (cdr attribute-input))))
              (setf (slot-value instance (or (attribute-option :slot attribute)
                                             (attribute-name attribute)))
                    attribute-value))))
    (initialize-instance instance)
    instance))

(defun unserialize-schema-attribute (attribute input)
  (let ((unserializer (attribute-option :unserializer attribute)))
    (if unserializer
        (funcall unserializer)
        (if (null input)
            (when (not (attribute-optional-p attribute))
              (validation-error
               "Attribute ~A is not optional but value was not provided"
               (attribute-name attribute)))
                                        ; else
            (unserialize-schema-attribute-value (attribute-type attribute) input)))))

(defun unserialize-schema-attribute-value (type input)
  (%unserialize-schema-attribute-value
   (if (listp type)
       (first type)
       type)
   type
   input))

(defgeneric %unserialize-schema-attribute-value (type-name type input)
  (:method ((type-name (eql :integer)) attribute input)
    (if (integerp input)
        input
        (parse-integer input)))
  (:method ((type-name (eql :string)) type input)
    input)
  (:method ((type-name (eql :boolean)) type input)
    (if (stringp input)
        (let ((true-strings (list "true" "t" "yes" "on"))
              (false-strings (list "false" "f" "no" "off")))
          (assert (member input (append true-strings false-strings) :test #'equalp)
                  nil "Invalid boolean ~A" input)
          (member input true-strings :test #'equalp))
        (not (null input))))
  (:method ((type-name (eql :object)) type input)
    (unserialize-schema-object type input))
  (:method ((type-name (eql :list)) type input)
    (let ((list-type (second type)))
      (loop for elem in input
         collect (unserialize-schema-attribute-value list-type elem))))
  (:method ((type-name (eql :option)) type input)
    input)
  (:method ((type-name symbol) type input)
    ;; Assume a schema reference
    (let ((schema (find-schema type-name nil)))
      (if (not schema)
          (error "Invalid type ~A" type-name)
                                        ; else
          (unserialize-schema-attribute-value schema input)))))
