(in-package :schemata)

;; Unserialization

(defgeneric unserialize-with-schema (schema data format))

(defmethod unserialize-with-schema ((schema or-schema) input format)
  ;; if there's a discriminator, use it
  (if (discriminator-of schema)
      (let* ((subschema-index (the integer (funcall (discriminator-of schema) input)))
             (subschema (nth subschema-index (schemas-of schema))))
        (unserialize-with-schema subschema input format))
      ;; else
      (progn
        (when (null (schemas-of schema))
          (error "Cannot unserialize: ~s with: ~s" input schema))
        (let ((try-schema (first (schemas-of schema)))
              (rest-schema (make-instance 'or-schema :schemas (rest (schemas-of schema)))))
          (handler-case
              (unserialize-with-schema try-schema input format)
            (error ()
              (unserialize-with-schema rest-schema input format)))))))

(defmethod unserialize-with-schema ((schema object-schema) input format)
  "Unserializes an schema object

Args: - object (list) : An schema object
      - input (or assoc-list hash-table) : An association list or a hash-table.
                                           Probably obtained from parse-api-input.

See: parse-api-input (function)"

  (let ((unserializer (object-unserializer schema))
        (object-class (object-class schema)))
    (cond
      (unserializer (funcall unserializer input))
      (object-class (unserialize-schema-object-to-class schema input object-class format))
      (t input))))

(defun unserialize-schema-object-to-class (object input class format)
  (let ((instance (allocate-instance (find-class class))))
    (loop for attribute in (object-attributes object)
          do
             (multiple-value-bind (attribute-input accessed-p)
                 (access:access input (attribute-name attribute))
               (cond
                 ((and (not accessed-p)
                       (not (attribute-optional-p attribute)))
                  (validation-error "~A not provided" (attribute-name attribute)))
                 (accessed-p
                  (let ((attribute-value (unserialize-schema-attribute attribute attribute-input format)))
                    (setf (slot-value instance (or (attribute-slot attribute)
                                                   (attribute-name attribute)))
                          attribute-value))))))
    (initialize-instance instance)
    instance))

(defun unserialize-schema-attribute (attribute input format)
  (let ((unserializer (attribute-unserializer attribute)))
    (if unserializer
        (funcall unserializer input format)
        (unserialize-with-schema (attribute-type attribute) input format))))

(defmethod unserialize-with-schema ((schema type-schema) data format)
  (unserialize-with-type (schema-type schema) data format))

(defmethod unserialize-with-schema ((schema schema-reference-schema) data format)
  (unserialize-with-schema (referenced-schema schema) data format))

(defmethod unserialize-with-schema ((schema list-of-schema) data format)
  (loop for elem in (the list data)
        collect (unserialize-with-schema (elements-schema schema) elem format)))

(defmethod unserialize-with-schema ((schema vector-of-schema) data format)
  (apply #'vector
         (loop for elem across (the vector data)
               collect (unserialize-with-schema (elements-schema schema) elem format))))

(defgeneric unserialize-with-type (type input format)
  (:method (type input format)
    (coerce input type))
  (:method ((type (eql 'cl:integer)) input format)
    (if (integerp input)
        input
        (parse-integer input)))
  (:method ((type (eql 'boolean)) input format)
    (if (stringp input)
        (let ((true-strings (list "true" "t" "yes" "on"))
              (false-strings (list "false" "f" "no" "off")))
          (assert (member input (append true-strings false-strings) :test #'equalp)
                  nil "Invalid boolean ~A" input)
          (member input true-strings :test #'equalp))
        (not (null input))))
  (:method ((type (eql 'cl:keyword)) input format)
    (if (stringp input)
        (intern (string-upcase input) :keyword)
        (the keyword input))))
