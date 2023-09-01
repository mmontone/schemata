(in-package :schemata)

;; (intern (string (attribute-name schema-attribute)) :keyword)

(defgeneric parse-with-schema (schema string-or-data )
  (:documentation "Parses the string to an association list using the schema"))

(defmethod parse-with-schema ((schema object-schema) input )
  "Parses an schema object

Args: - object (list) : An schema object
      - input (assoc-list) : An association list with values.
                             Probably obtained from parse-api-input.

See: parse-api-input (function)"

  (loop for attribute in (object-attributes schema)
        collect
        (let* ((attribute-input (assoc (string (attribute-name attribute))
                                      input
                                      :test #'equalp
                                      :key #'string))
               (attribute-value (parse-schema-attribute attribute (cdr attribute-input))))
          (cons (attribute-name attribute) attribute-value))))

(defun parse-schema-attribute (attribute input )
  (let ((parser (attribute-parser attribute)))
    (if parser
        (funcall parser)
        (if (null input)
            (when (not (attribute-optional-p attribute))
              (validation-error
               "Attribute ~A is not optional but value was not provided"
               (attribute-name attribute)))
                                        ;; else
            (parse-with-schema (attribute-type attribute) input )))))

(defmethod parse-with-schema ((schema type-schema) data )
  (parse-with-type (schema-type schema) data ))

(defmethod parse-with-schema ((schema schema-reference-schema) data )
  (parse-with-schema (referenced-schema schema) data ))

(defmethod parse-with-schema ((schema list-schema) data )
  (loop for elem in (the list data)
        collect (parse-with-schema (elements-schema schema) elem )))

(defgeneric parse-with-type (type input )
  (:method (type input )
    (coerce input type))
  (:method ((type (eql 'cl:integer)) input )
    (if (integerp input)
        input
        (parse-integer input)))
  (:method ((type (eql 'boolean)) input )
    (if (stringp input)
        (let ((true-strings (list "true" "t" "yes" "on"))
              (false-strings (list "false" "f" "no" "off")))
          (assert (member input (append true-strings false-strings) :test #'equalp)
                  nil "Invalid boolean ~A" input)
          (member input true-strings :test #'equalp))
        (not (null input))))
  (:method ((type (eql 'cl:keyword)) input )
    (if (stringp input)
        (intern (string-upcase input) :keyword)
        (the keyword input))))
