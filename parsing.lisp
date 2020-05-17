(in-package :schemata)

(defgeneric parse-with-schema (schema string-or-data &optional format)
  (:documentation "Parses the string to an association list using the schema"))

(defmethod parse-with-schema ((schema symbol) string-or-data &optional (format :json))
  (parse-with-schema (find-schema schema) string-or-data format))

(defmethod parse-with-schema (schema data &optional (format :json))
  (%parse-with-schema (schema-type schema)
                      schema
                      data))

(defmethod %parse-with-schema ((schema-type (eql :object))
                               schema data)
  (if (null data)
      data
      (loop
        for schema-attribute in (object-attributes schema)
        for data-attribute = (assoc (string (attribute-name schema-attribute))
                                    data
                                    :test #'equalp
                                    :key #'string)
        appending
        (cond
          ;; Don't do validation here, only parsing
          #+nil((and (not data-attribute)
                     (not (attribute-optional-p schema-attribute)))
                (validation-error "Attribute ~a not found in ~a"
                                  (attribute-name schema-attribute)
                                  data))
          ((not data-attribute)
           ;; don't add the attribute to the data in this case
           ;; idea to think of: we could use the attribute default value if specified, instead
           nil)
          ((or (equalp (attribute-type schema-attribute) :boolean)
               (not (null (cdr data-attribute))))
           (list (cons (intern (string (attribute-name schema-attribute)) :keyword)
                       (parse-schema-attribute schema-attribute (cdr data-attribute)))))))))

(defun parse-schema-attribute (schema-attribute value)
  (let ((parsed-value (parse-schema-attribute-value (attribute-type schema-attribute) value)))
    (if (attribute-parser schema-attribute)
        (funcall (attribute-parser schema-attribute)
                 parsed-value)
        parsed-value)))

(defmethod %parse-with-schema ((schema-type (eql :list))
                               schema data)
  (let ((elem-schema (second schema)))
    (flet ((parse-elem (elem)
             (if (symbolp elem-schema)
                 (parse-schema-attribute-value elem-schema elem)
                 (parse-with-schema elem-schema elem))))
      (loop for elem in data
            collect (parse-elem elem)))))

(defmethod parse-schema-attribute-value ((type (eql :string)) data)
  (string data))

(defmethod parse-schema-attribute-value ((type (eql :boolean)) data)
  (cond
    ((or (eql data t)
         (eql data nil))
     data)
    ((stringp data)
     (cond
       ((member data (list "true" "t" "yes" "on") :test #'equalp)
        t)
       ((member data (list "false" "f" "no" "off") :test #'equalp)
        nil)
       (t (validation-error "~A is not a boolean" data))))
    (t (validation-error "~A is not a boolean" data))))

(defmethod parse-schema-attribute-value ((type (eql :integer)) data)
  (cond
    ((integerp data)
     data)
    ((stringp data)
     (parse-integer data))
    (t (validation-error "~A is not an integer" data))))

(defmethod parse-schema-attribute-value ((type (eql :float)) data)
  (cond
    ((floatp data)
     data)
    ((stringp data)
     (read-from-string data))
    (t (validation-error "~A is not a float" data))))

(defmethod parse-schema-attribute-value ((type (eql :timestamp)) data)
  (or (ignore-errors (local-time:parse-timestring data :allow-missing-timezone-part t))
      (chronicity:parse data)))

(defmethod parse-schema-attribute-value ((type (eql :datetime)) data)
  (or (ignore-errors (local-time:parse-timestring data :allow-missing-timezone-part t))
      (chronicity:parse data)))

(defmethod parse-schema-attribute-value ((type (eql :time)) data)
  (or
   (ignore-errors (local-time:parse-timestring
                   data
                   :allow-missing-date-part t
                   :allow-missing-timezone-part t))
   (chronicity:parse data)))

(defmethod parse-schema-attribute-value ((type (eql :date)) data)
  (or
   (ignore-errors (local-time:parse-timestring
                   data
                   :allow-missing-time-part t
                   :allow-missing-timezone-part t))
   (chronicity:parse data)))

(defmethod parse-schema-attribute-value ((type (eql :keyword)) data)
  (intern (string-upcase data) :keyword))

(defmethod parse-schema-attribute-value ((type symbol) data)
  (let ((schema (find-schema type)))
    (parse-with-schema schema data)))

(defmethod parse-schema-attribute-value ((type cons) data)
  (parse-with-schema type data))

(defun parse-xml-with-schema (schema-or-name input)
  (let ((schema (if (symbolp schema-or-name)
                    (find-schema schema-or-name)
                    schema-or-name)))

    (ecase (schema-type schema)
      (:list
       (let ((items (third input)))
         (loop for item in items
               collect
               (parse-xml-with-schema
                (second schema) ;; the list type
                item))))
      (:object
       (assert (equalp (alexandria:make-keyword (object-name schema))
                       (alexandria:make-keyword (first input))) nil
                       "~A is not a ~A" input (object-name schema))
       (loop for attribute in (object-attributes schema)
             appending (let ((input-attribute
                               (find (symbol-name (attribute-name attribute))
                                     (cddr input)
                                     :key #'first
                                     :test #'equalp)))
                         (if input-attribute
                             ;; The attrbute is present
                             (list (cons (alexandria:make-keyword (first input-attribute))
                                         (cond
                                           ((listp (attribute-type attribute))
                                            ;; It is a compound type (:list, :object, etc)
                                            (parse-xml-with-schema
                                             (second (attribute-type attribute)) ;; The compound object type
                                             (third input-attribute) ;; The attribute value
                                             ))
                                           ((keywordp (attribute-type attribute))
                                            ;; the attribute type is simple, parse the attribute value
                                            (unserialize-schema-attribute-value
                                             (attribute-type attribute)
                                             (third input-attribute)))
                                           ((symbolp (attribute-type attribute))
                                            ;; assume a schema reference
                                            (let ((attribute-schema (find-schema (attribute-type attribute))))
                                              (parse-xml-with-schema
                                               attribute-schema
                                               (third input-attribute) ;; The attribute value
                                               )))))))))))))
