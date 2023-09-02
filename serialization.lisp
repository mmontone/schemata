(in-package :schemata)

;; Schemas may be used either to serialize objects or validate input

(defgeneric %serialize-with-schema (schema serializer input stream))

(defun serialize-with-schema (schema input
                              &optional (serializer generic-serializer::*serializer*)
                                (stream generic-serializer::*serializer-output*))
  (%serialize-with-schema schema serializer input stream))

(defmethod %serialize-with-schema ((schema type-schema) serializer input stream)
  (generic-serializer:serialize-value serializer input stream))

(defmethod %serialize-with-schema ((schema object-schema) serializer input stream)
  (let ((object-name (object-name schema)))
    (generic-serializer:with-object ((or (and (stringp object-name)
                                              object-name)
                                         (symbol-name object-name))
                                     :serializer serializer
                                     :stream stream)
      (loop for attribute in (object-attributes schema)
            do
               (serialize-schema-attribute attribute serializer input stream)))))

(defun serialize-schema-attribute (attribute serializer input stream)
  (let* ((reader (or (attribute-reader attribute)
                     (and (attribute-slot attribute)
                          (lambda (obj) (slot-value obj (attribute-slot attribute))))
                     (symbol-function (attribute-name attribute))))
         (attribute-value (funcall reader input)))
    (when (and
           ;; Booleans are always serialized. We don't have a way of
           ;; distinguishing between "false" and "unset" boolean attributes (both are nil)
           (not (eql (attribute-type attribute) 'boolean))
           (not (and (attribute-optional-p attribute) (null-value attribute-value))))
      (generic-serializer:with-attribute ((attribute-name attribute) :serializer serializer
                                                                     :stream stream)
        (cond
          ((attribute-serializer attribute)
           (generic-serializer:serialize (funcall (attribute-serializer attribute) attribute-value stream) serializer stream))
          ((attribute-formatter attribute)
           (generic-serializer:serialize (funcall (attribute-formatter attribute) attribute-value) serializer stream))
          (t
           (%serialize-with-schema (attribute-type attribute) serializer attribute-value stream)))))))

(defmethod %serialize-with-schema ((schema schema-reference-schema) serializer input stream)
  (%serialize-with-schema (find-schema (schema-name schema)) serializer input stream))

(defmethod %serialize-with-schema ((schema type-schema) serializer input stream)
  (let ((class (find-class (schema-type schema) nil)))
    (cond
      ((and class (typep class 'schema-class))
       (%serialize-with-schema (schema-class-schema class)
                               serializer input stream))
      (t
       ;; else, serialize the attribute value
       (generic-serializer:serialize input serializer stream)))))

(defmethod %serialize-with-schema ((schema list-schema) serializer input stream)
  (generic-serializer:with-list ("LIST" :serializer serializer
                                        :stream stream)
    (loop for elem in (coerce input 'list)
          do
             (%serialize-with-schema (elements-schema schema) serializer elem stream))))

(defmethod generic-serializer:serialize ((thing local-time:timestamp)
                                         &optional (serializer generic-serializer::*serializer*)
                                           (stream generic-serializer::*serializer-output*) &rest args)
  (declare (ignore serializer args))
  (local-time:format-rfc1123-timestring stream thing))
