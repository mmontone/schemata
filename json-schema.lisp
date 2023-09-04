(defpackage :schemata.json-schema
  (:use :cl :schemata)
  (:export
   :json-schema
   :render-json-schema
   :schema-from-json-schema
   :decode-json-schema))

(in-package :schemata.json-schema)

;; JSON-schema (WIP)

(defun decode-json-schema (source)
  "Decode SOURCE with a JSON-Schema. Create a Schemata schema as result."
  (schema-from-json-schema
   (let ((cl-json:*identifier-name-to-key* 'identity)
         (cl-json:*json-identifier-name-to-lisp* 'identity))
     (json:decode-json-from-source source))))

(defun json-schema (schema)
  (with-output-to-string (s)
    (let ((json:*json-output* s))
      (render-json-schema schema))))

(defun render-json-schema (schema &optional attribute)
  (ecase (schema-type schema)
    (object (render-object-json-schema schema attribute))
    (list-of (render-array-json-schema schema attribute))
    (t (render-type-json-schema schema attribute))))

(defun render-object-json-schema (schema attribute)
  (json:with-object ()
    (json:encode-object-member "type" "object")
    (json:as-object-member ("properties")
      (json:with-object ()
        (loop for attribute in (object-attributes schema)
              do
                 (json:as-object-member ((attribute-name attribute))
                   (render-json-schema (attribute-type attribute) attribute)))))
    (json:encode-object-member "description" (schema-documentation schema))))


;; JSON schema parsing

(defun alist (x)
  (if (hash-table-p x)
      (alexandria:hash-table-alist x)
      x))

(defun parse-json-schema-ref (ref)
  (let ((schema-name (car (last (split-sequence:split-sequence #\/ ref)))))
    (list 'ref (intern (json::simplified-camel-case-to-lisp schema-name)))))

(defun schema-from-json-schema (json-schema)
  "Create a SCHEMA from JSON-SCHEMA.

IMPORTANT:
JSON-SCHEMA is an association list obtained from CL-JSON:DECODE-JSON-FROM-SOURCE, with CL-JSON:*IDENTIFIER-NAME-TO-KEY* and CL-JSON:*JSON-IDENTIFIER-NAME-TO-LISP* bound to NIL.

Example:

    (schema-from-json-schema
        (let ((cl-json:*identifier-name-to-key* 'identity)
              (cl-json:*json-identifier-name-to-lisp* 'identity))
            (json:decode-json-from-string json-schema-string)))
 "
  
  (if (access:access json-schema "$ref")
      (parse-json-schema-ref (access:access json-schema "$ref"))
      (case (alexandria:make-keyword (string-upcase (access:access json-schema "type")))
        (:object (parse-json-schema-object json-schema))
        (:array (parse-json-schema-array json-schema))
        (:integer (parse-json-schema-integer json-schema))
        (:number (parse-json-schema-number json-schema))
        (:string (parse-json-schema-string json-schema))
        (:boolean (parse-json-schema-boolean json-schema))
        (t (error "Invalid JSON schema type: ~A" (access:access json-schema "type"))))))

(defun parse-json-schema-object (json-schema)
  (let ((required-props (access:access json-schema :required)))
    (eval `(schema (object ,(access:access json-schema "title")
                           ,(loop for prop in (alist (access:access json-schema "properties"))
                                  collect (parse-json-schema-object-property prop (member (car prop) required-props :test 'equalp)))
                           (:documentation ,(access:access json-schema :description)))))))

(defun parse-json-schema-object-property (prop &optional (required-p t))
  `(,(intern (json:camel-case-to-lisp (car prop)))
    ,(schema-from-json-schema (cdr prop))
    :external-name ,(car prop)
    ,@(when (not required-p)
        (list :required nil))
    ,@(let ((default (access:access (cdr prop) "default")))
        (when default
          (list :default default)))
    ;; CUSTOM JSON SCHEMA PROPERTIES
    ;; These are not JSON schema properties, we parse some extra attributes
    ;; to fill-in REST-SERVER schema things not present in JSON schemas, like
    ;; accessors, readers, formatters, etc
    ;; Extension properties begin with an "x-" prefix
    ,@(when (access:access (cdr prop) "x-accessor")
        (list :accessor (read-from-string (access:access (cdr prop) "x-accessor"))))
    ,@(when (access:access (cdr prop) "x-reader")
        (list :reader (read-from-string (access:access (cdr prop) "x-reader"))))
    ,@(when (access:access (cdr prop) "x-writer")
        (list :writer (read-from-string (access:access (cdr prop) "x-writer"))))
    ,@(when (access:access (cdr prop) "x-parser")
        (list :parser (read-from-string (access:access (cdr prop) "x-parser"))))
    ,@(when (access:access (cdr prop) "x-formatter")
        (list :formatter (read-from-string (access:access (cdr prop) "x-formatter"))))
    ,@(when (access:access (cdr prop) "x-validator")
        (list :validator (read-from-string (access:access (cdr prop) "x-validator"))))
    ,@(when (access:access (cdr prop) "x-add-validator")
        (list :add-validator (read-from-string (access:access (cdr prop) "x-add-validator"))))

    :documentation ,(access:access (cdr prop) "description")))

(defun parse-json-schema-boolean (json-schema)
  (declare (ignore json-schema))
  'boolean)

(defun parse-json-schema-integer (json-schema)
  (declare (ignore json-schema))
  'integer)

(defun parse-json-schema-string (json-schema)
  (cond
    ((equalp (access:access json-schema "format")
             "date")
     'local-time:date)
    ((equalp (access:access json-schema "format")
             "date-time")
     'local-time:timestamp)
    (t 'string)))

(defun parse-json-schema-number (json-schema)
  (alexandria:make-keyword (string-upcase (access:access json-schema :format))))

(defun parse-json-schema-array (json-schema)
  `(list-of ,(schema-from-json-schema (access:accesses json-schema "items"))))
