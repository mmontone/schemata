(in-package :schemata.tests)

(schemata:def-schema-class message-created ()
  ((user-locale :type string
                :external-name "user_locale"
                :reader message-created-user-locale)
   (timestamp :type integer
              :reader message-created-timestamp)))

(schemata:defschema update-message
    (or message-created
        ;; message-deleted
        ;; ...
        ))

(schemata:defschema updates-array
    (schemata:list-of update-message))


(schemata:def-schema-class get-updates-response ()
  ((marker :type integer
           :reader get-updates-response-marker)
   (updates
    :initform #()
    :schema updates-array)))

(require :yason)

(defparameter *data*
  (yason:parse #p"class-test.json"))

(let ((schemata:*ignore-unknown-attributes* t))
  (schemata:unserialize-with-schema
   (schemata:find-schema 'get-updates-response)
   *data* :json))

(let ((schemata:*ignore-unknown-attributes* t))
  (schemata:validate-with-schema (schemata:find-schema 'get-updates-response)
                                 *data*))

(schemata:validate-with-schema
 (schemata:find-schema 'get-updates-response)
 *data* :ignore-unknown-attributes t)
