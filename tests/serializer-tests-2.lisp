(uiop:define-package #:test-schemata
    (:use #:cl))

(in-package #:test-schemata)


(schemata:def-schema-class photo ()
  ((name :type string
         :initarg :name)
   (photo-format :type string
                 :initarg :photo-format)))


(schemata:def-schema-class video ()
  ((name :type string
         :initarg :name)
   (video-format :type string
                 :initarg :video-format)))


(schemata:def-schema-class message ()
  ((text :type string
         :initarg :text)
   (media :type (or photo
                    video)
          :initarg :media)))

(defun test-message-with-photo ()
  (schemata:unserialize-with-schema
   (schemata:find-schema 'message)
   '(("text" . "Hello")
     ("media" . (("name" . "photo")
                 ("photo-format" . "jpg"))))
   :json))


(defun test-message-with-video ()
  (schemata:unserialize-with-schema
   (schemata:find-schema 'message)
   '(("text" . "Hello")
     ("media" . (("name" . "movie")
                 ("video-format" . "mp4"))))
   :json))

(test-message-with-video)

(test-message-with-photo)

(schemata:unserialize-with-schema
 (schemata:schema (or integer string))
 "adf"
 :json)

(schemata:unserialize-with-schema
 (schemata:schema (or integer string))
 "adf"
 :json)

(schemata:unserialize-with-schema
 (schemata:schema (or integer string))
 22
 :json)

(schemata:unserialize-with-schema
 (schemata:schema (or integer string))
 #p"lala"
 :json)

(schemata::parse-schema '(or string integer :discriminator lal))

(defun string-integer-discriminator (data)
  (etypecase data
    (integer 0)
    (string 1)))

(schemata:unserialize-with-schema
 (schemata:schema (or integer string :discriminator string-integer-discriminator))
 "lala" :json)

(schemata:unserialize-with-schema
 (schemata:schema (or integer string :discriminator string-integer-discriminator))
 22 :json)

;; pathname is not string or integer. fails.
(schemata:unserialize-with-schema
 (schemata:schema (or integer string :discriminator string-integer-discriminator))
 #p"lala" :json)

(generic-serializer:with-serializer :json
  (schemata:serialize-with-schema
   (schemata:schema integer)
   22))

(generic-serializer:with-serializer :json
  (schemata:serialize-with-schema
   (schemata:schema (or integer string :discriminator string-integer-discriminator))
   22))

(generic-serializer:with-serializer :json
  (schemata:serialize-with-schema
   (schemata:schema (or integer string :discriminator string-integer-discriminator))
   "lal"))

(generic-serializer:with-serializer :json
  (schemata:serialize-with-schema
   (schemata:schema (or integer string :discriminator string-integer-discriminator))
   #p"lal"))

(generic-serializer:with-serializer :json
  (schemata:serialize-with-schema
   (schemata:schema (or integer string))
   "lal"))

(generic-serializer:with-serializer :json
  (schemata:serialize-with-schema
   (schemata:schema (or integer string))
   34))

(generic-serializer:with-serializer :json
  (schemata:serialize-with-schema
   (schemata:schema (or integer string))
   #p"lal"))

(schemata:schema (vector-of string))
