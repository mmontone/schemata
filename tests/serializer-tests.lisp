(in-package :schemata.tests)

(deftest basic-json-schema-serialization-test ()
  (let ((user (make-instance 'user
                             :realname "Mariano"
                             :id 2
                             :age 30
                             :groups (list (make-instance 'group
                                                          :name "My group"
                                                          :id 3))
                             :best-friend (make-instance 'user
                                                         :id 3
                                                         :realname "Fernando"
                                                         :age 31))))
    (let ((json
            (with-output-to-string (s)
              (generic-serializer:with-serializer-output s
                (generic-serializer:with-serializer :json
                  (serialize-with-schema
                   *schema* user))))))
      (finishes (json:decode-json-from-string json)))))

(with-output-to-string (s)
  (generic-serializer:with-serializer-output s
    (generic-serializer:with-serializer :json
      (serialize-with-schema
       (find-schema 'user-schema) *user*))))

(with-output-to-string (s)
  (generic-serializer:with-serializer-output s
    (generic-serializer:with-serializer :json
      (serialize-with-schema
       (find-schema 'minimal-user-schema) *user*))))

(with-output-to-string (s)
  (generic-serializer:with-serializer-output s
    (generic-serializer:with-serializer :xml
      (serialize-with-schema
       *schema* *user*))))

(with-output-to-string (s)
  (generic-serializer:with-serializer-output s
    (generic-serializer:with-serializer :xml
      (serialize-with-schema
       (find-schema 'user-schema) *user*))))

(with-output-to-string (s)
  (generic-serializer:with-serializer-output s
    (generic-serializer:with-serializer :xml
      (serialize-with-schema
       (find-schema 'minimal-user-schema) *user*))))

(with-output-to-string (s)
  (generic-serializer:with-serializer-output s
    (generic-serializer:with-serializer :json
      (serialize-with-schema
       (schema-class-schema
        (find-class 'schema-user))
       *schema-user*))))

(with-output-to-string (s)
  (generic-serializer:with-serializer-output s
    (generic-serializer:with-serializer :json
      (generic-serializer:serialize *schema-user*))))

(with-output-to-string (s)
  (generic-serializer:with-serializer-output s
    (generic-serializer:with-serializer :xml
      (serialize-with-schema
       (schema-class-schema
        (find-class 'schema-user))
       *schema-user*))))

(with-output-to-string (s)
  (generic-serializer:with-serializer-output s
    (generic-serializer:with-serializer :xml
      (generic-serializer:serialize *schema-user*))))

;; Unserialization

(let ((data
        (with-output-to-string (s)
          (generic-serializer:with-serializer-output s
            (generic-serializer:with-serializer :json
              (serialize-with-schema
               (find-schema 'user-schema) *user*))))))
  (unserialize-with-schema
   (find-schema 'user-schema)
   (json:decode-json-from-string data)
   :json))

;; Parsing

(let ((data
        (with-output-to-string (s)
          (generic-serializer:with-serializer-output s
            (generic-serializer:with-serializer :json
              (serialize-with-schema
               (find-schema 'user-schema) *user*))))))
  (parse-with-schema
   (find-schema 'user-schema)
   (json:decode-json-from-string data)))

(deftest schema-unserialization-validation-test ()

  ;; Fails
  (signals validation-error
    (let ((data '((id . 22))))
      (unserialize-with-schema
       (find-schema 'user-schema)
       data :json)))

  ;; Ok
  (finishes
    (let ((data '((id . 22) (realname . "asdf") (age . "22"))))
      (unserialize-with-schema
       (find-schema 'user-schema)
       data :json)))

  ;; Ok
  (finishes
    (let ((data '((id . 22) (realname . "asdf") (age . "23"))))
      (unserialize-with-schema
       (find-schema 'user-schema)
       data :json)))

  ;; Ok
  (finishes
    (let ((data '((id . 22) (realname . "asdf") (age . 454))))
      (unserialize-with-schema
       (find-schema 'user-schema)
       data :json)))

  ;; Fails
  (signals validation-error
    (let ((data '((id . 22) (realname . "asdf") (age . "23")
                  (best-friend . 33))))
      (unserialize-with-schema
       (find-schema 'user-schema)
       data :json)))

  ;; Fails
  (signals validation-error
    (let ((data '((id . 22) (realname . "asdf") (age . "23")
                  (best-friend . ((id . 34))))))
      (unserialize-with-schema
       (find-schema 'user-schema)
       data :json)))

  ;; Ok
  (finishes
    (let ((data '((id . 22) (realname . "asdf") (age . "23")
                  (best-friend . ((id . 34) (realname . "dfdf") (age . 44))))))
      (unserialize-with-schema
       (find-schema 'user-schema)
       data :json))))
