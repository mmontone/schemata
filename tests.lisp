(defpackage :schemata.tests
  (:use :cl :schemata :stefil)
  (:local-nicknames (:gs :generic-serializer))
  (:export :run-tests))

(in-package :schemata.tests)

(stefil:defsuite* schemata)

(defun run-tests ()
  (schemata))

(defparameter *schema*
  (schema
   (object user
           ((id integer)
            (realname string)
            (age integer :required nil)
            (sex (member :male :female))
            (best-friend (object user
                                 ((id integer)
                                  (realname string))))
            (groups (list-of (object group
                                     ((id integer)
                                      (name string))))
                    :required nil)))))

(define-schema user-schema
    (object user
            ((id integer :accessor id)
             (realname string)
             (age integer :required nil)
             (best-friend (schema user-schema)
                          :required nil)
             (groups (list-of (schema group-schema))
                     :required nil))
            (:class user)))

(define-schema minimal-user-schema
    (object user
            ((id integer)
             (realname string))))

(define-schema group-schema
    (object group
            ((id integer)
             (name string)
             (users (list-of user-schema)
                    :required nil
                    ))
            (:class group)))

(defclass user ()
  ((id :initarg :id
       :accessor id
       :initform (error "Provide the id"))
   (realname :initarg :realname
             :accessor realname
             :initform (error "Provide the realname"))
   (age :initarg :age
        :accessor age
        :initform (error "Provide the age"))
   (sex :initarg :sex
        :accessor sex
        :initform nil)
   (groups :initarg :groups
           :accessor groups
           :initform nil)
   (best-friend :initarg :best-friend
                :accessor best-friend
                :initform nil)))

(defclass group ()
  ((id :initarg :id
       :accessor id
       :initform (error "Provide the id"))
   (name :initarg :name
         :accessor name
         :initform (error "Provide the name"))
   (users :initarg :users
          :accessor users
          :initform nil)))

(defparameter *user*
  (make-instance 'user
                 :realname "Mariano"
                 :id 2
                 :age 30
                 :groups (list (make-instance 'group
                                              :name "My group"
                                              :id 3))
                 :best-friend (make-instance 'user
                                             :id 3
                                             :realname "Fernando"
                                             :age 31
                                             )))

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
              (gs:with-serializer-output s
                (gs:with-serializer :json
                  (serialize-with-schema
                   *schema* user))))))
      (finishes (json:decode-json-from-string json)))))

(with-output-to-string (s)
  (gs:with-serializer-output s
    (gs:with-serializer :json
      (serialize-with-schema
       (find-schema 'user-schema) *user*))))

(with-output-to-string (s)
  (gs:with-serializer-output s
    (gs:with-serializer :json
      (serialize-with-schema
       (find-schema 'minimal-user-schema) *user*))))

(with-output-to-string (s)
  (gs:with-serializer-output s
    (gs:with-serializer :xml
      (serialize-with-schema
       *schema* *user*))))

(with-output-to-string (s)
  (gs:with-serializer-output s
    (gs:with-serializer :xml
      (serialize-with-schema
       (find-schema 'user-schema) *user*))))

(with-output-to-string (s)
  (gs:with-serializer-output s
    (gs:with-serializer :xml
      (serialize-with-schema
       (find-schema 'minimal-user-schema) *user*))))

;; MOP

(defclass schema-user ()
  ((id :initarg :id
       :accessor id
       :type integer)
   (realname :initarg :realname
             :accessor realname
             :initform (error "Provide the realname")
             :type string)
   (age :initarg :age
        :accessor age
        :initform (error "Provide the age")
        :type integer)
   (groups :initarg :groups
           :accessor groups
           :initform nil
           :schema (list-of (ref group-schema)))
   (best-friend :initarg :best-friend
                :accessor best-friend
                :initform nil
                :schema (ref user-schema)
                :required nil)
   (another-friend :initarg :another-friend
                   :accessor another-friend
                   :initform nil
                   :schema (ref schema-user)
                   :required nil)
   (hobbies :initarg :hobbies
            :accessor hobbies
            :schema (list-of string)
            :initform nil
            :required nil))
  (:metaclass schemata:schema-class))

(closer-mop:finalize-inheritance (find-class 'schema-user))

(schema-class-schema (find-class 'schema-user))
(find-schema 'schema-user)

(defparameter *schema-user*
  (make-instance 'schema-user
                 :realname "Mariano"
                 :id 2
                 :age 30
                 :groups (list (make-instance 'group
                                              :name "My group"
                                              :id 3))
                 :best-friend (make-instance 'schema-user
                                             :id 3
                                             :realname "Fernando"
                                             :age 31)
                 :another-friend (make-instance 'schema-user
                                                :id 3
                                                :realname "Julio"
                                                :age 31)
                 :hobbies (list "reading" "swimming")))

(with-output-to-string (s)
  (gs:with-serializer-output s
    (gs:with-serializer :json
      (serialize-with-schema
       (schema-class-schema
        (find-class 'schema-user))
       *schema-user*))))

(with-output-to-string (s)
  (gs:with-serializer-output s
    (gs:with-serializer :json
      (gs:serialize *schema-user*))))

(with-output-to-string (s)
  (gs:with-serializer-output s
    (gs:with-serializer :xml
      (serialize-with-schema
       (schema-class-schema
        (find-class 'schema-user))
       *schema-user*))))

(with-output-to-string (s)
  (gs:with-serializer-output s
    (gs:with-serializer :xml
      (gs:serialize *schema-user*))))

;; Unserialization

(let ((data
        (with-output-to-string (s)
          (gs:with-serializer-output s
            (gs:with-serializer :json
              (serialize-with-schema
               (find-schema 'user-schema) *user*))))))
  (unserialize-with-schema
   (find-schema 'user-schema)
   (json:decode-json-from-string data)
   :json))

;; Parsing

(let ((data
        (with-output-to-string (s)
          (gs:with-serializer-output s
            (gs:with-serializer :json
              (serialize-with-schema
               (find-schema 'user-schema) *user*))))))
  (parse-with-schema
   (find-schema 'user-schema)
   (json:decode-json-from-string data)))

;; Validation

(deftest schema-parsing-validation-test ()

  ;; Fails
  (signals validation-error
    (let ((data '((id . 22))))
      (parse-with-schema
       (find-schema 'user-schema)
       data)))

  ;; Ok
  (finishes
    (let ((data '((id . 22) (realname . "asdf"))))
      (parse-with-schema
       (find-schema 'user-schema)
       data)))

  ;; Ok
  (finishes
    (let ((data '((id . 22) (realname . "asdf") (age . "23"))))
      (parse-with-schema
       (find-schema 'user-schema)
       data)))

  ;; Ok
  (finishes
    (let ((data '((id . 22) (realname . "asdf") (age . 454))))
      (parse-with-schema
       (find-schema 'user-schema)
       data)))

  ;; Fails
  (signals validation-error
           (let ((data '((id . 22) (realname . "asdf") (age . "23")
                         (best-friend . 33))))
             (parse-with-schema
              (find-schema 'user-schema)
              data)))

  ;; Fails
  (signals validation-error
           (let ((data '((id . 22) (realname . "asdf") (age . "23")
                         (best-friend . ((id . 34))))))
             (parse-with-schema
              (find-schema 'user-schema)
              data)))

  ;; Ok
  (finishes
    (let ((data '((id . 22) (realname . "asdf") (age . "23")
                  (best-friend . ((id . 34) (realname . "dfdf"))))))
      (parse-with-schema
       (find-schema 'user-schema)
       data))))

(deftest schema-unserialization-validation-test ()

  ;; Fails
  (signals validation-error
    (let ((data '((id . 22))))
      (unserialize-with-schema
       (find-schema 'user-schema)
       data :json)))

  ;; Ok
  (finishes
    (let ((data '((id . 22) (realname . "asdf"))))
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
                  (best-friend . ((id . 34) (realname . "dfdf"))))))
      (unserialize-with-schema
       (find-schema 'user-schema)
       data :json))))

(deftest validate-with-schema-test ()
  (signals validation-error
    (schemata:validate-with-schema *schema*
                                   '((:id . 22))))
  (finishes
    (schemata:validate-with-schema *schema*
                                   '((:id . 1)
                                     (:realname . "bar")
                                     (:sex . :male)
                                     (:best-friend . ((:id . 2)
                                                      (:realname . "foo")))))))

(deftest populate-with-schema-test ()
  (let ((user
          (make-instance 'user
                         :realname "Mariano"
                         :id 2
                         :age 30
                         :groups (list (make-instance 'group
                                                      :name "My group"
                                                      :id 3))
                         :best-friend (make-instance 'user
                                                     :id 3
                                                     :realname "Fernando"
                                                     :age 31
                                                     )))
        (data `((:id . 1)
                (:realname . "bar")
                (:sex . :male)
                (:best-friend . ((:id . 2)
                                 (:realname . "foo"))))))
    (schemata:validate-with-schema *schema* data)
    (schemata:populate-with-schema *schema* user data)
    (is (string= (realname user) "bar"))))

(deftest patch-with-schema-test ()
  (let ((user
          (make-instance 'user
                         :realname "Mariano"
                         :id 2
                         :age 30
                         :groups (list (make-instance 'group
                                                      :name "My group"
                                                      :id 3))
                         :best-friend (make-instance 'user
                                                     :id 3
                                                     :realname "Fernando"
                                                     :age 31
                                                     ))))
    (signals validation-error
      (schemata:patch-with-schema *schema* user '((:realname . 22))))
    (finishes
      (schemata:patch-with-schema *schema* user nil))
    (finishes
      (schemata:patch-with-schema *schema* user '((:realname . "John"))))
    (is (string= (realname user) "John"))
    (is (= (age user) 30))
    (is (= (id user) 2))))
