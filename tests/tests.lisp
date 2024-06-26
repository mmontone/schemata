(defpackage :schemata.tests
  (:use :cl :schemata :stefil)
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

(defschema user-schema
    (object user
            ((id integer :accessor id)
             (realname string)
             (age integer :required nil)
             (best-friend (schema user-schema)
                          :required nil)
             (groups (list-of (schema group-schema))
                     :required nil))
            (:class user)))

(defschema minimal-user-schema
    (object user
            ((id integer)
             (realname string))))

(defschema group-schema
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

(deftest alists-test ()
  (signals validation-error
    (validate-with-schema
     (schema (alist-of (symbol . string)))
     'something))
  (finishes
    (validate-with-schema
     (schema (alist-of (symbol . string)))
     '()))
  (signals validation-error
    (validate-with-schema
     (schema (alist-of (symbol . string)))
     '(foo)))
  (signals validation-error
    (validate-with-schema
     (schema (alist-of (symbol . string)))
     '((22 . "foo"))))
  (finishes
    (validate-with-schema
     (schema (alist-of (symbol . string)))
     '((foo . "foo"))))

  (let ((alist-schema (schema (alist ((:x . integer)
                                      (:y . string))))))
    ;; non list errors
    (signals validation-error
      (validate-with-schema alist-schema 'foo))
    ;; required keys
    (signals validation-error
      (validate-with-schema alist-schema '()))
    ;; malformation
    (signals validation-error
      (validate-with-schema alist-schema '(:x 22 :y "lala")))
    ;; values
    (signals validation-error
      (validate-with-schema alist-schema '((:x . 22) (:y . 33))))
    (finishes
      (validate-with-schema alist-schema '((:x . 22) (:y . "foo"))))
    )
  )

(deftest schemas-tests ()
  (finishes
    (validate-with-schema
     (schema (member-of '(foo bar)))
     'foo))
  (signals validation-error
    (validate-with-schema
     (schema (member-of '(foo bar)))
     'baz))

  (validate-with-schema
   (schema (const 'foo))
   'foo)

  (signals validation-error
    (validate-with-schema
     (schema (const 'foo))
     'bar))

  (finishes
    (validate-with-schema
     (schema (or (const 'foo) (const 'bar)))
     'foo))

  (finishes
    (validate-with-schema
     (schema (or (const 'foo) (const 'bar)))
     'bar))

  (signals validation-error
    (validate-with-schema
     (schema (or (const 'foo) (const 'bar)))
     'baz)))

(deftest plists-test ()
  (signals validation-error
    (validate-with-schema
     (schema (plist-of symbol string))
     'something))
  (finishes
    (validate-with-schema
     (schema (plist-of symbol string))
     '()))
  (signals validation-error
    (validate-with-schema
     (schema (plist-of symbol string))
     '(foo)))
  (signals validation-error
    (validate-with-schema
     (schema (plist-of symbol integer))
     '(:aaa "foo")))
  (finishes
    (validate-with-schema
     (schema (plist-of symbol integer))
     '(foo 22)))

  (let ((plist-schema (schema (plist (:x integer :y string)))))
    ;; non list errors
    (signals validation-error
      (validate-with-schema plist-schema 'foo))
    ;; required keys
    (signals validation-error
      (validate-with-schema plist-schema '()))
    ;; malformation
    (signals validation-error
      (validate-with-schema plist-schema '(:x 22 :z :y "lala")))
    ;; values
    (signals validation-error
      (validate-with-schema plist-schema '(:x 22 :y 33)))
    (finishes
      (validate-with-schema plist-schema '(:x 22 :y "foo")))
    )
  )
