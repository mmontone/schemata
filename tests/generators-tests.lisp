(in-package :schemata-generators)

(defparameter *person-names*
  (list "John" "Peter" "Stan" "Steve" "Wendy" "Caroline"))

(def-generator person-name-generator ()
  (generator (member-of *person-names*)))

(def-generator age-generator ()
  (generator (func (lambda () (1+ (random 100))))))

(defschema person
    (object person
            ((name string :generator (generator (person-name-generator)))
             (age integer :required nil
                          :generator (generator (age-generator))))))

(defschema person
    (object person
            ((name string :generator (generator (person-name-generator)))
             (age integer :required nil :generator (generator (age-generator)))
             (friends (list-of (ref person))))))

(generate (find-schema 'person))

(defschema person
    (object person
            ((name string :generator (generator (person-name-generator)))
             (age integer :required nil :generator (generator (age-generator)))
             (friend (ref person) :required nil))))

(defschema person
    (object person
            ((name string :generator (generator (person-name-generator)))
             (age integer :required nil :generator (generator (age-generator)))
             (friends (list-of person))
             (best-friend (ref person) :required nil))))
