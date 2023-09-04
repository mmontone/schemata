(in-package :schemata-generators)

(defschema person
    (object person
            ((name string)
             (age integer :required nil))))

(defschema person
    (object person
            ((name string)
             (age integer :required nil)
             (friends (list-of (ref person))))))

(generate (find-schema 'person))

(defschema person
    (object person
            ((name string)
             (age integer :required nil)
             (friend (ref person) :required nil))))

(defschema person
    (object person
            ((name string)
             (age integer :required nil)
             (friends (list-of person))
             (best-friend (ref person) :required nil))))
