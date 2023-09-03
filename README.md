# SCHEMATA

Generic purpose schema library for serialization and validation of data.

This library is used by CL-REST-SERVER for API serialization and validation.

## Example

```lisp
(schemata:defschema customer
    (object "customer"
             ((id string :external-name "id" :accessor
                  customer-id :documentation "customer id")
              (number string :external-name "number" :required nil
                              :accessor customer-nr :documentation
                      "customer number")
              (name string :external-name "name" :accessor
                    customer-name :documentation "customer name")
              (address-1 string :external-name "address1"
                                 :required nil :documentation
                         "customer first address")
              (address-2 string :external-name "address2"
                                 :required nil :documentation
                         "customer second address")
              (postal-code string :external-name "postalcode"
                                   :required nil :documentation
                           "postal code")
              (postal-area string :external-name "postalarea"
                                   :required nil :documentation
                           "postal area")
              (country string :external-name "country" :required nil 
                       :documentation "country code")
              (phone string :external-name "phone" :required nil
                             :documentation "phone")
              (fax string :external-name "fax" :required nil
                           :documentation "fax")
              (email string :external-name "email" :required nil
                             :documentation "email"))
             (:documentation "customer data fetched")))
```

Then you can validate data using that schema, and serialize/unserialize data too.

[Read the full documentation](https://mmontone.github.io/schemata/ "Full documentation")

## Schema types

### Type schemas

Schemas can be built from Common Lisp types:

```lisp
SCHEMATA> (defparameter *s* (schema string))
*S*
SCHEMATA> *s*
#<TYPE-SCHEMA STRING {1006FBBD13}>
SCHEMATA> (validate-with-schema *s* "22")
NIL
SCHEMATA> (validate-with-schema *s* 22 :error-p nil)
#<VALIDATION-ERROR "~s is not of type: ~a" {100152EB13}>
```

### Object schema

Object schemas are built using the syntax: `(object name attributes options)`.
Attributes are specified as: `(attribute-name attribute-type &rest options)`.

The `attribute-type` is parsed as a schema.

Possible attribute options are: `required`, `required-message`, `default`, `accessor`, `writer`, `reader`, `parser`, `validator`, `add-validator`, `formatter`, `external-name`, `serializer`, `unserializer`, `slot`.

Example:

```lisp
SCHEMATA> (schema (object person
                          ((name string)
                           (age integer :required nil))))
#<OBJECT-SCHEMA {1001843543}>
```

### List schema

Homogeneous list of schemas are specified via `list-of`.

Example:

```lisp
SCHEMATA> (defparameter *s* (schema (list-of integer)))
*S*
SCHEMATA> (validate-with-schema *s* '(1 2 "foo"))
; Evaluation aborted on #<SCHEMATA:VALIDATION-ERROR "~s is not of type: ~a" {1006ECA323}>.
SCHEMATA> (validate-with-schema *s* '(1 2 3))
NIL
```
### Schema references

Defined schemas can be referenced via either `(schema schema-name)` or `(ref schema-name)` (they are identical).

Example:

```lisp
SCHEMATA> (defschema person
            (object person
                    ((name string))))
#<OBJECT-SCHEMA {1006F8A813}>
SCHEMATA> (defparameter *list-of-person* (schema (list-of (ref person))))
*LIST-OF-PERSON*
SCHEMATA> *list-of-person*
#<LIST-SCHEMA {1006F8C2A3}>
SCHEMATA> (parse-with-schema *list-of-person* '((("name" . "Mariano")) (("name" . "Peter"))))
(((NAME . "Mariano")) ((NAME . "Peter")))
SCHEMATA> (validate-with-schema *list-of-person* '((("name" . 22)) (("name" . "Peter"))))
; processing (DEFMETHOD SCHEMA-VALIDATE ...); Evaluation aborted on #<SB-PCL::NO-APPLICABLE-METHOD-ERROR {1008018513}>.
SCHEMATA> (validate-with-schema *list-of-person* '((("name" . 22)) (("name" . "Peter"))))
; Evaluation aborted on #<SCHEMATA:VALIDATION-ERROR "~s is not of type: ~a" {10082EB883}>.
SCHEMATA> (validate-with-schema *list-of-person* '((("name" . "Mariano")) (("name" . "Peter"))))
NIL
SCHEMATA> (validate-with-schema *list-of-person* '((("names" . "Mariano")) (("name" . "Peter"))))
; Evaluation aborted on #<SCHEMATA:VALIDATION-ERROR "Attributes not part of schema: ~a" {1008CD3DD3}>.
```

## SATISFIES-SCHEMA type

Schemata integrates with the Lisp type system via the SATISFIES-SCHEMA type.
Schemas can be thought as types over data.
Defined schemas can be checked using TYPEP and CHECK-TYPE with the type `(satisfies-schema schema-name)`.

Example:

```lisp
SCHEMATA> (defschema string-schema string)
#<TYPE-SCHEMA STRING {10019DA8B3}>
SCHEMATA> (typep "foo" '(satisfies-schema string-schema))
T
SCHEMATA> (typep 22 '(satisfies-schema string-schema))
NIL
SCHEMATA> (let ((x "foo"))
            (check-type x (satisfies-schema string-schema))
            x)
"foo"
```

## SCHEMA-CLASS metaclass

SCHEMA-CLASS classes get an schema attached.

Example:

```lisp
SCHEMATA> (def-schema-class person ()
            ((name :type string :initarg :name)
             (age :type integer :required nil :initarg :age)))
#<SCHEMA-CLASS SCHEMATA::PERSON>

SCHEMATA> (validate-with-schema (find-class 'person) '(("name" . "Mariano") ("age" . 22)))
NIL

SCHEMATA> (validate-with-schema (find-class 'person) '(("name" . "Mariano") ("age" . 'asdf)) :error-p nil)
#<VALIDATION-ERROR 'ASDF is not of type: INTEGER {100109F833}>

SCHEMATA> (generic-serializer:with-serializer :json
            (generic-serializer:serialize (make-instance 'person :name "Mariano" :age 44)))
{"name":"Mariano","age":44}
```
