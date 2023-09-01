# SCHEMATA

Generic purpose schema library for serialization and validation of data.

This library is used by CL-REST-SERVER for API serialization and validation.

## Example

```lisp
(schemata:define-schema customer
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

## Schema types

### Type schemas

Schemas can be built from Common Lisp types:

```lisp
SCHEMATA> (schema string)
#<TYPE-SCHEMA STRING {10010ADF83}>
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
SCHEMATA> (define-schema person
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

## Functions

### attribute-external-name

```lisp
(attribute)
```

Name of the field that is shown in error messages (and in serialization?)

### find-schema

```lisp
(name &optional (errorp t))
```

Find a schema definition by name

### object-class

```lisp
(object)
```

Returns the CLOS class associated with an object. May be null.

### patch-with-schema

```lisp
(schema object data)
```

Populate CLOS objects from data + schema.
Only populates attributes available in DATA, validating them.
Useful for PATCH rest api operations implementations.
DATA should be an association list.


### populate-with-schema

```lisp
(schema object data &key exclude)
```

Populate CLOS objects from data + schema.
Attributes members of EXCLUDE parameter are not populated.

### serializable-class-schema

```lisp
(serializable-class)
```

Generate a schema using the serializable class meta info

### validate-with-schema

```lisp
(schema data &key (format :json) (collect-errors *collect-validation-errors*)
        (error-p *signal-validation-errors*))
```

Validate input using schema.
Useful for validating resource operations posted content (for :post and :put methods).
Input can be a string or an association list.



Args:
  - schema (symbol or schema): The schema
  - data (alist): The data to validate.
  - format (keyword): The data format
  - collect-errors (boolean): If true, collect all the validation errors. If false, return the first validation error found. Default: true.
  - error-p (boolean): If true, when validation errors are found, a validation error is signaled. If false, the validation errors are returned as the function result and no error is signaled.

## Macros
### define-schema

```lisp
(name schema)
```

Define a schema

## Generic-Functions
### parse-with-schema
Parses the string to an association list using the schema

## Slot-Accessors
## Variables
## Classs
### serializable-class
Metaclass for serializable objects

## Conditions
## Constants
