# SCHEMATA

Generic purpose schema library for serialization and validation of data.

This library is used by CL-REST-SERVER for API serialization and validation.

## Example

```lisp
(schemata:define-schema customer
    (:object "customer"
             ((id :string :external-name "id" :accessor
                  customer-id :documentation "customer id")
              (number :string :external-name "number" :optional t
                              :accessor customer-nr :documentation
                      "customer number")
              (name :string :external-name "name" :accessor
                    customer-name :documentation "customer name")
              (address-1 :string :external-name "address1"
                                 :optional t :documentation
                         "customer first address")
              (address-2 :string :external-name "address2"
                                 :optional t :documentation
                         "customer second address")
              (postal-code :string :external-name "postalcode"
                                   :optional t :documentation
                           "postal code")
              (postal-area :string :external-name "postalarea"
                                   :optional t :documentation
                           "postal area")
              (country :string :external-name "country" :optional
                       t :documentation "country code")
              (phone :string :external-name "phone" :optional t
                             :documentation "phone")
              (fax :string :external-name "fax" :optional t
                           :documentation "fax")
              (email :string :external-name "email" :optional t
                             :documentation "email"))
             (:documentation "customer data fetched")))
```

Then you can validate data using that schema, and serialize/unserialize data too.

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
