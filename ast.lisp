(cl:defpackage :cl-tiger/ast
  (:use :cl)
  (:export
   #:type-id
   #:make-type-id
   #:record-type
   #:make-record-type
   #:array-type
   #:make-array-type
   #:type-declaration
   #:make-type-declaration
   #:type-declarations
   #:make-type-declarations))

(cl:in-package :cl-tiger/ast)

(defclass type-id ()
  ((name
    :initform "int"
    :initarg :name
    :accessor type-id-name
    :documentation "The name of the type.")))

(defmethod print-object ((type-id type-id) stream)
  (format stream "type-id(~A)" (type-id-name type-id)))

(defun make-type-id (name)
  (make-instance 'type-id :name name))

(defclass record-type ()
  ((fields
    :initform nil
    :initarg :fields
    :accessor record-type-fields
    :documentation
    "A list of fields of the form (name type-id) where the name is a string
    and the type-id is of type type-id.")))

(defmethod print-object ((record-type record-type) stream)
  (format stream "record { ")
  (let ((first? t))
    (dolist (field (record-type-fields record-type))
      (let ((name (first field))
            (type-id (second field)))
        (unless first?
          (format stream ", "))
        (format stream "~A : ~A" name type-id))
      (setf first? nil)))
  (format stream " }"))

(defun make-record-type (fields)
  (make-instance 'record-type :fields fields))

(defclass array-type ()
  ((base-type
    :initform "int"
    :initarg :base-type
    :accessor array-type-base-type)))

(defmethod print-object ((array-type array-type) stream)
  (format stream "array of ~A" (array-type-base-type array-type)))

(defun make-array-type (base-type)
  (make-instance 'array-type :base-type base-type))

(defclass type-declaration ()
  ((name
    :initform (error "Must supply the name of the type-declaration.")
    :initarg :name
    :accessor type-declaration-name)
   (type
    :initform (error "Must supply the name of the type-declaration.")
    :initarg :type
    :accessor type-declaration-type)))

(defmethod print-object ((type-declaration type-declaration) stream)
  (format stream "type ~A = ~A"
          (type-declaration-name type-declaration)
          (type-declaration-type type-declaration)))

(defun make-type-declaration (name type)
  (make-instance 'type-declaration :name name :type type))

(defclass type-declarations ()
  ((decls
    :initform nil
    :initarg :decls
    :accessor type-declarations-decls
    :documentation "A list of type declarations that appeared successively.")))

(defmethod print-object ((type-declarations type-declarations) stream)
  (format stream "decls(~{~A~^, ~})" (type-declarations-decls type-declarations)))

(defun make-type-declarations (decls)
  (make-instance 'type-declarations :decls decls))
