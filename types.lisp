(cl:defpackage :cl-tiger/types
  (:use :cl)
  (:local-nicknames
   (:symbol :cl-tiger/symbol))
  (:export

   #:ty

   #:int-ty
   #:make-int-ty

   #:string-ty
   #:make-string-ty

   #:record-ty
   #:make-record-ty

   #:array-ty
   #:make-array-ty

   #:nil-ty
   #:make-nil-ty

   #:unit-ty
   #:make-unit-ty

   #:name-ty
   #:make-name-ty))

(cl:in-package :cl-tiger/types)

(defclass ty ()
  ())

(defclass int-ty (ty)
  ())

(defun make-int-ty ()
  (make-instance 'int-ty))

(defclass string-ty (ty)
  ())

(defun make-string-ty ()
  (make-instance 'string-ty))

(defclass record-ty (ty)
  ((fields
    ;; A list of (sym ty)
    :type list
    :initform nil
    :initarg :fields
    :accessor record-ty-fields)))

(defun make-record-ty (fields)
  (make-instance 'record-ty :fields fields))

(defclass array-ty (ty)
  ((base-type
    :type ty
    :initform (error "Must supply the base-type of array-ty")
    :initarg :base-type
    :accessor array-ty-base-type)))

(defun make-array-ty (base-type)
  (make-instance 'array-ty :base-type base-type))

(defclass nil-ty (ty)
  ())

(defun make-nil-ty ()
  (make-instance 'nil-ty))

(defclass unit-ty (ty)
  ())

(defun make-unit-ty ()
  (make-instance 'unit-ty))

(defclass name-ty (ty)
  ((sym
    :type symbol:sym
    :initform (error "Must supply the symbol of the name-ty.")
    :initarg :sym
    :accessor name-ty-sym)
   (ty
    :type (or ty null)
    :initform nil
    :initarg :ty
    :accessor name-ty-ty)))

(defun make-name-ty (sym &optional ty)
  (make-instance 'name-ty :sym sym :ty ty))
