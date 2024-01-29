(cl:defpackage :cl-tiger/types
  (:use :cl)
  (:local-nicknames
   (:symbol :cl-tiger/symbol))
  (:export
   #:ty
   #:int-ty
   #:string-ty
   #:record-ty
   #:record-ty-fields
   #:array-ty
   #:array-ty-base-type
   #:nil-ty
   #:unit-ty
   #:name-ty
   #:name-ty-sym
   #:name-ty-ty-ref
   #:ty-ref
   #:ty-ref-value))

(cl:in-package :cl-tiger/types)

(serapeum:defunion ty
  int-ty
  string-ty
  (record-ty
   ;; A list of (sym ty)
   (fields list))
  (array-ty
   (base-type ty))
  nil-ty
  unit-ty
  (name-ty
   (sym symbol:sym)
   (ty-ref ty-ref)))

(defstruct (ty-ref
            (:conc-name ty-ref-)
            (:constructor ty-ref (value)))
  (value nil :type (or ty null)))
