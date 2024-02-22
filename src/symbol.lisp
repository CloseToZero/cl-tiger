(cl:defpackage :cl-tiger/symbol
  (:use :cl)
  (:export
   #:sym
   #:get-sym
   #:sym-name))

(cl:in-package :cl-tiger/symbol)

(defclass sym ()
  ((name :type string
         :initarg :name
         :accessor sym-name)))

(defmethod print-object ((sym sym) stream)
  (format stream "(sym ~A)" (sym-name sym)))

(defun make-sym (name)
  (make-instance 'sym :name name))

(defvar *symbols* (make-hash-table :test #'equal))

(defun get-sym (name)
  (or (gethash name *symbols*)
      (setf (gethash name *symbols*)
            (make-sym name))))
