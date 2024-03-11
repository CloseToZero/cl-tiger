(cl:defpackage :cl-tiger/temp
  (:use :cl)
  (:local-nicknames
   (:symbol :cl-tiger/symbol))
  (:export
   #:*temp-count*
   #:*label-count*
   #:temp
   #:temp-num
   #:temp-name
   #:new-temp
   #:named-temp

   #:label
   #:new-label
   #:new-named-label
   #:label-name))

(cl:in-package :cl-tiger/temp)

(defvar *temp-count* 0)

(serapeum:defconstructor temp
  (num integer)
  (name string))

;; User should use (new-temp) rather than (temp integer) to create temp,
;; even we also export the temp symbol.
(defun new-temp (&optional (base-name ""))
  (let ((count *temp-count*))
    (incf *temp-count*)
    (temp count (format nil "t~A~D" base-name count))))

(defvar *name->temp-table* (make-hash-table :test #'equal))

(defun named-temp (name)
  (or (gethash name *name->temp-table*)
      (let ((count *temp-count*))
        (incf *temp-count*)
        (setf (gethash name *name->temp-table*)
              (temp count name)))))

(defmethod fset:compare ((x temp) (y temp))
  (let ((num-x (temp-num x))
        (num-y (temp-num y)))
    (cond ((< num-x num-y) :less)
          ((> num-x num-y) :greater)
          (t :equal))))

(deftype label () 'symbol:sym)

(defvar *label-count* 0)

(defun new-label (&optional (base-name ""))
  (symbol:get-sym
   (let ((count *label-count*))
     (incf *label-count*)
     (format nil "L~A~D" base-name count))))

(defun new-named-label (name)
  (symbol:get-sym name))

(defun label-name (label)
  (symbol:sym-name label))
