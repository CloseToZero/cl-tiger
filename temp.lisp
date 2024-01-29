(cl:defpackage :cl-tiger/temp
  (:use :cl)
  (:local-nicknames
   (:symbol :cl-tiger/symbol))
  (:export
   #:temp
   #:temp-num
   #:temp-name
   #:new-temp

   #:label
   #:new-label))

(cl:in-package :cl-tiger/temp)

(defvar *temp-count* 0)

(serapeum:defconstructor temp
  (num integer)
  (name string))

;; User should use (new-temp) rather than (temp integer) to create temp,
;; even we also export the temp symbol.
(defun new-temp ()
  (let ((count *temp-count*))
    (incf *temp-count*)
    (temp count (format nil "t~D" count))))

(deftype label () 'symbol:sym)

(defvar *label-count* 0)

(defun new-label (&optional name)
  (symbol:get-sym
   (or name
       (let ((count *label-count*))
         (incf *label-count*)
         (format nil "L~D" count)))))
