(cl:defpackage :cl-tiger/runtime
  (:use :cl)
  (:export
   #:*runtime-functions*))

(cl:in-package :cl-tiger/runtime)

(defvar *runtime-functions*
  (list "AllocRecord"
        "AllocArray"
        "StringCompare"))
