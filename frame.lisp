(cl:defpackage :cl-tiger/frame
  (:use :cl)
  (:local-nicknames
   (:target :cl-tiger/target)
   (:temp :cl-tiger/temp)
   (:ir :cl-tiger/ir))
  (:export
   #:access
   #:frame
   #:frame-name
   #:frame-formals
   #:new-frame
   #:alloc-local
   #:fp
   #:word-size
   #:access-expr

   ;; internal functions:
   #:new-frame%
   #:alloc-local%
   #:fp%
   #:word-size%
   #:access-expr%))

(cl:in-package :cl-tiger/frame)

(defclass access ()
  ())

(defclass frame ()
  ((name
    :type temp:label
    :initarg :name
    :reader frame-name)
   (formals
    ;; A list of access (not a list of boolean).
    :type list
    :initarg :formals
    :reader frame-formals)))

;; formals: A list of boolean to specify the number of arguments and
;; whether the corresponding argument can only reside in memory (which
;; we call it: escape).
(defun new-frame (name formals target)
  (new-frame% name formals
              (target:target-arch target) (target:target-os target)))

(defgeneric new-frame% (name formals target-arch target-os))

;; Returns an access.
(defun alloc-local (frame escape target)
  (alloc-local% frame escape
                (target:target-arch target) (target:target-os target)))

(defgeneric alloc-local% (frame escape target-arch target-os))

(defun fp (target)
  (fp% (target:target-arch target) (target:target-os target)))

;; Returns an temp:temp
(defgeneric fp% (target-arch target-os))

(defun word-size (target)
  (word-size% (target:target-arch target) (target:target-os target)))

(defgeneric word-size% (target-arch target-os))

(defun access-expr (access fp-expr target)
  (access-expr% access fp-expr (target:target-arch target) (target:target-os target)))

(defgeneric access-expr% (access fp-expr target-arch target-os))
