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
   #:rv
   #:word-size
   #:access-expr
   #:external-call
   #:view-shift-for-fun-body
   #:frag
   #:frag-str
   #:frag-str-label
   #:frag-str-str
   #:frag-fun
   #:frag-fun-body
   #:frag-fun-frame

   ;; internal functions:
   #:new-frame%
   #:alloc-local%
   #:fp%
   #:rv%
   #:word-size%
   #:access-expr%
   #:external-call%
   #:view-shift-for-fun-body%))

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

;; Returns an temp:temp
(defun fp (target)
  (fp% (target:target-arch target) (target:target-os target)))

(defgeneric fp% (target-arch target-os))

;; Returns an temp:temp
(defun rv (target)
  (rv% (target:target-arch target) (target:target-os target)))

(defgeneric rv% (target-arch target-os))

(defun word-size (target)
  (word-size% (target:target-arch target) (target:target-os target)))

(defgeneric word-size% (target-arch target-os))

(defun access-expr (access fp-expr target)
  (access-expr% access fp-expr (target:target-arch target) (target:target-os target)))

(defgeneric access-expr% (access fp-expr target-arch target-os))

(defun external-call (name args target)
  (external-call% name args  (target:target-arch target) (target:target-os target)))

(defgeneric external-call% (name args target-arch target-os))

;; Returns an new body-stm
(defun view-shift-for-fun-body (frame body-stm target)
  (view-shift-for-fun-body% frame body-stm (target:target-arch target) (target:target-os target)))

(defgeneric view-shift-for-fun-body% (frame body-stm target-arch target-os))

(serapeum:defunion frag
  (frag-str
   (label temp:label)
   (str string))
  (frag-fun
   (body ir:stm)
   (frame frame)))
