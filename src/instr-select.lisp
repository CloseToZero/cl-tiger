(cl:defpackage :cl-tiger/instr-select
  (:use :cl)
  (:local-nicknames
   (:target :cl-tiger/target)
   (:frame :cl-tiger/frame))
  (:export
   #:select-instrs

   ;; internal functions
   #:alloc-data
   #:select-instrs%))

(cl:in-package :cl-tiger/instr-select)

(defvar *data-frags* nil)

(defun alloc-data (label data)
  (push (frame:frag-data label data) *data-frags*))

(defun select-instrs (stm frame target)
  (let ((*data-frags* nil))
    (list
     (select-instrs% stm frame target (target:target-arch target) (target:target-os target))
     (nreverse *data-frags*))))

(defgeneric select-instrs% (stm frame target target-arch target-os))
