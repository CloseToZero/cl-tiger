(cl:defpackage :cl-tiger/instr-select
  (:use :cl)
  (:local-nicknames
   (:target :cl-tiger/target))
  (:export
   #:select-instrs

   ;; internal functions
   #:select-instrs%))

(cl:in-package :cl-tiger/instr-select)

(defun select-instrs (stm frame target)
  (select-instrs% stm frame target (target:target-arch target) (target:target-os target)))

(defgeneric select-instrs% (stm frame target target-arch target-os))
