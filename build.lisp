(cl:defpackage :cl-tiger/build
  (:use :cl)
  (:local-nicknames
   (:temp :cl-tiger/temp)
   (:target :cl-tiger/target)
   (:frame :cl-tiger/frame))
  (:export
   #:frag
   #:frag-str
   #:frag-str-def
   #:frag-fun
   #:frag-fun-prolog
   #:frag-fun-body-instrs
   #:frag-fun-epilog

   #:build
   #:build%))

(cl:in-package :cl-tiger/build)

(serapeum:defunion frag
  (frag-str
   (def string))
  (frag-fun
   ;; A list of strings
   (prolog list)
   ;; A list of instr:instr
   (body-instrs list)
   ;; A list of strings
   (epilog list)))

(defun build (frag-strs frag-funs dst-dir target)
  (build% frag-strs frag-funs dst-dir
          target (target:target-arch target) (target:target-os target)))

(defgeneric build% (frag-strs frag-funs dst-dir target target-arch target-os))
