(cl:defpackage :cl-tiger/ir
  (:use :cl)
  (:local-nicknames
   (:temp :cl-tiger/temp))
  (:export
   #:bin-op
   #:plus-bin-op
   #:minus-bin-op
   #:times-bin-op
   #:div-bin-op
   #:and-bin-op
   #:or-bin-op
   #:xor-bin-op
   #:lshift-bin-op
   #:rshift-bin-op
   #:arshift-bin-op

   #:rel-op
   #:eq-rel-op
   #:neq-rel-op
   #:lt-rel-op
   #:le-rel-op
   #:gt-rel-op
   #:ge-rel-op
   #:ult-rel-op
   #:ule-rel-op
   #:ugt-rel-op
   #:uge-rel-op

   #:expr
   #:const-expr
   #:const-expr-value
   #:label-expr
   #:label-expr-value
   #:temp-expr
   #:temp-expr-value
   #:bin-op-expr
   #:bin-op-expr-left
   #:bin-op-expr-op
   #:bin-op-expr-right
   #:mem-expr
   #:mem-expr-value
   #:call-expr
   #:call-expr-fun
   #:call-expr-args
   #:stm-then-expr
   #:stm-then-expr-stm
   #:stm-then-expr-expr

   #:stm
   #:move-stm
   #:move-stm-left
   #:move-stm-right
   #:expr-stm
   #:expr-stm-value
   #:jump-stm
   #:jump-stm-target
   #:jump-stm-possible-labels
   #:cjump-stm
   #:cjump-stm-left
   #:cjump-stm-op
   #:cjump-stm-right
   #:cjump-stm-true-target
   #:cjump-stm-false-target
   #:compound-stm
   #:compound-stm-first
   #:compound-stm-second
   #:label-stm
   #:label-stm-name))

(cl:in-package :cl-tiger/ir)

(serapeum:defunion bin-op
  plus-bin-op
  minus-bin-op
  times-bin-op
  div-bin-op
  and-bin-op
  or-bin-op
  xor-bin-op
  lshift-bin-op
  rshift-bin-op
  arshift-bin-op)

(serapeum:defunion rel-op
  eq-rel-op
  neq-rel-op
  lt-rel-op
  le-rel-op
  gt-rel-op
  ge-rel-op
  ult-rel-op
  ule-rel-op
  ugt-rel-op
  uge-rel-op)

(serapeum:defunion expr
  (const-expr
   (value integer))
  (label-expr
   (value temp:label))
  (temp-expr
   (value temp:temp))
  (bin-op-expr
   (left expr)
   (op bin-op)
   (right expr))
  (mem-expr
   (value expr))
  (call-expr
   (fun expr)
   ;; A list of expr.
   (args list))
  (stm-then-expr
   (stm stm)
   (expr expr)))

(serapeum:defunion stm
  (move-stm
   (left expr)
   (right expr))
  (expr-stm
   (value expr))
  (jump-stm
   (target expr)
   ;; A list of temp:label.
   (possible-labels list))
  (cjump-stm
   (left expr)
   (op rel-op)
   (right expr)
   (true-target temp:label)
   (false-target temp:label))
  (compound-stm
   (first stm)
   (second stm))
  (label-stm
   (name temp:label)))
