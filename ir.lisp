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
   #:int-expr
   #:int-expr-value
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
   #:label-stm-name

   #:pretty-print))

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
  (int-expr
   (value fixnum))
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

(defun pretty-print-expr (stream expr cur-indent indent)
  (format stream "~v:<~>" cur-indent)
  (serapeum:match-of expr expr
    ((int-expr value)
     (format stream "(int ~A)" value))
    ((label-expr value)
     (format stream "(label-expr ~A)" (temp:label-name value)))
    ((temp-expr value)
     (format stream "(temp ~A)" (temp:temp-name value)))
    ((bin-op-expr left op right)
     (format stream "(~A~%" (serapeum:match-of bin-op op
                              (plus-bin-op "+")
                              (minus-bin-op "-")
                              (times-bin-op "*")
                              (div-bin-op "/")
                              (and-bin-op "and")
                              (or-bin-op "or")
                              (xor-bin-op "xor")
                              (lshift-bin-op "lshift")
                              (rshift-bin-op "rshift")
                              (arshift-bin-op "arshift")))
     (pretty-print-expr stream left (+ cur-indent indent) indent)
     (format stream "~%")
     (pretty-print-expr stream right (+ cur-indent indent) indent)
     (format stream ")"))
    ((mem-expr value)
     (format stream "(mem~%")
     (pretty-print-expr stream value (+ cur-indent indent) indent)
     (format stream ")"))
    ((call-expr fun args)
     (format stream "(call~%")
     (pretty-print-expr stream fun (+ cur-indent indent) indent)
     (loop for (arg . rest-args) on args
           do (format stream "~%")
              (pretty-print-expr stream arg (+ cur-indent indent) indent)
              (unless (null rest-args)
                (format stream "~%")))
     (format stream ")"))
    ((stm-then-expr stm expr)
     (format stream "(stm-then-expr~%")
     (pretty-print-stm stream stm (+ cur-indent indent) indent)
     (format stream "~%")
     (pretty-print-expr stream expr (+ cur-indent indent) indent)
     (format stream ")"))))

(defun pretty-print-stm (stream stm cur-indent indent)
  (format stream "~v:<~>" cur-indent)
  (serapeum:match-of stm stm
    ((move-stm left right)
     (format stream "(move~%")
     (pretty-print-expr stream left (+ cur-indent indent) indent)
     (format stream "~%")
     (pretty-print-expr stream right (+ cur-indent indent) indent)
     (format stream ")"))
    ((expr-stm value)
     (format stream "(expr-stm~%")
     (pretty-print-expr stream value (+ cur-indent indent) indent)
     (format stream ")"))
    ((jump-stm target possible-labels)
     (format stream "(jump~%")
     (pretty-print-expr stream target (+ cur-indent indent) indent)
     (format stream "~%")
     (format stream "~v:<~>" (+ cur-indent indent))
     (format stream "[~%")
     (dolist (label possible-labels)
       (format stream "~v:<~>" (+ cur-indent indent indent))
       (format stream "~A~%" (temp:label-name label)))
     (format stream "~v:<~>" (+ cur-indent indent))
     (format stream "]")
     (format stream ")"))
    ((cjump-stm left op right true-target false-target)
     (format stream "(cjump~%")
     (pretty-print-expr stream left (+ cur-indent indent) indent)
     (format stream "~%")
     (format stream "~v:<~>" (+ cur-indent indent))
     (format stream "~A~%" (serapeum:match-of rel-op op
                             (eq-rel-op "=")
                             (neq-rel-op "!=")
                             (lt-rel-op "<")
                             (le-rel-op "<=")
                             (gt-rel-op ">")
                             (ge-rel-op ">=")
                             (ult-rel-op "u<")
                             (ule-rel-op "u<=")
                             (ugt-rel-op "u>")
                             (uge-rel-op "u>=")))
     (pretty-print-expr stream right (+ cur-indent indent) indent)
     (format stream "~%")
     (format stream "~v:<~>" (+ cur-indent indent))
     (format stream "~A" (temp:label-name true-target))
     (format stream "~%")
     (format stream "~v:<~>" (+ cur-indent indent))
     (format stream "~A" (temp:label-name false-target))
     (format stream ")"))
    ((compound-stm first second)
     (format stream "(compound-stm~%")
     (pretty-print-stm stream first (+ cur-indent indent) indent)
     (format stream "~%")
     (pretty-print-stm stream second (+ cur-indent indent) indent)
     (format stream ")"))
    ((label-stm name)
     (format stream "(label-stm ~A)" (temp:label-name name)))))

(defun pretty-print (stream ir indent)
  (trivia:ematch ir
    ((<expr>) (pretty-print-expr stream ir 0 indent))
    ((<stm>) (pretty-print-stm stream ir 0 indent))))
