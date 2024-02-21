(cl:defpackage :cl-tiger/ir
  (:use :cl)
  (:local-nicknames
   (:temp :cl-tiger/temp))
  (:export
   #:bin-op
   #:bin-op-plus
   #:bin-op-minus
   #:bin-op-times
   #:bin-op-div
   #:bin-op-and
   #:bin-op-or
   #:bin-op-xor
   #:bin-op-lshift
   #:bin-op-rshift
   #:bin-op-arshift

   #:rel-op
   #:rel-op-eq
   #:rel-op-neq
   #:rel-op-lt
   #:rel-op-le
   #:rel-op-gt
   #:rel-op-ge
   #:rel-op-ult
   #:rel-op-ule
   #:rel-op-ugt
   #:rel-op-uge

   #:expr
   #:expr-int
   #:expr-int-value
   #:expr-label
   #:expr-label-value
   #:expr-temp
   #:expr-temp-value
   #:expr-bin-op
   #:expr-bin-op-left
   #:expr-bin-op-op
   #:expr-bin-op-right
   #:expr-mem
   #:expr-mem-value
   #:expr-call
   #:expr-call-fun
   #:expr-call-args
   #:expr-stm-then-expr
   #:expr-stm-then-expr-stm
   #:expr-stm-then-expr-expr

   #:stm
   #:stm-move
   #:stm-move-left
   #:stm-move-right
   #:stm-expr
   #:stm-expr-value
   #:stm-jump
   #:stm-jump-target
   #:stm-jump-possible-labels
   #:stm-cjump
   #:stm-cjump-left
   #:stm-cjump-op
   #:stm-cjump-right
   #:stm-cjump-true-target
   #:stm-cjump-false-target
   #:stm-compound
   #:stm-compound-first
   #:stm-compound-second
   #:stm-label
   #:stm-label-name

   #:not-rel-op

   #:stms->stm-compound

   #:pretty-print))

(cl:in-package :cl-tiger/ir)

(serapeum:defunion bin-op
  bin-op-plus
  bin-op-minus
  bin-op-times
  bin-op-div
  bin-op-and
  bin-op-or
  bin-op-xor
  bin-op-lshift
  bin-op-rshift
  bin-op-arshift)

(serapeum:defunion rel-op
  rel-op-eq
  rel-op-neq
  rel-op-lt
  rel-op-le
  rel-op-gt
  rel-op-ge
  rel-op-ult
  rel-op-ule
  rel-op-ugt
  rel-op-uge)

(serapeum:defunion expr
  (expr-int
   (value fixnum))
  (expr-label
   (value temp:label))
  (expr-temp
   (value temp:temp))
  (expr-bin-op
   (left expr)
   (op bin-op)
   (right expr))
  (expr-mem
   (value expr))
  (expr-call
   (fun expr)
   ;; A list of expr.
   (args list))
  (expr-stm-then-expr
   (stm stm)
   (expr expr)))

(serapeum:defunion stm
  (stm-move
   (left expr)
   (right expr))
  (stm-expr
   (value expr))
  (stm-jump
   (target expr)
   ;; A list of temp:label.
   (possible-labels list))
  (stm-cjump
   (left expr)
   (op rel-op)
   (right expr)
   (true-target temp:label)
   (false-target temp:label))
  (stm-compound
   (first stm)
   (second stm))
  (stm-label
   (name temp:label)))

(defun not-rel-op (rel-op)
  (serapeum:match-of rel-op rel-op
    (rel-op-eq rel-op-neq)
    (rel-op-neq rel-op-eq)
    (rel-op-lt rel-op-ge)
    (rel-op-le rel-op-gt)
    (rel-op-gt rel-op-le)
    (rel-op-ge rel-op-lt)
    (rel-op-ult rel-op-uge)
    (rel-op-ule rel-op-ugt)
    (rel-op-ugt rel-op-ule)
    (rel-op-uge rel-op-ult)))

(defun stms->stm-compound (&rest stms)
  (cond ((null stms)
         (stm-expr (expr-int 0)))
        ((null (cdr stms))
         ;; A list of length 1.
         (car stms))
        (t
         (stm-compound
          (car stms) (apply #'stms->stm-compound (rest stms))))))

(defun pretty-print-expr (stream expr cur-indent indent)
  (format stream "~v:<~>" cur-indent)
  (serapeum:match-of expr expr
    ((expr-int value)
     (format stream "(int ~A)" value))
    ((expr-label value)
     (format stream "(expr-label ~A)" (temp:label-name value)))
    ((expr-temp value)
     (format stream "(temp ~A)" (temp:temp-name value)))
    ((expr-bin-op left op right)
     (format stream "(~A~%" (serapeum:match-of bin-op op
                              (bin-op-plus "+")
                              (bin-op-minus "-")
                              (bin-op-times "*")
                              (bin-op-div "/")
                              (bin-op-and "and")
                              (bin-op-or "or")
                              (bin-op-xor "xor")
                              (bin-op-lshift "lshift")
                              (bin-op-rshift "rshift")
                              (bin-op-arshift "arshift")))
     (pretty-print-expr stream left (+ cur-indent indent) indent)
     (format stream "~%")
     (pretty-print-expr stream right (+ cur-indent indent) indent)
     (format stream ")"))
    ((expr-mem value)
     (format stream "(mem~%")
     (pretty-print-expr stream value (+ cur-indent indent) indent)
     (format stream ")"))
    ((expr-call fun args)
     (format stream "(call~%")
     (pretty-print-expr stream fun (+ cur-indent indent) indent)
     (loop for (arg . rest-args) on args
           do (format stream "~%")
              (pretty-print-expr stream arg (+ cur-indent indent) indent)
              (unless (null rest-args)
                (format stream "~%")))
     (format stream ")"))
    ((expr-stm-then-expr stm expr)
     (format stream "(stm-then-expr~%")
     (pretty-print-stm stream stm (+ cur-indent indent) indent)
     (format stream "~%")
     (pretty-print-expr stream expr (+ cur-indent indent) indent)
     (format stream ")"))))

(defun pretty-print-stm (stream stm cur-indent indent)
  (format stream "~v:<~>" cur-indent)
  (serapeum:match-of stm stm
    ((stm-move left right)
     (format stream "(move~%")
     (pretty-print-expr stream left (+ cur-indent indent) indent)
     (format stream "~%")
     (pretty-print-expr stream right (+ cur-indent indent) indent)
     (format stream ")"))
    ((stm-expr value)
     (format stream "(stm-expr~%")
     (pretty-print-expr stream value (+ cur-indent indent) indent)
     (format stream ")"))
    ((stm-jump target possible-labels)
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
    ((stm-cjump left op right true-target false-target)
     (format stream "(cjump~%")
     (pretty-print-expr stream left (+ cur-indent indent) indent)
     (format stream "~%")
     (format stream "~v:<~>" (+ cur-indent indent))
     (format stream "~A~%" (serapeum:match-of rel-op op
                             (rel-op-eq "=")
                             (rel-op-neq "!=")
                             (rel-op-lt "<")
                             (rel-op-le "<=")
                             (rel-op-gt ">")
                             (rel-op-ge ">=")
                             (rel-op-ult "u<")
                             (rel-op-ule "u<=")
                             (rel-op-ugt "u>")
                             (rel-op-uge "u>=")))
     (pretty-print-expr stream right (+ cur-indent indent) indent)
     (format stream "~%")
     (format stream "~v:<~>" (+ cur-indent indent))
     (format stream "~A" (temp:label-name true-target))
     (format stream "~%")
     (format stream "~v:<~>" (+ cur-indent indent))
     (format stream "~A" (temp:label-name false-target))
     (format stream ")"))
    ((stm-compound first second)
     (format stream "(compound~%")
     (pretty-print-stm stream first (+ cur-indent indent) indent)
     (format stream "~%")
     (pretty-print-stm stream second (+ cur-indent indent) indent)
     (format stream ")"))
    ((stm-label name)
     (format stream "(stm-label ~A)" (temp:label-name name)))))

(defun pretty-print (stream ir indent)
  (trivia:ematch ir
    ((<expr>) (pretty-print-expr stream ir 0 indent))
    ((<stm>) (pretty-print-stm stream ir 0 indent))))
