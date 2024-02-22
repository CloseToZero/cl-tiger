(cl:defpackage :cl-tiger/straight-line
  (:use :cl))

(cl:in-package :cl-tiger/straight-line)

(deftype id () 'string)

(serapeum:defunion op
  op-plus
  op-minus
  op-times
  op-div)

(serapeum:defunion stm
  (stm-compound
   (first stm)
   (second stm))
  (stm-assign
   (id id)
   (expr expr))
  (stm-print
   ;; A list of expr.
   (exprs list)))

(serapeum:defunion expr
  (expr-id
   (id id))
  (expr-int
   (value integer))
  (expr-op
   (left expr)
   (op op)
   (right expr))
  (expr-stm-then-expr
   (stm stm)
   (expr expr)))

(defvar *prog*
  (stm-compound
   (stm-assign "a" (expr-op (expr-int 5) op-plus (expr-int 3)))
   (stm-compound
    (stm-assign "b"
                (expr-stm-then-expr
                 (stm-print
                  (list (expr-id "a")
                        (expr-op (expr-id "a") op-minus (expr-int 1))))
                 (expr-op (expr-int 10) op-times (expr-id "a"))))
    (stm-print (list (expr-id "b"))))))

(defun interpret-stm (stm env)
  ;; Returns another assoc list as new env.
  (serapeum:match-of stm stm
    ((stm-compound first second)
     (let ((new-env (interpret-stm first env)))
       (interpret-stm second new-env)))
    ((stm-assign id expr)
     (trivia:let-match (((list int-value new-env) (interpret-expr expr env)))
       (cons (cons id int-value) new-env)))
    ((stm-print exprs)
     (let ((first-expr? t))
       (prog1
           (reduce (lambda (cur-env expr)
                     (trivia:let-match (((list int-value new-env) (interpret-expr expr cur-env)))
                       (format t (if  first-expr? "~A" " ~A") int-value)
                       (setf first-expr? nil)
                       new-env))
                   exprs
                   :initial-value env)
         (fresh-line))))))

(defun interpret-expr (expr env)
  ;; Returns (int-return-value new-env)
  (serapeum:match-of expr expr
    ((expr-id id)
     (let ((id-value (assoc id env :test #'string=)))
       (unless id-value (error "Undefined variable: ~A, current environment: ~A." id env))
       (list (cdr id-value) env)))
    ((expr-int value)
     (list value env))
    ((expr-op left op right)
     (trivia:let-match* (((list int-value-1 new-env-1) (interpret-expr left env))
                         ((list int-value-2 new-env-2) (interpret-expr right new-env-1)))
       (list (funcall
              (serapeum:match-of op op
                  (op-plus #'+)
                  (op-minus #'-)
                  (op-times #'*)
                  (op-div #'/))
              int-value-1 int-value-2)
             new-env-2)))
    ((expr-stm-then-expr stm expr)
     (let ((new-env (interpret-stm stm env)))
       (interpret-expr expr new-env)))))

;; (interpret-stm *prog* nil)
