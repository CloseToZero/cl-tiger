(cl:defpackage :cl-tiger/straight-line
  (:use :cl))

(cl:in-package :cl-tiger/straight-line)

(deftype id () 'string)

(serapeum:defunion op
  plus-op
  minus-op
  times-op
  div-op)

(serapeum:defunion stm
  (compound-stm
   (first stm)
   (rest stm))
  (assign-stm
   (id id)
   (expr expr))
  (print-stm
   ;; A list of expr.
   (exprs list)))

(serapeum:defunion expr
  (id-expr
   (id id))
  (num-expr
   (num integer))
  (op-expr
   (left expr)
   (op op)
   (right expr))
  (stm-expr
   (stm stm)
   (expr expr)))

(defvar *prog*
  (compound-stm
   (assign-stm "a" (op-expr (num-expr 5) plus-op (num-expr 3)))
   (compound-stm
    (assign-stm "b"
                (stm-expr
                 (print-stm
                  (list (id-expr "a")
                        (op-expr (id-expr "a") minus-op (num-expr 1))))
                 (op-expr (num-expr 10) times-op (id-expr "a"))))
    (print-stm (list (id-expr "b"))))))

(defun interpret-stm (stm env)
  ;; Returns another assoc list as new env.
  (serapeum:match-of stm stm
    ((compound-stm first rest)
     (let ((new-env (interpret-stm first env)))
       (interpret-stm rest new-env)))
    ((assign-stm id expr)
     (trivia:let-match (((list int-value new-env) (interpret-expr expr env)))
       (cons (cons id int-value) new-env)))
    ((print-stm exprs)
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
    ((id-expr id)
     (let ((id-value (assoc id env :test #'string=)))
       (unless id-value (error "Undefined variable: ~A, current environment: ~A." id env))
       (list (cdr id-value) env)))
    ((num-expr num)
     (list num env))
    ((op-expr left op right)
     (trivia:let-match* (((list int-value-1 new-env-1) (interpret-expr left env))
                         ((list int-value-2 new-env-2) (interpret-expr right new-env-1)))
       (list (funcall
              (serapeum:match-of op op
                  (plus-op #'+)
                  (minus-op #'-)
                  (times-op #'*)
                  (div-op #'/))
              int-value-1 int-value-2)
             new-env-2)))
    ((stm-expr stm expr)
     (let ((new-env (interpret-stm stm env)))
       (interpret-expr expr new-env)))))

;; (interpret-stm *prog* nil)
