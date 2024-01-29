;;; cl-tiger - The Tiger Programming Language implemented in Common Lisp

;; Copyright (c) 2023 Zhexuan Chen <2915234902@qq.com>

;; This file is part of cl-tiger.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(cl:defpackage :cl-tiger/straight-line
  (:use :cl))

(cl:in-package :cl-tiger/straight-line)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype id () 'string)

  (defclass stm ()
    ())

  (defclass compound-stm (stm)
    ((first-stm
      :type stm
      :initarg :first-stm
      :accessor compound-stm-first)
     (rest-stm
      :type stm
      :initarg :rest-stm
      :accessor compound-stm-rest)))

  (defclass expr ()
    ())

  (defclass assign-stm (stm)
    ((id
      :type id
      :initarg :id
      :accessor assign-stm-id)
     (expr
      :type expr
      :initarg :expr
      :accessor assign-stm-expr)))

  (defclass print-stm (stm)
    ((exprs
      :type list
      :initform nil
      :initarg :exprs
      :accessor print-stm-exprs)))

  (defclass id-expr (expr)
    ((id
      :type id
      :initarg :id
      :accessor id-expr-id)))

  (defclass num-expr (expr)
    ((num
      :type number
      :initarg :num
      :accessor num-expr-num)))

  (deftype bin-op () '(member :plus :minus :times :div))

  (defclass bin-op-expr (expr)
    ((left-expr
      :type expr
      :initarg :left-expr
      :accessor bin-op-expr-left)
     (right-expr
      :type expr
      :initarg :right-expr
      :accessor bin-op-expr-right)
     (op :type bin-op
       :initarg :op
       :accessor bin-op-expr-op)))

  (defclass stm-expr-expr (expr)
    ((stm
      :type stm
      :initarg :stm
      :accessor stm-expr-expr-stm)
     (expr
      :type expr
      :initarg :expr
      :accessor stm-expr-expr-expr))))

(defmethod print-object ((stm compound-stm) stream)
  (format stream "~A; ~A"
          (compound-stm-first stm)
          (compound-stm-rest stm)))

(defun make-compound-stm (first-stm rest-stm)
  (make-instance 'compound-stm
                 :first-stm first-stm
                 :rest-stm rest-stm))

(defmethod print-object ((stm assign-stm) stream)
  (format stream "~A = ~A"
          (assign-stm-id stm)
          (assign-stm-expr stm)))

(defun make-assign-stm (id expr)
  (make-instance 'assign-stm
                 :id id
                 :expr expr))

(defmethod print-object ((stm print-stm) stream)
  (format stream "print(~{~A~^, ~})" (print-stm-exprs stm)))

(defun make-print-stm (exprs)
  (make-instance 'print-stm :exprs exprs))

(defun make-print-stm* (&rest exprs)
  (make-print-stm exprs))

(defmethod print-object ((expr id-expr) stream)
  (format stream "expr(~A)" (id-expr-id expr)))

(defun make-id-expr (id)
  (make-instance 'id-expr :id id))

(defmethod print-object ((expr num-expr) stream)
  (format stream "expr(~A)" (num-expr-num expr)))

(defun make-num-expr (num)
  (make-instance 'num-expr :num num))

(defmethod print-object ((expr bin-op-expr) stream)
  (format stream "~A ~A ~A"
          (bin-op-expr-left expr)
          (ecase (bin-op-expr-op expr)
            (:plus "+")
            (:minus "-")
            (:times "*")
            (:div "/"))
          (bin-op-expr-right expr)))

(defun make-bin-op-expr (left-expr op right-expr)
  (make-instance 'bin-op-expr
                 :left-expr left-expr
                 :op op
                 :right-expr right-expr))

(defmethod print-object ((expr stm-expr-expr) stream)
  (format stream "(~A, ~A)"
          (stm-expr-expr-stm expr)
          (stm-expr-expr-expr expr)))

(defun make-stm-expr-expr (stm expr)
  (make-instance 'stm-expr-expr :stm stm :expr expr))

(defvar *prog*
  (make-compound-stm
   (make-assign-stm
    "a"
    (make-bin-op-expr (make-num-expr 5) :plus (make-num-expr 3)))
   (make-compound-stm
    (make-assign-stm
     "b"
     (make-stm-expr-expr
      (make-print-stm*
       (make-id-expr "a")
       (make-bin-op-expr (make-id-expr "a") :minus (make-num-expr 1)))
      (make-bin-op-expr (make-num-expr 10) :times (make-id-expr "a"))))
    (make-print-stm* (make-id-expr "b")))))

(defun interpret-stm (stm env)
  ;; Returns another assoc list as new env.
  (trivia:ematch stm
    ((compound-stm (compound-stm-first first-stm)
                   (compound-stm-rest rest-stm))
     (let ((new-env (interpret-stm first-stm env)))
       (interpret-stm rest-stm new-env)))
    ((assign-stm (assign-stm-id id)
                 (assign-stm-expr expr))
     (trivia:let-match1 (list int-value new-env) (interpret-expr expr env)
       (cons (cons id int-value) new-env)))
    ((print-stm (print-stm-exprs exprs))
     (let ((first-expr? t))
       (prog1
           (reduce (lambda (cur-env expr)
                     (trivia:let-match1 (list int-value new-env) (interpret-expr expr cur-env)
                       (format t (if first-expr? "~A" " ~A") int-value)
                       (setf first-expr? nil)
                       new-env))
                   exprs
                   :initial-value env)
         (fresh-line))))))

(defun interpret-expr (expr env)
  ;; Returns (int-return-value new-env)
  (trivia:ematch expr
    ((id-expr (id-expr-id id))
     (let ((id-value (assoc id env :test #'string=)))
       (unless id-value (error "Undefined variable: ~A, current environment: ~A." id env))
       (list (cdr id-value) env)))
    ((num-expr (num-expr-num num))
     (list num env))
    ((bin-op-expr (bin-op-expr-left left-expr)
                  (bin-op-expr-op op)
                  (bin-op-expr-right right-expr))
     (trivia:let-match* (((list int-value-1 new-env-1) (interpret-expr left-expr env))
                         ((list int-value-2 new-env-2) (interpret-expr right-expr new-env-1)))
       (list (funcall
              (ecase op
                (:plus #'+)
                (:minus #'-)
                (:times #'*)
                (:div #'/))
              int-value-1 int-value-2)
             new-env-2)))
    ((stm-expr-expr (stm-expr-expr-stm stm)
                    (stm-expr-expr-expr expr))
     (let ((new-env (interpret-stm stm env)))
       (interpret-expr expr new-env)))))

;; (interpret-stm *prog* nil)
