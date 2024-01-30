(cl:defpackage :cl-tiger/translate
  (:use :cl)
  (:local-nicknames
   (:ast :cl-tiger/ast)
   (:temp :cl-tiger/temp)
   (:ir :cl-tiger/ir)
   (:frame :cl-tiger/frame)
   (:cl-ds :cl-data-structures)
   (:cl-ds.hamt :cl-data-structures.dicts.hamt))
  (:export
   #:find-and-fill-escapes

   #:level
   #:top-level
   #:inner-level
   #:inner-level-parent
   #:inner-level-name
   #:inner-level-frame

   #:access
   #:access-level
   #:access-frame-access

   #:new-level
   #:level-formals
   #:alloc-local))

(cl:in-package :cl-tiger/translate)

;; A mapping from symbol:sym to (depth ast:escape-ref)
(defvar *base-escape-ref-env*
  (cl-ds.hamt:make-functional-hamt-dictionary #'sxhash #'eq))

(defun insert-escape-ref (escape-ref-env sym depth escape-ref)
  (cl-ds:insert escape-ref-env sym (list depth escape-ref)))

(defun get-escape-ref (escape-ref-env sym)
  (cl-ds:at escape-ref-env sym))

;; prog is the expression of the whole program with the type
;; of ast:expr.
(defun find-and-fill-escapes (prog)
  (find-and-fill-escapes-expr *base-escape-ref-env* 0 prog)
  (values))

(defun find-and-fill-escapes-expr (escape-ref-env depth expr)
  (serapeum:match-of ast:expr expr
    ((ast:var-expr var)
     (find-and-fill-escapes-var escape-ref-env depth var))
    ((ast:nil-expr) nil)
    ((ast:int-expr _) nil)
    ((ast:string-expr _ _) nil)
    ((ast:call-expr _ args _)
     (mapc (lambda (arg)
             (find-and-fill-escapes-expr escape-ref-env depth arg))
           args))
    ((ast:op-expr left _ right _)
     (find-and-fill-escapes-expr escape-ref-env depth left)
     (find-and-fill-escapes-expr escape-ref-env depth right))
    ((ast:record-expr _ fields _)
     (mapc (lambda (field)
             ;; form: (sym expr pos).
             (find-and-fill-escapes-expr escape-ref-env depth (second field)))
           fields))
    ((ast:seq-expr exprs)
     (mapc (lambda (expr-with-pos)
               ;; expr-with-pos form: (expr pos).
               (find-and-fill-escapes-expr escape-ref-env depth (first expr-with-pos)))
             exprs))
    ((ast:assign-expr _ expr _)
     (find-and-fill-escapes-expr escape-ref-env depth expr))
    ((ast:if-expr test then else _)
     (find-and-fill-escapes-expr escape-ref-env depth test)
     (find-and-fill-escapes-expr escape-ref-env depth then)
     (when else (find-and-fill-escapes-expr escape-ref-env depth else)))
    ((ast:while-expr test body _)
     (find-and-fill-escapes-expr escape-ref-env depth test)
     (find-and-fill-escapes-expr escape-ref-env depth body))
    ((ast:for-expr var low high body _ escape-ref)
     (find-and-fill-escapes-expr escape-ref-env depth low)
     (find-and-fill-escapes-expr escape-ref-env depth high)
     (find-and-fill-escapes-expr (insert-escape-ref
                                 escape-ref-env var depth
                                 (progn (setf (ast:escape-ref-value escape-ref) nil)
                                        escape-ref))
                                depth body))
    ((ast:break-expr _)
     nil)
    ((ast:array-expr _ size init _)
     (find-and-fill-escapes-expr escape-ref-env depth size)
     (find-and-fill-escapes-expr escape-ref-env depth init))
    ((ast:let-expr decls body _)
     (let ((new-escape-ref-env (find-and-fill-escapes-decls escape-ref-env depth decls)))
       (find-and-fill-escapes-expr new-escape-ref-env depth body)))))

(defun find-and-fill-escapes-var (escape-ref-env depth var)
  (serapeum:match-of ast:var var
    ((ast:simple-var sym _)
     (trivia:let-match1 (list escape-ref-depth escape-ref) (get-escape-ref escape-ref-env sym)
       (when (> depth escape-ref-depth)
         (setf (ast:escape-ref-value escape-ref) t)))
     nil)
    ((ast:field-var var _ _)
     (find-and-fill-escapes-var escape-ref-env depth var))
    ((ast:subscript-var var expr _)
     (find-and-fill-escapes-expr escape-ref-env depth expr)
     (find-and-fill-escapes-var escape-ref-env depth var))))

(defun find-and-fill-escapes-decl (escape-ref-env depth decl)
  (serapeum:match-of ast:decl decl
    ((ast:var-decl name _ init _ escape-ref)
     (find-and-fill-escapes-expr escape-ref-env depth init)
     (insert-escape-ref escape-ref-env name depth
                        (progn (setf (ast:escape-ref-value escape-ref) nil)
                               escape-ref)))
    ((ast:type-decls _) escape-ref-env)
    ((ast:function-decls function-decls)
     (mapc (lambda (function-decl)
             (find-and-fill-escapes-expr
              (loop with acc-escape-ref-env = escape-ref-env
                    for param-field in (ast:function-decl-params function-decl)
                    do (setf acc-escape-ref-env
                             (insert-escape-ref acc-escape-ref-env
                                                (ast:field-name param-field)
                                                (+ depth 1)
                                                (let ((escape-ref (ast:field-escape-ref param-field)))
                                                  (setf (ast:escape-ref-value escape-ref) nil)
                                                  escape-ref)))
                    finally (return acc-escape-ref-env))
              (+ depth 1) (ast:function-decl-body function-decl)))
           function-decls)
     escape-ref-env)))

(defun find-and-fill-escapes-decls (escape-ref-env depth decls)
  (reduce (lambda (acc-escape-ref-env decl)
            (find-and-fill-escapes-decl acc-escape-ref-env depth decl))
          decls
          :initial-value escape-ref-env))

(serapeum:defunion level
  top-level
  (inner-level
   (parent level)
   (name temp:label)
   (frame frame:frame)))

(serapeum:defconstructor access
  (level level)
  (frame-access frame:access))

(serapeum:defunion tagged-ir
  (expr
   (value ir:expr))
  (stm
   (value ir:stm))
  (condi
   (value (function (temp:label temp:label) ir:stm))))

(defun new-level (parent name formals target)
  (let* ((frame (frame:new-frame name (cons t formals) target))
         (level (inner-level parent name frame)))
    level))

(defun level-formals (level)
  (rest (mapcar (lambda (frame-access)
                  (access level frame-access))
                (frame:frame-formals (inner-level-frame level)))))

(defun alloc-local (level escape target)
  (serapeum:match-of level level
    (top-level (error "Cannot alloc a local variable in the top level."))
    ((inner-level _ _ frame)
     (access level (frame:alloc-local frame escape target)))))
