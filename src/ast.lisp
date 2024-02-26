(cl:defpackage :cl-tiger/ast
  (:use :cl)
  (:local-nicknames (:symbol :cl-tiger/symbol))
  (:export
   #:pos

   #:op
   #:op-plus
   #:op-minus
   #:op-times
   #:op-div
   #:op-eq
   #:op-neq
   #:op-lt
   #:op-le
   #:op-gt
   #:op-ge

   #:escape-ref
   #:escape-ref-value

   #:field
   #:field-name
   #:field-type-id
   #:field-pos
   #:field-escape-ref

   #:ty
   #:ty-name
   #:ty-name-name
   #:ty-name-pos
   #:ty-record
   #:ty-record-fields
   #:ty-array
   #:ty-array-base-type-id
   #:ty-array-pos

   #:decl-function
   #:decl-function-name
   #:decl-function-params
   #:decl-function-result
   #:decl-function-body
   #:decl-function-pos

   #:decl-type
   #:decl-type-name
   #:decl-type-ty
   #:decl-type-pos

   #:decl
   #:decl-functions
   #:decl-functions-decls
   #:decl-types
   #:decl-types-decls
   #:decl-var
   #:decl-var-name
   #:decl-var-typ
   #:decl-var-init
   #:decl-var-pos
   #:decl-var-escape-ref

   #:var
   #:var-simple
   #:var-simple-sym
   #:var-simple-pos
   #:var-field
   #:var-field-var
   #:var-field-sym
   #:var-field-pos
   #:var-subscript
   #:var-subscript-var
   #:var-subscript-expr
   #:var-subscript-pos

   #:expr
   #:expr-var
   #:expr-var-var
   #:expr-nil
   #:expr-int
   #:expr-int-value
   #:expr-string
   #:expr-string-str
   #:expr-string-pos
   #:expr-call
   #:expr-call-fun
   #:expr-call-args
   #:expr-call-pos
   #:expr-op
   #:expr-op-left
   #:expr-op-op
   #:expr-op-right
   #:expr-op-pos
   #:expr-record
   #:expr-record-type-id
   #:expr-record-fields
   #:expr-record-pos
   #:expr-seq
   #:expr-seq-exprs
   #:expr-assign
   #:expr-assign-var
   #:expr-assign-expr
   #:expr-assign-pos
   #:expr-if
   #:expr-if-test
   #:expr-if-then
   #:expr-if-else
   #:expr-if-pos
   #:expr-while
   #:expr-while-test
   #:expr-while-body
   #:expr-while-pos
   #:expr-for
   #:expr-for-var
   #:expr-for-low
   #:expr-for-high
   #:expr-for-body
   #:expr-for-pos
   #:expr-for-escape-ref
   #:expr-break
   #:expr-break-pos
   #:expr-array
   #:expr-array-type-id
   #:expr-array-size
   #:expr-array-init
   #:expr-array-pos
   #:expr-let
   #:expr-let-decls
   #:expr-let-body
   #:expr-let-pos

   #:op->string))

(cl:in-package :cl-tiger/ast)

(deftype pos () 'integer)

(serapeum:defunion op
  op-plus
  op-minus
  op-times
  op-div
  op-eq
  op-neq
  op-lt
  op-le
  op-gt
  op-ge)

;; escape reference type,
;; so we can have mutable data inside immutable data.
(defstruct (escape-ref
            (:conc-name escape-ref-)
            (:constructor escape-ref (value)))
  (value t :type boolean))

(serapeum:defconstructor field
  (name symbol:sym)
  (type-id symbol:sym)
  (pos pos)
  (escape-ref escape-ref))

(serapeum:defunion ty
  (ty-name
   (name symbol:sym)
   (pos pos))
  (ty-record
   ;; A list of field.
   ;; In this case, the escape-ref of a field is useless.
   (fields list))
  (ty-array
   (base-type-id symbol:sym)
   (pos pos)))

(serapeum:defconstructor decl-function
  (name symbol:sym)
  ;; A list of field.
  (params list)
  ;; A list likes (sym pos) or nil.
  (result (or list null))
  (body expr)
  (pos pos))

(serapeum:defconstructor decl-type
  (name symbol:sym)
  (ty ty)
  (pos pos))

(serapeum:defunion decl
  (decl-functions
   ;; A list of one or more decl-function.
   (decls list))
  (decl-types
   ;; A list of one or more decl-type.
   (decls list))
  (decl-var
   (name symbol:sym)
   ;; A list of (sym pos) or nil.
   (typ (or list null))
   (init expr)
   (pos pos)
   (escape-ref escape-ref)))

(serapeum:defunion var
  (var-simple
   (sym symbol:sym)
   (pos pos))
  (var-field
   (var var)
   (sym symbol:sym)
   (pos pos))
  (var-subscript
   (var var)
   (expr expr)
   (pos pos)))

(serapeum:defunion expr
  (expr-var
   (var var))
  expr-nil
  (expr-int
   (value fixnum))
  (expr-string
   (str string)
   (pos pos))
  (expr-call
   (fun symbol:sym)
   ;; A list of expr.
   (args list)
   (pos pos))
  (expr-op
   (left expr)
   (op op)
   (right expr)
   (pos pos))
  (expr-record
   (type-id symbol:sym)
   ;; A list of (sym expr pos).
   (fields list)
   (pos pos))
  (expr-seq
   ;; A list of (expr pos).
   (exprs list))
  (expr-assign
   (var var)
   (expr expr)
   (pos pos))
  (expr-if
   (test expr)
   (then expr)
   (else (or expr null))
   (pos pos))
  (expr-while
   (test expr)
   (body expr)
   (pos pos))
  (expr-for
   (var symbol:sym)
   (low expr)
   (high expr)
   (body expr)
   (pos pos)
   (escape-ref escape-ref))
  (expr-break
   (pos pos))
  (expr-array
   (type-id symbol:sym)
   (size expr)
   (init expr)
   (pos pos))
  (expr-let
   ;; A list of decl.
   (decls list)
   (body expr)
   (pos pos)))

(defun op->string (op)
  (serapeum:match-of op op
    (op-plus "+")
    (op-minus "-")
    (op-times "*")
    (op-div "/")
    (op-eq "=")
    (op-neq "<>")
    (op-lt "<")
    (op-le "<=")
    (op-gt ">")
    (op-ge ">=")))
