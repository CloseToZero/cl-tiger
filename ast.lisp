(cl:defpackage :cl-tiger/ast
  (:use :cl)
  (:local-nicknames (:symbol :cl-tiger/symbol))
  (:export
   #:pos

   #:op
   #:plus-op
   #:minus-op
   #:times-op
   #:div-op
   #:eq-op
   #:neq-op
   #:lt-op
   #:le-op
   #:gt-op
   #:ge-op

   #:escape-ref
   #:escape-ref-value

   #:field
   #:field-name
   #:field-type-id
   #:field-pos
   #:field-escape-ref

   #:ty
   #:name-ty
   #:name-ty-name
   #:name-ty-pos
   #:record-ty
   #:record-ty-fields
   #:array-ty
   #:array-ty-base-type-id
   #:array-ty-pos

   #:function-decl
   #:function-decl-name
   #:function-decl-params
   #:function-decl-result
   #:function-decl-body
   #:function-decl-pos

   #:type-decl
   #:type-decl-name
   #:type-decl-ty
   #:type-decl-pos

   #:decl
   #:function-decls
   #:function-decls-decls
   #:type-decls
   #:type-decls-decls
   #:var-decl
   #:var-decl-name
   #:var-decl-typ
   #:var-decl-init
   #:var-decl-pos
   #:var-decl-escape-ref

   #:var
   #:simple-var
   #:simple-var-sym
   #:simple-var-pos
   #:field-var
   #:field-var-var
   #:field-var-sym
   #:field-var-pos
   #:subscript-var
   #:subscript-var-var
   #:subscript-var-expr
   #:subscript-var-pos

   #:expr
   #:var-expr
   #:var-expr-var
   #:nil-expr
   #:int-expr
   #:int-expr-value
   #:string-expr
   #:string-expr-str
   #:string-expr-pos
   #:call-expr
   #:call-expr-fun
   #:call-expr-args
   #:call-expr-pos
   #:op-expr
   #:op-expr-left
   #:op-expr-op
   #:op-expr-right
   #:op-expr-pos
   #:record-expr
   #:record-expr-type-id
   #:record-expr-fields
   #:record-expr-pos
   #:seq-expr
   #:seq-expr-exprs
   #:assign-expr
   #:assign-expr-var
   #:assign-expr-expr
   #:assign-expr-pos
   #:if-expr
   #:if-expr-test
   #:if-expr-then
   #:if-expr-else
   #:if-expr-pos
   #:while-expr
   #:while-expr-test
   #:while-expr-body
   #:while-expr-pos
   #:for-expr
   #:for-expr-var
   #:for-expr-low
   #:for-expr-high
   #:for-expr-body
   #:for-expr-pos
   #:for-expr-escape-ref
   #:break-expr
   #:break-expr-pos
   #:array-expr
   #:array-expr-type-id
   #:array-expr-size
   #:array-expr-init
   #:array-expr-pos
   #:let-expr
   #:let-expr-decls
   #:let-expr-body
   #:let-expr-pos))

(cl:in-package :cl-tiger/ast)

(deftype pos () 'integer)

(serapeum:defunion op
  plus-op
  minus-op
  times-op
  div-op
  eq-op
  neq-op
  lt-op
  le-op
  gt-op
  ge-op)

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
  (name-ty
   (name symbol:sym)
   (pos pos))
  (record-ty
   ;; A list of field.
   (fields list))
  (array-ty
   (base-type-id symbol:sym)
   (pos pos)))

(serapeum:defconstructor function-decl
  (name symbol:sym)
  ;; A list of field.
  (params list)
  ;; A list likes (sym pos) or nil.
  (result (or list null))
  (body expr)
  (pos pos))

(serapeum:defconstructor type-decl
  (name symbol:sym)
  (ty ty)
  (pos pos))

(serapeum:defunion decl
  (function-decls
   ;; A list of one or more function-decl.
   (decls list))
  (type-decls
   ;; A list of one or more type-decl.
   (decls list))
  (var-decl
   (name symbol:sym)
   ;; A list of (sym pos) or nil.
   (typ (or list null))
   (init expr)
   (pos pos)
   (escape-ref escape-ref)))

(serapeum:defunion var
  (simple-var
   (sym symbol:sym)
   (pos pos))
  (field-var
   (var var)
   (sym symbol:sym)
   (pos pos))
  (subscript-var
   (var var)
   (expr expr)
   (pos pos)))

(serapeum:defunion expr
  (var-expr
   (var var))
  nil-expr
  (int-expr
   (value fixnum))
  (string-expr
   (str string)
   (pos pos))
  (call-expr
   (fun symbol:sym)
   ;; A list of expr.
   (args list)
   (pos pos))
  (op-expr
   (left expr)
   (op op)
   (right expr)
   (pos pos))
  (record-expr
   (type-id symbol:sym)
   ;; A list of (sym expr pos).
   (fields list)
   (pos pos))
  (seq-expr
   ;; A list of (expr pos).
   (exprs list))
  (assign-expr
   (var var)
   (expr expr)
   (pos pos))
  (if-expr
   (test expr)
   (then expr)
   (else (or expr null))
   (pos pos))
  (while-expr
   (test expr)
   (body expr)
   (pos pos))
  (for-expr
   (var symbol:sym)
   (low expr)
   (high expr)
   (body expr)
   (pos pos)
   (escape-ref escape-ref))
  (break-expr
   (pos pos))
  (array-expr
   (type-id symbol:sym)
   (size expr)
   (init expr)
   (pos pos))
  (let-expr
   ;; A list of decl.
   (decls list)
   (body expr)
   (pos pos)))
