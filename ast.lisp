(cl:defpackage :cl-tiger/ast
  (:use :cl)
  (:local-nicknames (:symbol :cl-tiger/symbol))
  (:export

   #:pos

   #:field
   #:make-field

   #:ty

   #:name-ty
   #:make-name-ty

   #:record-ty
   #:make-record-ty

   #:array-ty
   #:make-array-ty

   #:decl

   #:function-decl
   #:make-function-decl

   #:type-decl
   #:make-type-decl

   #:function-decls
   #:make-function-decls

   #:type-decls
   #:make-type-decls

   #:var-decl
   #:make-var-decl

   #:var

   #:simple-var
   #:make-simple-var

   #:field-var
   #:make-field-var

   #:subscript-var
   #:make-subscript-var

   #:expr

   #:var-expr
   #:make-var-expr

   #:nil-expr
   #:make-nil-expr

   #:int-expr
   #:make-int-expr

   #:string-expr
   #:make-string-expr

   #:call-expr
   #:make-call-expr

   #:op

   #:op-expr
   #:make-op-expr

   #:record-expr
   #:make-record-expr

   #:seq-expr
   #:make-seq-expr

   #:assign-expr
   #:make-assign-expr

   #:if-expr
   #:make-if-expr

   #:while-expr
   #:make-while-expr

   #:for-expr
   #:make-for-expr

   #:break-expr
   #:make-break-expr

   #:let-expr
   #:make-let-expr

   #:array-expr
   #:make-array-expr))

(cl:in-package :cl-tiger/ast)

(deftype pos () 'integer)

(defclass var ()
  ())

(defclass expr ()
  ())

(defclass simple-var (var)
  ((sym
    :type symbol:sym
    :initform (error "Must supply the symbol of the simple-var.")
    :initarg :sym
    :accessor simple-var-sym)
   (pos
    :type pos
    :initform (error "Must supply the position of the simple-var.")
    :initarg :pos
    :accessor simple-var-pos)))

(defun make-simple-var (sym pos)
  (make-instance 'simple-var :sym sym :pos pos))

(defclass field-var (var)
  ((var
    :type var
    :initform (error "Must supply the var of the field-var.")
    :initarg :var
    :accessor field-var-var)
   (sym
    :type symbol:sym
    :initform (error "Must supply the symbol of the field-var.")
    :initarg :sym
    :accessor field-var-sym)
   (pos
    :type pos
    :initform (error "Must supply the position of the field-var.")
    :initarg :pos
    :accessor field-var-pos)))

(defun make-field-var (var sym pos)
  (make-instance 'field-var :var var :sym sym :pos pos))

(defclass subscript-var (var)
  ((var
    :type var
    :initform (error "Must supply the var of the subscript-var.")
    :initarg :var
    :accessor subscript-var-var)
   (expr
    :type expr
    :initform (error "Must supply the expression of the subscript-var.")
    :initarg :expr
    :accessor subscript-var-expr)
   (pos
    :type pos
    :initform (error "Must supply the position of the subscript-var.")
    :initarg :pos
    :accessor subscript-var-pos)))

(defun make-subscript-var (var expr pos)
  (make-instance 'subscript-var :var var :expr expr :pos pos))

(defclass var-expr (expr)
  ((var
    :type var
    :initform (error "Must supply the var of the var-expr.")
    :initarg :var
    :accessor var-expr-var)))

(defun make-var-expr (var)
  (make-instance 'var-expr :var var))

(defclass nil-expr (expr)
  ())

(defun make-nil-expr ()
  (make-instance 'nil-expr))

(defclass int-expr (expr)
  ((value
    :type fixnum
    :initform (error "Must supply the value of the int-expr")
    :initarg :value
    :accessor init-expr-value)))

(defun make-int-expr (value)
  (make-instance 'int-expr :value value))

(defclass string-expr (expr)
  ((str
    :type string
    :initform (error "Must supply the string of the string-expr.")
    :initarg :str
    :accessor string-expr-str)
   (pos
    :type pos
    :initform (error "Must supply the position of the string-expr.")
    :initarg :pos
    :accessor string-expr-pos)))

(defun make-string-expr (str pos)
  (make-instance 'string-expr :str str :pos pos))

(defclass call-expr (expr)
  ((fun
    :type symbol:sym
    :initform (error "Must supply the function of the call-expr.")
    :initarg :fun
    :accessor call-expr-fun)
   (args
    ;; A list of expressions.
    :type list
    :initform (error "Must supply the arguments of the call-expr.")
    :initarg :args
    :accessor call-expr-args)
   (pos
    :type pos
    :initform (error "Must supply the position of the call-expr.")
    :initarg :pos
    :accessor call-expr-pos)))

(defun make-call-expr (fun args pos)
  (make-instance 'call-expr :fun fun :args args :pos pos))

(deftype op ()
  '(member :plus :minus :times :div :eq :neq :lt :le :gt :ge))

(defclass op-expr (expr)
  ((left
    :type expr
    :initform (error "Must supply the left expression of the op-expr.")
    :initarg :left
    :accessor op-expr-left)
   (op
    :type op
    :initform (error "Must supply the operator of the op-expr.")
    :initarg :op
    :accessor op-expr-op)
   (right
    :type expr
    :initform (error "Must supply the right expression of the op-expr.")
    :initarg :right
    :accessor op-expr-right)))

(defun make-op-expr (left op right)
  (make-instance 'op-expr :left left :op op :right right))

(defclass record-expr (expr)
  ((fields
    ;; A list of (sym expr pos).
    :type list
    :initform (error "Must supply the record fields of the record-expr.")
    :initarg :fields
    :accessor record-expr-fields)
   (type-id
    :type symbol:sym
    :initform (error "Must supply the type-id of the record-expr.")
    :initarg :type-id
    :accessor record-expr-type-id)
   (pos
    :type pos
    :initform (error "Must supply the position of the record-expr.")
    :initarg :pos
    :accessor record-expr-pos)))

(defun make-record-expr (fields type-id pos)
  (make-instance 'record-expr :fields fields :type-id type-id :pos pos))

(defclass seq-expr (expr)
  ((exprs
    ;; A list of (expr pos).
    :type list
    :initform (error "Must supply the expressions of the seq-expr.")
    :initarg :exprs
    :accessor seq-expr-exprs)))

(defun make-seq-expr (exprs)
  (make-instance 'seq-expr :exprs exprs))

(defclass assign-expr (expr)
  ((var
    :type var
    :initform (error "Must supply the var of the assign-expr.")
    :initarg :var
    :accessor assign-expr-var)
   (expr
    :type expr
    :initform (error "Must supply the expression of the assign-expr.")
    :initarg :expr
    :accessor assign-expr-expr)
   (pos
    :type pos
    :initform (error "Must supply the position of the assign-expr.")
    :initarg :pos
    :accessor assign-expr-pos)))

(defun make-assign-expr (var expr pos)
  (make-instance 'assign-expr :var var :expr expr :pos pos))

(defclass if-expr (expr)
  ((test
    :type expr
    :initform (error "Must supply the test expression of the if-expr.")
    :initarg :test
    :accessor if-expr-test)
   (then
    :type expr
    :initform (error "Must supply the then expression of the if-expr.")
    :initarg :then
    :accessor if-expr-then)
   (else
    :type (or expr null)
    :initform nil
    :initarg :then
    :accessor if-expr-else)
   (pos
    :type pos
    :initform (error "Must supply the position of the if-expr.")
    :initarg :pos
    :accessor if-expr-pos)))

;; The argument else can be nil.
(defun make-if-expr (test then else pos)
  (make-instance 'if-expr :test test :then then :else else :pos pos))

(defclass while-expr (expr)
  ((test
    :type expr
    :initform (error "Must supply the test expression of the while-expr.")
    :initarg :test
    :accessor while-expr-test)
   (body
    :type expr
    :initform (error "Must supply the body expression of the while-expr.")
    :initarg :body
    :accessor while-expr-body)
   (pos
    :type pos
    :initform (error "Must supply the position of the while-expr.")
    :initarg :pos
    :accessor while-expr-pos)))

(defun make-while-expr (test body pos)
  (make-instance 'while-expr :test test :body body :pos pos))

(defclass for-expr (expr)
  ((var
    :type symbol:sym
    :initform (error "Must supply the var of the for-expr.")
    :initarg :var
    :accessor for-expr-var)
   (low
    :type expr
    :initform (error "Must supply the low expression of the for-expr.")
    :initarg :low
    :accessor for-expr-low)
   (high
    :type expr
    :initform (error "Must supply the high expression of the for-expr.")
    :initarg :high
    :accessor for-expr-high)
   (body
    :type expr
    :initform (error "Must supply the body expression of the for-expr.")
    :initarg :body
    :accessor for-expr-body)
   (pos
    :type pos
    :initform (error "Must supply the position of the for-expr.")
    :initarg :pos
    :accessor for-expr-pos)
   (escape
    :type boolean
    :initform t
    :initarg :escape
    :accessor for-expr-escape)))

(defun make-for-expr (var low high body pos &optional (escape t))
  (make-instance 'for-expr :var var :low low :high high :body body :pos pos :escape escape))

(defclass break-expr (expr)
  ((pos
    :type pos
    :initform (error "Must supply the position of the break-expr.")
    :initarg :pos
    :accessor break-expr-pos)))

(defun make-break-expr (pos)
  (make-instance 'break-expr :pos pos))

(defclass field ()
  ((name
    :type symbol:sym
    :initform (error "Must supply the name of the field.")
    :initarg :name
    :accessor field-name)
   (type-id
    :type symbol:sym
    :initform (error "Must supply the type-id of the field.")
    :initarg :type-id
    :accessor field-type-id)
   (pos
    :type pos
    :initform (error "Must supply the position of the field.")
    :initarg :pos
    :accessor field-pos)
   (escape
    :type boolean
    :initform t
    :initarg :escape
    :accessor field-type-escape)))

(defun make-field (name type-id pos &optional (escape t))
  (make-instance 'field :name name :type-id type-id :pos pos :escape escape))

(defclass ty ()
  ())

(defclass name-ty (ty)
  ((name
    :type symbol:sym
    :initform (error "Must supply the name of the name-ty.")
    :initarg :name
    :accessor name-ty-name)
   (pos
    :type pos
    :initform (error "Must supply the position of the name-ty.")
    :initarg :pos
    :accessor name-ty-pos)))

(defun make-name-ty (name pos)
  (make-instance 'name-ty :name name :pos pos))

(defclass record-ty (ty)
  ((fields
    ;; A list of field.
    :type list
    :initform nil
    :initarg :fields
    :accessor record-ty-fields)))

(defun make-record-ty (fields)
  (make-instance 'record-ty :fields fields))

(defclass array-ty (ty)
  ((base-type-id
    :type symbol:sym
    :initform (error "Must supply the base-type-id of the array-ty.")
    :initarg :base-type-id
    :accessor array-ty-base-type-id)
   (pos
    :type pos
    :initform (error "Must supply the position of the array-ty.")
    :initarg :pos
    :accessor array-ty-pos)))

(defun make-array-ty (base-type-id pos)
  (make-instance 'array-ty :base-type-id base-type-id :pos pos))

(defclass function-decl ()
  ((name
    :type symbol:sym
    :initform (error "Must supply the name of the function declaration.")
    :initarg :name
    :accessor function-decl-name)
   (params
    ;; A list of field.
    :type list
    :initform (error "Must supply the params of the function declaration.")
    :initarg :params
    :accessor function-decl-params)
   (result
    ;; A list likes (sym pos) or nil.
    :type (or list null)
    :initform nil
    :initarg :result
    :accessor function-decl-result)
   (body
    :type expr
    :initform (error "Must supply the body of the function declaration.")
    :initarg :body
    :accessor function-decl-body)
   (pos
    :type pos
    :initform (error "Must supply the position of the function declaration.")
    :initarg :pos
    :accessor function-decl-pos)))

;; The result can be nil.
(defun make-function-decl (name params result body pos)
  (make-instance 'function-decl :name name :params params :result result :body body :pos pos))

(defclass type-decl ()
  ((name
    :type symbol:sym
    :initform (error "Must supply the name of the type declaration.")
    :initarg :name
    :accessor type-decl-name)
   (ty
    :type ty
    :initform (error "Must supply the type of the type declaration.")
    :initarg :ty
    :accessor type-decl-ty)
   (pos
    :type pos
    :initform (error "Must supply the position of the type declaration.")
    :initarg :pos
    :accessor type-decl-pos)))

(defun make-type-decl (name ty pos)
  (make-instance 'type-decl :name name :ty ty :pos pos))

(defclass decl ()
  ())

(defclass function-decls (decl)
  ((decls
    ;; A list of function-decl.
    :type list
    :initform nil
    :initarg :decls
    :accessor function-decls-decls)))

(defun make-function-decls (decls)
  (make-instance 'function-decls :decls decls))

(defclass type-decls (decl)
  ((decls
    ;; A list of type-decl.
    :type list
    :initform nil
    :initarg :decls
    :accessor type-decls-decls)))

(defun make-type-decls (decls)
  (make-instance 'type-decls :decls decls))

(defclass var-decl (decl)
  ((name
    :type symbol:sym
    :initform (error "Must supply the name of the variable declaration.")
    :initarg :name
    :accessor var-decl-name)
   (typ
    ;; A list of (sym pos) or nil.
    :type (or list null)
    :initform nil
    :initarg :typ
    :accessor var-decl-typ)
   (init
    :type expr
    :initform (error "Must supply the init expression of the variable declaration.")
    :initarg :init
    :accessor var-decl-init)
   (pos
    :type pos
    :initform (error "Must supply the position of the variable declaration.")
    :initarg :pos
    :accessor var-decl-pos)
   (escape
    :type boolean
    :initform t
    :initarg :escape
    :accessor var-decl-escape)))

(defun make-var-decl (name typ init pos &optional (escape t))
  (make-instance 'var-decl :name name :typ typ :init init :pos pos :escape escape))

(defclass let-expr (expr)
  ((decls
    ;; A list of decl.
    :type list
    :initform nil
    :initarg :decls
    :accessor let-expr-decls)
   (body
    :type expr
    :initform (error "Must supply the body of the let-expr.")
    :initarg :body
    :accessor let-expr-body)
   (pos
    :type pos
    :initform (error "Must supply the position of the let-expr.")
    :initarg :pos
    :accessor let-expr-pos)))

(defun make-let-expr (decls body pos)
  (make-instance 'let-expr :decls decls :body body :pos pos))

(defclass array-expr (expr)
  ((base-type-id
    :type symbol:sym
    :initform (error "Must supply the base-type-id of the array-expr.")
    :initarg :type-id
    :accessor array-expr-base-type-id)
   (size
    :type expr
    :initform (error "Must supply the size expression of the array-expr.")
    :initarg :size
    :accessor array-expr-size)
   (init
    :type expr
    :initform (error "Must supply the init expression of the array-expr.")
    :initarg :init
    :accessor array-expr-init)
   (pos
    :type pos
    :initform (error "Must supply the position of the array-expr.")
    :initarg :pos
    :accessor array-expr-pos)))

(defun make-array-expr (base-type-id size init pos)
  (make-instance 'array-expr
                 :base-type-id base-type-id
                 :size size :init init :pos pos))
