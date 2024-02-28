(cl:defpackage :cl-tiger/type-check
  (:use :cl)
  (:local-nicknames
   (:symbol :cl-tiger/symbol)
   (:utils :cl-tiger/utils)
   (:ast :cl-tiger/ast)
   (:types :cl-tiger/types))
  (:export
   #:type-check-error
   #:break-not-within-loop
   #:circular-dep
   #:then-else-types-of-if-mismatch
   #:then-else-types-of-if-mismatch-short-then-ty
   #:then-else-types-of-if-mismatch-short-else-ty
   #:then-else-types-of-if-mismatch-then-ty
   #:then-else-types-of-if-mismatch-else-ty
   #:then-of-if-then-not-unit
   #:then-of-if-then-not-unit-short-ty
   #:then-of-if-then-not-unit-ty
   #:body-of-while-not-unit
   #:body-of-while-not-unit-short-ty
   #:body-of-while-not-unit-ty
   #:for-low-not-int
   #:for-low-not-int-short-ty
   #:for-low-not-int-ty
   #:for-high-not-int
   #:for-high-not-int-short-ty
   #:for-high-not-int-ty
   #:assign-index-var
   #:assign-index-var-var
   #:unsupported-operation
   #:unsupported-operation-op
   #:unsupported-operation-short-left-ty
   #:unsupported-operation-short-right-ty
   #:unsupported-operation-left-ty
   #:unsupported-operation-right-ty
   #:undefined-type
   #:undefined-type-type-id
   #:undefined-field-type
   #:undefined-field-type-field-name
   #:undefined-array-base-type
   #:undefined-var
   #:undefined-var-name
   #:undefined-fun
   #:undefined-fun-name
   #:return-value-type-mismatch
   #:return-value-type-mismatch-short-decl-ty
   #:return-value-type-mismatch-short-actual-ty
   #:return-value-type-mismatch-decl-ty
   #:return-value-type-mismatch-actual-ty
   #:reference-unknown-record-field
   #:reference-unknown-record-field-short-record-ty
   #:reference-unknown-record-field-record-ty
   #:reference-unknown-record-field-unknown-field
   #:type-mismatch-of-assignment
   #:type-mismatch-of-assignment-short-var-ty
   #:type-mismatch-of-assignment-short-expr-ty
   #:type-mismatch-of-assignment-var-ty
   #:type-mismatch-of-assignment-expr-ty
   #:subscript-non-array
   #:subscript-non-array-short-var-ty
   #:subscript-non-array-var-ty
   #:access-field-of-non-record
   #:access-field-of-non-record-short-var-ty
   #:access-field-of-non-record-var-ty
   #:init-expr-type-mismatch
   #:init-expr-type-mismatch-short-decl-ty
   #:init-expr-type-mismatch-short-init-ty
   #:init-expr-type-mismatch-decl-ty
   #:init-expr-type-mismatch-init-ty
   #:array-init-expr-type-mismatch
   #:array-init-expr-type-mismatch-short-array-ty
   #:array-init-expr-type-mismatch-short-init-ty
   #:array-init-expr-type-mismatch-array-ty
   #:array-init-expr-type-mismatch-init-ty
   #:function-formal-actual-type-mismatch
   #:function-formal-actual-type-mismatch-short-formal-ty
   #:function-formal-actual-type-mismatch-short-actual-ty
   #:function-formal-actual-type-mismatch-formal-ty
   #:function-formal-actual-type-mismatch-actual-ty
   #:wrong-num-of-args
   #:wrong-num-of-args-fun
   #:wrong-num-of-args-expected-num-of-args
   #:wrong-num-of-args-actual-num-of-args
   #:duplicate-names-in-consecutive-type-decls
   #:duplicate-names-in-consecutive-type-decls-name
   #:duplicate-names-in-consecutive-fun-decls
   #:duplicate-names-in-consecutive-fun-decls-name

   #:continue-type-check

   #:type-check-program))

(cl:in-package :cl-tiger/type-check)

(define-condition type-check-error (error)
  ((msg
    :type string
    :initarg :msg
    :reader type-check-error-msg)
   (pos
    :type ast:pos
    :initarg :pos
    :reader type-check-error-pos)
   (line-map
    ;; From `cl-tiger/utils:get-line-map'.
    :type list
    :initform nil
    :initarg :line-map
    :reader type-check-error-line-map))
  (:report (lambda (type-check-error stream)
             (let ((line-info (utils:pos->line-info
                               (type-check-error-pos type-check-error)
                               (type-check-error-line-map type-check-error))))
               (format stream "~A" (type-check-error-msg type-check-error))
               (if line-info
                   (format stream " (line: ~A, column: ~A)~%~A~%~v:<^~>"
                           (first line-info)
                           (second line-info)
                           (third line-info)
                           (second line-info))
                   (format stream " (pos: ~A)" (type-check-error-pos type-check-error)))))))

(define-condition break-not-within-loop (type-check-error)
  ())

(define-condition circular-dep (type-check-error)
  ())

(define-condition then-else-types-of-if-mismatch (type-check-error)
  ((short-then-ty
    :type types:ty
    :initarg :short-then-ty
    :reader then-else-types-of-if-mismatch-short-then-ty)
   (short-else-ty
    :type types:ty
    :initarg :short-else-ty
    :reader then-else-types-of-if-mismatch-short-else-ty)
   (then-ty
    :type types:ty
    :initarg :then-ty
    :reader then-else-types-of-if-mismatch-then-ty)
   (else-ty
    :type types:ty
    :initarg :else-ty
    :reader then-else-types-of-if-mismatch-else-ty)))

(define-condition then-of-if-then-not-unit (type-check-error)
  ((short-ty
    :type types:ty
    :initarg :short-ty
    :reader then-of-if-then-not-unit-short-ty)
   (ty
    :type types:ty
    :initarg :ty
    :reader then-of-if-then-not-unit-ty)))

(define-condition body-of-while-not-unit (type-check-error)
  ((short-ty
    :type types:ty
    :initarg :short-ty
    :reader body-of-while-not-unit-short-ty)
   (ty
    :type types:ty
    :initarg :ty
    :reader body-of-while-not-unit-ty)))

(define-condition for-low-not-int (type-check-error)
  ((short-ty
    :type types:ty
    :initarg :short-ty
    :reader for-low-not-int-short-ty)
   (ty
    :type types:ty
    :initarg :ty
    :reader for-low-not-int-ty)))

(define-condition for-high-not-int (type-check-error)
  ((short-ty
    :type types:ty
    :initarg :short-ty
    :reader for-high-not-int-short-ty)
   (ty
    :type types:ty
    :initarg :ty
    :reader for-high-not-int-ty)))

(define-condition assign-index-var (type-check-error)
  ((var
    :type ast:var
    :initarg :var
    :reader assign-index-var-var)))

(define-condition unsupported-operation (type-check-error)
  ((op
    :type op
    :initarg :op
    :reader unsupported-operation-op)
   (short-left-ty
    :type types:ty
    :initarg :short-left-ty
    :reader unsupported-operation-short-left-ty)
   (short-right-ty
    :type types:ty
    :initarg :short-right-ty
    :reader unsupported-operation-short-right-ty)
   (left-ty
    :type types:ty
    :initarg :left-ty
    :reader unsupported-operation-left-ty)
   (right-ty
    :type types:ty
    :initarg :right-ty
    :reader unsupported-operation-right-ty)))

(define-condition undefined-type (type-check-error)
  ((type-id
    :type symbol:sym
    :initarg :type-id
    :reader undefined-type-type-id)))

(define-condition undefined-field-type (undefined-type)
  ((field-name
    :type symbol:sym
    :initarg :field-name
    :reader undefined-field-type-field-name)))

(define-condition undefined-array-base-type (undefined-type)
  ())

(define-condition undefined-var (type-check-error)
  ((name
    :type symbol:sym
    :initarg :name
    :reader undefined-var-name)))

(define-condition undefined-fun (type-check-error)
  ((name
    :type symbol:sym
    :initarg :name
    :reader undefined-fun-name)))

(define-condition return-value-type-mismatch (type-check-error)
  ((short-decl-ty
    :type types:ty
    :initarg :short-decl-ty
    :reader return-value-type-mismatch-short-decl-ty)
   (short-actual-ty
    :type types:ty
    :initarg :short-actual-ty
    :reader return-value-type-mismatch-short-actual-ty)
   (decl-ty
    :type types:ty
    :initarg :decl-ty
    :reader return-value-type-mismatch-decl-ty)
   (actual-ty
    :type types:ty
    :initarg :actual-ty
    :reader return-value-type-mismatch-actual-ty)))

(define-condition reference-unknown-record-field (type-check-error)
  ((short-record-ty
    :type types:ty
    :initarg :short-record-ty
    :reader reference-unknown-record-field-short-record-ty)
   (record-ty
    :type types:ty
    :initarg :record-ty
    :reader reference-unknown-record-field-record-ty)
   (unknown-field
    :type symbol:sym
    :initarg :unknown-field
    :reader reference-unknown-record-field-unknown-field)))

(define-condition type-mismatch-of-assignment (type-check-error)
  ((short-var-ty
    :type types:ty
    :initarg :short-var-ty
    :reader type-mismatch-of-assignment-short-var-ty)
   (short-expr-ty
    :type types:ty
    :initarg :short-expr-ty
    :reader type-mismatch-of-assignment-short-expr-ty)
   (var-ty
    :type types:ty
    :initarg :var-ty
    :reader type-mismatch-of-assignment-var-ty)
   (expr-ty
    :type types:ty
    :initarg :expr-ty
    :reader type-mismatch-of-assignment-expr-ty)))

(define-condition subscript-non-array (type-check-error)
  ((short-var-ty
    :type types:ty
    :initarg :short-var-ty
    :reader subscript-non-array-short-var-ty)
   (var-ty
    :type types:ty
    :initarg :var-ty
    :reader subscript-non-array-var-ty)))

(define-condition access-field-of-non-record (type-check-error)
  ((short-var-ty
    :type types:ty
    :initarg :short-var-ty
    :reader access-field-of-non-record-short-var-ty)
   (var-ty
    :type types:ty
    :initarg :var-ty
    :reader access-field-of-non-record-var-ty)))

(define-condition init-expr-type-mismatch (type-check-error)
  ((short-decl-ty
    :type types:ty
    :initarg :short-decl-ty
    :reader init-expr-type-mismatch-short-decl-ty)
   (short-init-ty
    :type types:ty
    :initarg :short-init-ty
    :reader init-expr-type-mismatch-short-init-ty)
   (decl-ty
    :type types:ty
    :initarg :decl-ty
    :reader init-expr-type-mismatch-decl-ty)
   (init-ty
    :type types:ty
    :initarg :init-ty
    :reader init-expr-type-mismatch-init-ty)))

(define-condition array-init-expr-type-mismatch (type-check-error)
  ((short-array-ty
    :type types:ty
    :initarg :short-array-ty
    :reader array-init-expr-type-mismatch-short-array-ty)
   (short-init-ty
    :type types:ty
    :initarg :short-init-ty
    :reader array-init-expr-type-mismatch-short-init-ty)
   (array-ty
    :type types:ty
    :initarg :array-ty
    :reader array-init-expr-type-mismatch-array-ty)
   (init-ty
    :type types:ty
    :initarg :init-ty
    :reader array-init-expr-type-mismatch-init-ty)))

(define-condition function-formal-actual-type-mismatch (type-check-error)
  ((short-formal-ty
    :type types:ty
    :initarg :short-formal-ty
    :reader function-formal-actual-type-mismatch-short-formal-ty)
   (short-actual-ty
    :type types:ty
    :initarg :short-actual-ty
    :reader function-formal-actual-type-mismatch-short-actual-ty)
   (formal-ty
    :type types:ty
    :initarg :formal-ty
    :reader function-formal-actual-type-mismatch-formal-ty)
   (actual-ty
    :type types:ty
    :initarg :actual-ty
    :reader function-formal-actual-type-mismatch-actual-ty)))

(define-condition wrong-num-of-args (type-check-error)
  ((fun
    :type symbol:sym
    :initarg :fun
    :reader wrong-num-of-args-fun)
   (expected-num-of-args
    :type integer
    :initarg :expected-num-of-args
    :reader wrong-num-of-args-expected-num-of-args)
   (actual-num-of-args
    :type integer
    :initarg :actual-num-of-args
    :reader wrong-num-of-args-actual-num-of-args)))

(define-condition duplicate-names-in-consecutive-type-decls (type-check-error)
  ((name
    :type symbol:sym
    :initarg :name
    :reader duplicate-names-in-consecutive-type-decls-name)))

(define-condition duplicate-names-in-consecutive-fun-decls (type-check-error)
  ((name
    :type symbol:sym
    :initarg :name
    :reader duplicate-names-in-consecutive-fun-decls-name)))

(defmacro def-type-check-error-constructor (type &rest initargs)
  `(defun ,type (pos line-map ,@initargs format &rest args)
     (with-simple-restart (continue-type-check "Ignore the type check error and continue to check.")
       (error ',type
              :msg (let ((*print-circle* t))
                     (apply #'format nil format args))
              :pos pos :line-map line-map
              ,@(mapcan (lambda (initarg)
                          (list (intern (symbol-name initarg) :keyword)
                                initarg))
                        initargs)))))

;; line-map can be nil
(def-type-check-error-constructor type-check-error)
(def-type-check-error-constructor break-not-within-loop)
(def-type-check-error-constructor circular-dep)
(def-type-check-error-constructor then-else-types-of-if-mismatch
  short-then-ty short-else-ty then-ty else-ty)
(def-type-check-error-constructor then-of-if-then-not-unit short-ty ty)
(def-type-check-error-constructor body-of-while-not-unit short-ty ty)
(def-type-check-error-constructor for-low-not-int short-ty ty)
(def-type-check-error-constructor for-high-not-int short-ty ty)
(def-type-check-error-constructor assign-index-var var)
(def-type-check-error-constructor unsupported-operation
  op short-left-ty short-right-ty left-ty right-ty)
(def-type-check-error-constructor undefined-type type-id)
(def-type-check-error-constructor undefined-field-type type-id field-name)
(def-type-check-error-constructor undefined-array-base-type type-id)
(def-type-check-error-constructor undefined-var name)
(def-type-check-error-constructor undefined-fun name)
(def-type-check-error-constructor return-value-type-mismatch
  short-decl-ty short-actual-ty decl-ty actual-ty)
(def-type-check-error-constructor reference-unknown-record-field
  short-record-ty record-ty unknown-field)
(def-type-check-error-constructor type-mismatch-of-assignment
  short-var-ty short-expr-ty var-ty expr-ty)
(def-type-check-error-constructor subscript-non-array
  short-var-ty var-ty)
(def-type-check-error-constructor access-field-of-non-record
  short-var-ty var-ty)
(def-type-check-error-constructor init-expr-type-mismatch
  short-decl-ty short-init-ty decl-ty init-ty)
(def-type-check-error-constructor array-init-expr-type-mismatch
  short-array-ty short-init-ty array-ty init-ty)
(def-type-check-error-constructor function-formal-actual-type-mismatch
  short-formal-ty short-actual-ty formal-ty actual-ty)
(def-type-check-error-constructor wrong-num-of-args
  fun expected-num-of-args actual-num-of-args)
(def-type-check-error-constructor duplicate-names-in-consecutive-type-decls
  name)
(def-type-check-error-constructor duplicate-names-in-consecutive-fun-decls
  name)

(defvar *line-map* nil)

(serapeum:defunion type-check-entry
  (type-check-entry-var
   (ty types:ty)
   (short-ty types:ty)
   ;; We can instead introduce a readonly? field,
   ;; but we want more concrete error messages.
   (is-index-var boolean))
  (type-check-entry-fun
   ;; A list of types:ty
   (formal-tys list)
   (short-formal-tys list)
   (result-ty types:ty)
   (short-result-ty types:ty)))

(serapeum:defconstructor type-check-var-result
  (ty types:ty)
  (short-ty types:ty)
  (is-index-var boolean))

(serapeum:defconstructor type-check-expr-result
  (ty types:ty)
  (short-ty types:ty))

(defun get-type-check-entry (type-check-env sym)
  (fset:@ type-check-env sym))

(defun insert-type-check-entry (type-check-env sym type-check-entry)
  (fset:with type-check-env sym type-check-entry))

(defvar *base-type-check-env*
  (let ((env (fset:empty-map)))
    (reduce (lambda (env binding)
              (trivia:let-match1 (list name formal-tys result-ty) binding
                (fset:with
                 env
                 (symbol:get-sym name)
                 ;; Right now, we assume all types in the bindings are "short".
                 (type-check-entry-fun formal-tys formal-tys result-ty result-ty))))
            types:*built-in-function-bindings*
            :initial-value env)))

(defun check-type-circular-dependency (ty pos)
  (let ((visited (make-hash-table))
        (path nil))
    (labels ((rec (ty)
               (serapeum:match-of types:ty ty
                 ((types:ty-name sym ty-ref)
                  (push (symbol:sym-name sym) path)
                  (when (gethash sym visited)
                    (circular-dep
                     pos *line-map*
                     "Circular type dependency: ~{~A~^ -> ~}." (nreverse path)))
                  (alexandria:when-let (type-ref-value (types:ty-ref-value ty-ref))
                    (setf (gethash sym visited) t)
                    (rec type-ref-value)))
                 (_ nil))))
      (rec ty)
      (values))))

(defun type-check-ty (ty-env ty)
  (serapeum:match-of ast:ty ty
    ((ast:ty-name name pos)
     (let ((ty (types:get-ty ty-env name)))
       (unless ty
         (undefined-type pos *line-map* name "Undefined type: ~A." (symbol:sym-name name)))
       ty))
    ((ast:ty-record fields)
     (types:ty-record
      (mapcar (lambda (field)
                (trivia:let-match1 (ast:field name type-id pos _) field
                  (let ((ty (types:get-ty ty-env type-id)))
                    (unless ty
                      (undefined-field-type
                       pos *line-map*
                       type-id name
                       "The type of field ~A is an undefined type: ~A."
                       (symbol:sym-name name)
                       (symbol:sym-name type-id)))
                    (list name ty))))
              fields)))
    ((ast:ty-array base-type-id pos)
     (let ((ty (types:get-ty ty-env base-type-id)))
       (unless ty
         (undefined-array-base-type
          pos
          *line-map* base-type-id
          "Undefined base type of an array: ~A."
          (symbol:sym-name base-type-id)))
       (types:ty-array ty)))))

(defun type-check-var (ty-env type-check-env within-loop var)
  (serapeum:match-of ast:var var
    ((ast:var-simple sym pos)
     (alexandria:if-let (type-check-entry (get-type-check-entry type-check-env sym))
       (serapeum:match-of type-check-entry type-check-entry
         ((type-check-entry-var ty short-ty is-index-var)
          (type-check-var-result (types:actual-ty ty) short-ty is-index-var))
         ((type-check-entry-fun _ _ _ _)
          (type-check-error
           pos *line-map*
           "var-simple cannot reference to a function.")))
       (undefined-var
        pos *line-map* sym
        "Undefined variable: ~A." (symbol:sym-name sym))))
    ((ast:var-field var sym pos)
     (trivia:let-match1 (type-check-var-result ty short-ty _)
         (type-check-var ty-env type-check-env within-loop var)
       (serapeum:match-of types:ty ty
         ((types:ty-record fields)
          (alexandria:if-let
              (field
               (find-if (lambda (field)
                          ;; field is of the form (sym ty)
                          (eq (first field) sym))
                        fields))
            (type-check-var-result
             (types:actual-ty (second field))
             (second field)
             nil)
            (reference-unknown-record-field
             pos *line-map* short-ty ty sym
             "Reference an unknown field ~A of the record with the type ~A."
             (symbol:sym-name sym)
             (types:short-ty->string short-ty))))
         (_ (access-field-of-non-record
             pos *line-map* short-ty ty
             "You can only access a field of a record, but you access a field of a value of the type: ~A."
             (types:short-ty->string short-ty))))))
    ((ast:var-subscript var expr pos)
     (prog1
         (trivia:let-match1 (type-check-var-result ty short-ty _)
             (type-check-var ty-env type-check-env within-loop var)
           (serapeum:match-of types:ty ty
             ((types:ty-array base-ty)
              (type-check-var-result (types:actual-ty base-ty) base-ty nil))
             (_ (subscript-non-array
                 pos *line-map* short-ty ty
                 "You can only subscript an array, but you subscript a value of the type: ~A."
                 (types:short-ty->string short-ty)))))
       (trivia:let-match1
           (type-check-expr-result index-ty _)
           (type-check-expr ty-env type-check-env within-loop expr)
         (unless (types:ty-compatible index-ty (types:get-ty types:*base-ty-env* (symbol:get-sym "int")))
           (type-check-error
            pos *line-map*
            "The type of the subscript expression of an array should be int.")))))))

(defun type-check-decl (ty-env type-check-env within-loop decl)
  (serapeum:match-of ast:decl decl
    ((ast:decl-var name typ init pos _)
     ;; typ form: (sym pos) or nil.
     (when typ
       (unless (types:get-ty ty-env (first typ))
         (undefined-type
          (second typ) *line-map* (first typ)
          "Undefined type: ~A." (symbol:sym-name (first typ)))))
     (trivia:let-match*
         (((type-check-expr-result init-ty short-init-ty)
           (type-check-expr ty-env type-check-env within-loop init))
          (final-ty init-ty)
          (short-final-ty short-init-ty))
       (when typ
         (let ((ty-sym (first typ)))
           (setf short-final-ty (types:ty-name ty-sym (types:ty-ref nil)))
           ;; Note: we already check (types:get-ty ty-env ty-sym) is not nil in the above,
           ;; so is safe to use types:actual-ty without checking here.
           (let ((decl-ty (types:actual-ty (types:get-ty ty-env ty-sym))))
             (cond ((types:ty-compatible init-ty decl-ty)
                    (setf final-ty decl-ty))
                   (t
                    (let ((short-decl-ty (types:ty-name ty-sym (types:ty-ref nil))))
                      (init-expr-type-mismatch
                       pos *line-map*
                       short-decl-ty short-init-ty decl-ty init-ty
                       "The type of the init expression is ~A, it doesn't match the declared type ~A of the variable ~A."
                       (types:short-ty->string short-init-ty)
                       (types:short-ty->string short-decl-ty)
                       (symbol:sym-name name))))))))
       (list ty-env
             (insert-type-check-entry
              type-check-env name (type-check-entry-var final-ty short-final-ty nil)))))
    ((ast:decl-types decl-types)
     (let ((name-exists-table (make-hash-table)))
       (mapc (lambda (decl-type)
               (let ((decl-type-name (ast:decl-type-name decl-type))
                     (decl-type-pos (ast:decl-type-pos decl-type)))
                 (when (gethash decl-type-name name-exists-table)
                   (duplicate-names-in-consecutive-type-decls
                    decl-type-pos *line-map* decl-type-name
                    "Duplicate names ~A in a consecutive type declarations."
                    (symbol:sym-name decl-type-name)))
                 (setf (gethash decl-type-name name-exists-table) t)
                 ;; (when (gethash decl-type-name types:*base-ty-env*)
                 ;;   (type-check-error
                 ;;    decl-type-pos *line-map*
                 ;;    "Cannot shadow built-in type: ~A."
                 ;;    (symbol:sym-name decl-type-name)))
                 ))
             decl-types))
     (let ((new-ty-env
             (reduce (lambda (acc-ty-env decl-type)
                       (types:insert-ty
                        acc-ty-env
                        (ast:decl-type-name decl-type)
                        (types:ty-name (ast:decl-type-name decl-type) (types:ty-ref nil))))
                     decl-types
                     :initial-value ty-env)))
       (mapc (lambda (decl-type)
               (let ((ty (type-check-ty new-ty-env (ast:decl-type-ty decl-type))))
                 (setf
                  (types:ty-ref-value
                   (types:ty-name-ty-ref
                    (types:get-ty new-ty-env (ast:decl-type-name decl-type))))
                  ty)
                 (check-type-circular-dependency ty (ast:decl-type-pos decl-type))))
             decl-types)
       (list new-ty-env type-check-env)))
    ((ast:decl-functions decl-functions)
     (let ((name-exists-table (make-hash-table)))
       (mapc (lambda (decl-function)
               (let ((decl-function-name (ast:decl-function-name decl-function))
                     (decl-function-pos (ast:decl-function-pos decl-function)))
                 (when (gethash decl-function-name name-exists-table)
                   (duplicate-names-in-consecutive-fun-decls
                    decl-function-pos *line-map* decl-function-name
                    "Duplicate names ~A in a consecutive function declarations."
                    (symbol:sym-name decl-function-name)))
                 (setf (gethash decl-function-name name-exists-table) t)))
             decl-functions))
     (let ((new-type-check-env
             (reduce (lambda (acc-type-check-env decl-function)
                       (let ((function-name (ast:decl-function-name decl-function))
                             (function-params (ast:decl-function-params decl-function))
                             (function-result (ast:decl-function-result decl-function)))
                         (insert-type-check-entry
                          acc-type-check-env
                          function-name
                          (type-check-entry-fun
                           (mapcar (lambda (param-field)
                                     (let ((ty (types:get-ty ty-env (ast:field-type-id param-field))))
                                       (unless ty
                                         (type-check-error
                                          (ast:field-pos param-field) *line-map*
                                          "Undefined type ~A of param ~A of function ~A."
                                          (symbol:sym-name (ast:field-type-id param-field))
                                          (symbol:sym-name (ast:field-name param-field))
                                          (symbol:sym-name function-name)))
                                       ty))
                                   function-params)
                           (mapcar (lambda (param-field)
                                     (types:ty-name
                                      (ast:field-type-id param-field)
                                      (types:ty-ref nil)))
                                   function-params)
                           ;; function-result form: (sym pos).
                           (let ((ty (if function-result
                                         (types:get-ty ty-env (first function-result))
                                         (types:get-unnamed-base-ty (symbol:get-sym "unit")))))
                             (unless ty
                               (type-check-error
                                (second function-result) *line-map*
                                "Undefined result type ~A of function ~A."
                                (symbol:sym-name (first function-result))
                                (symbol:sym-name function-name)))
                             ty)
                           (if function-result
                               (types:ty-name
                                (first function-result)
                                (types:ty-ref nil))
                               (types:get-unnamed-base-ty (symbol:get-sym "unit")))))))
                     decl-functions
                     :initial-value type-check-env)))
       (mapc (lambda (decl-function)
               (trivia:let-match1
                   (type-check-entry-fun formal-tys short-formal-tys result-ty short-result-ty)
                   (get-type-check-entry new-type-check-env (ast:decl-function-name decl-function))
                 (trivia:let-match1
                     (type-check-expr-result body-ty short-body-ty)
                     (type-check-expr
                      ty-env
                      (loop with acc-type-check-env = new-type-check-env
                            for formal-ty in formal-tys
                            for short-formal-ty in short-formal-tys
                            for param-field in (ast:decl-function-params decl-function)
                            do (setf acc-type-check-env
                                     (insert-type-check-entry
                                      acc-type-check-env
                                      (ast:field-name param-field)
                                      (type-check-entry-var formal-ty short-formal-ty nil)))
                            finally (return acc-type-check-env))
                      ;; Cannot break into the outer function.
                      nil
                      (ast:decl-function-body decl-function))
                   (unless (types:ty-compatible (types:actual-ty result-ty) body-ty)
                     (return-value-type-mismatch
                      (ast:decl-function-pos decl-function)
                      *line-map* short-result-ty short-body-ty result-ty body-ty
                      "Function ~A declared to return a value with type ~A, but actually return a value with type ~A."
                      (symbol:sym-name (ast:decl-function-name decl-function))
                      (types:short-ty->string short-result-ty)
                      (types:short-ty->string short-body-ty))))))
             decl-functions)
       (list ty-env new-type-check-env)))))

(defun type-check-decls (ty-env type-check-env within-loop decls)
  (reduce (lambda (acc decl)
            (type-check-decl (first acc) (second acc) within-loop decl))
          decls
          :initial-value (list ty-env type-check-env)))

(defun type-check-expr (ty-env type-check-env within-loop expr)
  (serapeum:match-of ast:expr expr
    ((ast:expr-var var)
     (trivia:let-match1 (type-check-var-result ty short-ty _)
         (type-check-var ty-env type-check-env within-loop var)
       (type-check-expr-result ty short-ty)))
    ((ast:expr-nil)
     (let ((ty (types:get-unnamed-base-ty (symbol:get-sym "nil"))))
       (type-check-expr-result ty ty)))
    ((ast:expr-int _)
     (let ((ty (types:get-ty types:*base-ty-env* (symbol:get-sym "int"))))
       (type-check-expr-result ty ty)))
    ((ast:expr-string _ _)
     (let ((ty (types:get-ty types:*base-ty-env* (symbol:get-sym "string"))))
       (type-check-expr-result ty ty)))
    ((ast:expr-call fun args pos)
     (alexandria:if-let (type-check-entry (get-type-check-entry type-check-env fun))
       (serapeum:match-of type-check-entry type-check-entry
         ((type-check-entry-fun formal-tys short-formal-tys result-ty short-result-ty)
          (unless (eql (length args) (length formal-tys))
            (let ((expected-num-of-args (length formal-tys))
                  (actual-num-of-args (length args)))
              (wrong-num-of-args
               pos *line-map* fun expected-num-of-args actual-num-of-args
               "Function ~A expect ~A args, but got ~A args."
               (symbol:sym-name fun) expected-num-of-args actual-num-of-args)))
          (loop for formal-ty in formal-tys
                for short-formal-ty in short-formal-tys
                for type-check-arg-result in (mapcar (lambda (arg)
                                              (type-check-expr ty-env type-check-env within-loop arg))
                                            args)
                for i from 1
                do (trivia:let-match1 (type-check-expr-result arg-ty short-arg-ty) type-check-arg-result
                     (unless (types:ty-compatible (types:actual-ty formal-ty) arg-ty)
                       (function-formal-actual-type-mismatch
                        pos *line-map* short-formal-ty short-arg-ty formal-ty arg-ty
                        "Function ~A ~Ath arg expect type ~A, but got a arg of type ~A."
                        (symbol:sym-name fun) i formal-ty arg-ty))))
          (type-check-expr-result (types:actual-ty result-ty) short-result-ty))
         (_ (type-check-error
             pos *line-map*
             "You can only call a function.")))
       (undefined-fun
        pos *line-map* fun
        "Undefined function ~A." (symbol:sym-name fun))))
    ((ast:expr-op left op right pos)
     (trivia:let-match
         (((type-check-expr-result left-ty short-left-ty)
           (type-check-expr ty-env type-check-env within-loop left))
          ((type-check-expr-result right-ty short-right-ty)
           (type-check-expr ty-env type-check-env within-loop right))
          (ty-int (types:get-ty types:*base-ty-env* (symbol:get-sym "int"))))
       (trivia:match (list left-ty right-ty)
         ((list (types:ty-int) (types:ty-int))
          (type-check-expr-result ty-int ty-int))
         ((list (types:ty-string) (types:ty-string))
          (unless (serapeum:match-of ast:op op
                    ((or ast:op-eq ast:op-neq ast:op-lt ast:op-le ast:op-gt ast:op-ge)
                     t)
                    (_ nil))
            (unsupported-operation
             pos *line-map* op short-left-ty short-right-ty left-ty right-ty
             "Unsupported operation ~A on strings."
             (ast:op->string op)))
          (type-check-expr-result ty-int ty-int))
         (_
          (unless (and
                   (serapeum:match-of ast:op op
                     ((or ast:op-eq ast:op-neq)
                      t)
                     (_ nil))
                   (types:ty-compatible left-ty right-ty))
            (unsupported-operation
             pos *line-map* op short-left-ty short-right-ty left-ty right-ty
             "Unsupported operation (type ~A) ~A (type ~A)."
             (types:short-ty->string short-left-ty)
             (ast:op->string op)
             (types:short-ty->string short-right-ty)))
          (type-check-expr-result ty-int ty-int)))))
    ((ast:expr-record type-id fields pos)
     (alexandria:if-let (type-id-ty (types:get-ty ty-env type-id))
       (let ((ty (types:actual-ty type-id-ty)))
         (trivia:if-match (types:ty-record decl-fields) ty
           (progn
             ;; decl-field form: (sym ty).
             (loop for decl-field in decl-fields
                   for decl-field-sym = (first decl-field)
                   for decl-field-ty = (second decl-field)
                   do
                      (alexandria:if-let
                          (field (find-if (lambda (field)
                                            ;; field form: (sym expr pos)
                                            (eql (first field) decl-field-sym))
                                          fields))
                        (trivia:let-match1 (type-check-expr-result field-ty _)
                            (type-check-expr ty-env type-check-env within-loop (second field))
                          (unless (types:ty-compatible field-ty (types:actual-ty decl-field-ty))
                            (type-check-error
                             (third field) *line-map*
                             "The type of the init expression of the field ~A \
doesn't match the expected type."
                             (symbol:sym-name decl-field-sym))))
                        (type-check-error
                         pos *line-map*
                         "Missing the init expression of the field: ~A."
                         (symbol:sym-name decl-field-sym))))
             (loop for field in fields
                   ;; field form: (sym expr pos)
                   for field-sym = (first field)
                   do (unless (find-if (lambda (decl-field)
                                         ;; decl-field form: (sym ty).
                                         (eql (first decl-field) field-sym))
                                       decl-fields)
                        (type-check-error
                         pos *line-map*
                         "Unknown field ~A of the record." (symbol:sym-name field-sym))))
             (type-check-expr-result
              ty (types:ty-name type-id (types:ty-ref nil))))
           (type-check-error
            pos *line-map*
            "Type ~A is not a record." (symbol:sym-name type-id))))
       (undefined-type
        pos *line-map* type-id
        "Undefined record type: ~A." (symbol:sym-name type-id))))
    ((ast:expr-seq exprs)
     (reduce (lambda (acc-type expr-with-pos)
               ;; expr-with-pos form: (expr pos).
               (declare (ignore acc-type))
               (type-check-expr ty-env type-check-env within-loop (first expr-with-pos)))
             exprs
             :initial-value
             (let ((ty (types:get-unnamed-base-ty (symbol:get-sym "unit"))))
               (type-check-expr-result ty ty))))
    ((ast:expr-assign var expr pos)
     (trivia:let-match (((type-check-var-result var-ty short-var-ty is-index-var)
                         (type-check-var ty-env type-check-env within-loop var))
                        ((type-check-expr-result expr-ty short-expr-ty)
                         (type-check-expr ty-env type-check-env within-loop expr)))
       (when is-index-var
         (assign-index-var
          pos *line-map* var
          "Cannot assign an index variable."))
       (unless (types:ty-compatible var-ty expr-ty)
         (type-mismatch-of-assignment
          pos *line-map* short-var-ty short-expr-ty var-ty expr-ty
          "Type mismatch of the assignment, expect the type: ~A, but given a value of the type: ~A."
          (types:short-ty->string short-var-ty)
          (types:short-ty->string short-expr-ty)))
       (let ((ty (types:get-unnamed-base-ty (symbol:get-sym "unit"))))
         (type-check-expr-result ty ty))))
    ((ast:expr-if test then else pos)
     (trivia:let-match
         (((type-check-expr-result test-ty _)
           (type-check-expr ty-env type-check-env within-loop test))
          ((type-check-expr-result then-ty short-then-ty)
           (type-check-expr ty-env type-check-env within-loop then))
          ((type-check-expr-result else-ty short-else-ty)
           (if else
               (type-check-expr ty-env type-check-env within-loop else)
               (let ((ty (types:get-unnamed-base-ty (symbol:get-sym "unit"))))
                 ;; Give some arbitrary types, we won't use them.
                 (type-check-expr-result ty ty)))))
       (unless (types:ty-compatible test-ty (types:get-ty types:*base-ty-env* (symbol:get-sym "int")))
         (type-check-error
          pos *line-map*
          "The type of the test expression of an if expression should be int."))
       (if else
           (unless (types:ty-compatible then-ty else-ty)
             (then-else-types-of-if-mismatch
              pos *line-map*
              short-then-ty short-else-ty
              then-ty else-ty
              "The types of then and else branchs of an if expression should be the same, the type of then: ~A, the type of else: ~A."
              (types:short-ty->string short-then-ty)
              (types:short-ty->string short-else-ty)))
           (unless (types:ty-compatible then-ty (types:get-unnamed-base-ty (symbol:get-sym "unit")))
             (then-of-if-then-not-unit
              pos *line-map* short-then-ty then-ty
              "Without else branch, the type of then branch of if-expr should be unit, but is ~A."
              (types:short-ty->string short-then-ty))))
       (if else
           (type-check-expr-result
            (types:upgrade-from-compatible-tys then-ty else-ty)
            (types:upgrade-from-compatible-short-tys short-then-ty short-else-ty))
           (let ((ty (types:get-unnamed-base-ty (symbol:get-sym "unit"))))
             (type-check-expr-result ty ty)))))
    ((ast:expr-while test body pos)
     (trivia:let-match
         (((type-check-expr-result test-ty _)
           (type-check-expr ty-env type-check-env within-loop test))
          ((type-check-expr-result body-ty short-body-ty)
           (type-check-expr ty-env type-check-env t body)))
       (unless (types:ty-compatible test-ty (types:get-ty types:*base-ty-env* (symbol:get-sym "int")))
         (type-check-error
          pos *line-map*
          "The type of the test expression of a while expression should be int."))
       (unless (types:ty-compatible body-ty (types:get-unnamed-base-ty (symbol:get-sym "unit")))
         (body-of-while-not-unit
          pos *line-map* short-body-ty body-ty
          "The type of the body expression of a while expression should be unit, but is ~A."
          (types:short-ty->string short-body-ty)))
       (type-check-expr-result body-ty short-body-ty)))
    ((ast:expr-for var low high body pos _)
     (trivia:let-match
         (((type-check-expr-result low-ty short-low-ty)
           (type-check-expr ty-env type-check-env within-loop low))
          ((type-check-expr-result high-ty short-high-ty)
           (type-check-expr ty-env type-check-env within-loop high)))
       (let ((ty-int (types:get-ty types:*base-ty-env* (symbol:get-sym "int"))))
         (unless (types:ty-compatible low-ty ty-int)
           (for-low-not-int
            pos *line-map* short-low-ty low-ty
            "The type of the low expression of a for expression should be int, but is ~A."
            (types:short-ty->string short-low-ty)))
         (unless (types:ty-compatible high-ty ty-int)
           (for-high-not-int
            pos *line-map* short-high-ty high-ty
            "The type of the high expression of a for expression should be int, but is ~A."
            (types:short-ty->string short-high-ty)))
         (let ((new-type-check-env
                 (insert-type-check-entry type-check-env var (type-check-entry-var ty-int ty-int t))))
           (trivia:let-match1
               (type-check-expr-result body-ty short-body-ty)
               (type-check-expr ty-env new-type-check-env t body)
             (unless (types:ty-compatible body-ty (types:get-unnamed-base-ty (symbol:get-sym "unit")))
               (type-check-error
                pos *line-map*
                "The body expression of a for expression should product no value."))
             (type-check-expr-result body-ty short-body-ty))))))
    ((ast:expr-break pos)
     (unless within-loop
       (break-not-within-loop
        pos *line-map*
        "break expression not within a loop expression."))
     (let ((ty (types:get-unnamed-base-ty (symbol:get-sym "unit"))))
       (type-check-expr-result ty ty)))
    ((ast:expr-array type-id size init pos)
     (let* ((type-id-ty (types:get-ty ty-env type-id))
            (ty (progn
                  (unless type-id-ty
                    (undefined-type
                     pos *line-map* type-id
                     "Undefined array type: ~A." (symbol:sym-name type-id)))
                  (types:actual-ty type-id-ty))))
       (trivia:if-match (types:ty-array base-ty) ty
         (trivia:let-match
             (((type-check-expr-result size-ty _)
               (type-check-expr ty-env type-check-env within-loop size))
              ((type-check-expr-result init-ty short-init-ty)
               (type-check-expr ty-env type-check-env within-loop init)))
           (unless (types:ty-compatible size-ty (types:get-ty types:*base-ty-env* (symbol:get-sym "int")))
             (type-check-error
              pos *line-map*
              "The type of size expression of array creation expression should be int."))
           (unless (types:ty-compatible init-ty (types:actual-ty base-ty))
             (let ((short-array-ty (types:ty-name type-id (types:ty-ref nil))))
               (array-init-expr-type-mismatch
                pos *line-map* short-array-ty short-init-ty ty init-ty
                "The type of the init expression doesn't match the base type of the array type ~A, expected base type: ~A, but got: ~A."
                (types:short-ty->string short-array-ty)
                ;; We store base-ty of ty-array as short-ty, so the
                ;; following is safe.
                (types:short-ty->string base-ty)
                (types:short-ty->string short-init-ty)))))
         (type-check-error
          pos *line-map*
          "Type ~A is not an array." (symbol:sym-name type-id)))
       (type-check-expr-result
        ty
        (types:ty-name type-id (types:ty-ref nil)))))
    ((ast:expr-let decls body _)
     (trivia:let-match1 (list new-ty-env new-type-check-env)
         (type-check-decls ty-env type-check-env within-loop decls)
       (type-check-expr new-ty-env new-type-check-env within-loop body)))))

(defun type-check-program (prog &optional line-map)
  (let ((*line-map* line-map))
    (type-check-expr types:*base-ty-env*
                     *base-type-check-env*
                     nil
                     prog)))
