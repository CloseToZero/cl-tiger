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
   #:then-else-types-of-if-mismatch-then-ty
   #:then-else-types-of-if-mismatch-else-ty
   #:then-of-if-then-not-unit
   #:then-of-if-then-not-unit-ty
   #:body-of-while-not-unit
   #:body-of-while-not-unit-ty
   #:for-low-not-int
   #:for-low-not-int-ty
   #:for-high-not-int
   #:for-high-not-int-ty
   #:assign-index-var
   #:assign-index-var-var
   #:unsupport-operation
   #:unsupport-operation-op
   #:unsupport-operation-left-ty
   #:unsupport-operation-right-ty
   #:undefined-type
   #:undefined-type-type-id
   #:undefined-field-type
   #:undefined-field-type-field-name
   #:undefined-var
   #:undefined-var-name
   #:undefined-fun
   #:undefined-fun-name
   #:return-value-type-mismatch
   #:return-value-type-mismatch-declared-ty
   #:return-value-type-mismatch-actual-ty
   #:reference-unknown-record-field
   #:reference-unknown-record-field-record-ty
   #:reference-unknown-record-field-unknown-field
   #:type-mismatch-of-assignment
   #:type-mismatch-of-assignment-var-ty
   #:type-mismatch-of-assignment-expr-ty

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
  ((then-ty
    :type types:ty
    :initarg :then-ty
    :reader then-else-types-of-if-mismatch-then-ty)
   (else-ty
    :type types:ty
    :initarg :else-ty
    :reader then-else-types-of-if-mismatch-else-ty)))

(define-condition then-of-if-then-not-unit (type-check-error)
  ((ty
    :type types:ty
    :initarg :ty
    :reader then-of-if-then-not-unit-ty)))

(define-condition body-of-while-not-unit (type-check-error)
  ((ty
    :type types:ty
    :initarg :ty
    :reader body-of-while-not-unit-ty)))

(define-condition for-low-not-int (type-check-error)
  ((ty
    :type types:ty
    :initarg :ty
    :reader for-low-not-int-ty)))

(define-condition for-high-not-int (type-check-error)
  ((ty
    :type types:ty
    :initarg :ty
    :reader for-high-not-int-ty)))

(define-condition assign-index-var (type-check-error)
  ((var
    :type ast:var
    :initarg :var
    :reader assign-index-var-var)))

(define-condition unsupport-operation (type-check-error)
  ((op
    :type op
    :initarg :op
    :reader unsupport-operation-op)
   (left-ty
    :type types:ty
    :initarg :left-ty
    :reader unsupport-operation-left-ty)
   (right-ty
    :type types:ty
    :initarg :right-ty
    :reader unsupport-operation-right-ty)))

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
  ((declared-ty
    :type types:ty
    :initarg :declared-ty
    :reader return-value-type-mismatch-declared-ty)
   (actual-ty
    :type types:ty
    :initarg :actual-ty
    :reader return-value-type-mismatch-actual-ty)))

(define-condition reference-unknown-record-field (type-check-error)
  ((record-ty
    :type types:ty
    :initarg :record-ty
    :reader reference-unknown-record-field-record-ty)
   (unknown-field
    :type symbol:sym
    :initarg :unknown-field
    :reader reference-unknown-record-field-unknown-field)))

(define-condition type-mismatch-of-assignment (type-check-error)
  ((var-ty
    :type types:ty
    :initarg :var-ty
    :reader type-mismatch-of-assignment-var-ty)
   (expr-ty
    :type types:ty
    :initarg :expr-ty
    :reader type-mismatch-of-assignment-expr-ty)))

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
(def-type-check-error-constructor then-else-types-of-if-mismatch then-ty else-ty)
(def-type-check-error-constructor then-of-if-then-not-unit ty)
(def-type-check-error-constructor body-of-while-not-unit ty)
(def-type-check-error-constructor for-low-not-int ty)
(def-type-check-error-constructor for-high-not-int ty)
(def-type-check-error-constructor assign-index-var var)
(def-type-check-error-constructor unsupport-operation op left-ty right-ty)
(def-type-check-error-constructor undefined-type type-id)
(def-type-check-error-constructor undefined-field-type type-id field-name)
(def-type-check-error-constructor undefined-var name)
(def-type-check-error-constructor undefined-fun name)
(def-type-check-error-constructor return-value-type-mismatch declared-ty actual-ty)
(def-type-check-error-constructor reference-unknown-record-field record-ty unknown-field)
(def-type-check-error-constructor type-mismatch-of-assignment var-ty expr-ty)

(defvar *line-map* nil)

(serapeum:defunion type-check-entry
  (type-check-entry-var
   (ty types:ty)
   ;; We can instead introduce a readonly? field,
   ;; but we want more concrete error messages.
   (is-index-var boolean))
  (type-check-entry-fun
   ;; A list of types:ty
   (formal-types list)
   (result-type types:ty)))

(defun get-type-check-entry (type-check-env sym)
  (fset:@ type-check-env sym))

(defun insert-type-check-entry (type-check-env sym type-check-entry)
  (fset:with type-check-env sym type-check-entry))

(defvar *base-type-check-env*
  (let ((env (fset:empty-map)))
    (reduce (lambda (env binding)
              (trivia:let-match1 (list name formal-types result-type) binding
                (fset:with env
                           (symbol:get-sym name)
                           (type-check-entry-fun formal-types result-type))))
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

(defun type-check-ty (type-env ty)
  (serapeum:match-of ast:ty ty
    ((ast:ty-name name pos)
     (let ((ty (types:get-type type-env name)))
       (unless ty
         (undefined-type pos *line-map* name "Undefined type: ~A." (symbol:sym-name name)))
       ty))
    ((ast:ty-record fields)
     (types:ty-record
      (mapcar (lambda (field)
                (trivia:let-match1 (ast:field name type-id pos _) field
                  (let ((ty (types:get-type type-env type-id)))
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
     (let ((ty (types:get-type type-env base-type-id)))
       (unless ty
         (type-check-error
          pos
          *line-map*
          "Undefined base type of an array: ~A."
          (symbol:sym-name base-type-id)))
       (types:ty-array ty)))))

(serapeum:defconstructor type-check-var-result
  (ty types:ty)
  (is-index-var boolean))

(defun type-check-var (type-env type-check-env within-loop var)
  (serapeum:match-of ast:var var
    ((ast:var-simple sym pos)
     (alexandria:if-let (type-check-entry (get-type-check-entry type-check-env sym))
       (serapeum:match-of type-check-entry type-check-entry
         ((type-check-entry-var ty is-index-var)
          (type-check-var-result (types:actual-ty ty) is-index-var))
         ((type-check-entry-fun _ _)
          (type-check-error
           pos *line-map*
           "var-simple cannot reference to a function.")))
       (undefined-var
        pos *line-map* sym
        "Undefined variable: ~A." (symbol:sym-name sym))))
    ((ast:var-field var sym pos)
     (trivia:let-match1 (type-check-var-result ty _) (type-check-var type-env type-check-env within-loop var)
       (serapeum:match-of types:ty ty
         ((types:ty-record fields)
          (alexandria:if-let
              (field
               (find-if (lambda (field)
                          ;; field is of the form (sym ty)
                          (eq (first field) sym))
                        fields))
            (type-check-var-result (types:actual-ty (second field)) nil)
            (reference-unknown-record-field
             pos *line-map* ty sym
             "Reference an unknown field ~A of the record." (symbol:sym-name sym))))
         (_ (type-check-error
             pos *line-map*
             "You can only access the field of a record.")))))
    ((ast:var-subscript var expr pos)
     (prog1
         (trivia:let-match1 (type-check-var-result ty _)
             (type-check-var type-env type-check-env within-loop var)
           (serapeum:match-of types:ty ty
             ((types:ty-array base-type)
              (type-check-var-result (types:actual-ty base-type) nil))
             (_ (type-check-error
                 pos *line-map*
                 "You can only subscript an array."))))
       (let ((index-ty (type-check-expr type-env type-check-env within-loop expr)))
         (unless (types:type-compatible index-ty (types:get-type types:*base-type-env* (symbol:get-sym "int")))
           (type-check-error
            pos *line-map*
            "The type of the subscript expression of an array should be int.")))))))

(defun type-check-decl (type-env type-check-env within-loop decl)
  (serapeum:match-of ast:decl decl
    ((ast:decl-var name typ init pos _)
     ;; typ form: (sym pos) or nil.
     (when typ
       (unless (types:get-type type-env (first typ))
         (type-check-error
          (second typ) *line-map*
          "Undefined type: ~A." (symbol:sym-name (first typ)))))
     (let* ((init-ty (type-check-expr type-env type-check-env within-loop init))
            (final-ty init-ty))
       (when typ
         (let ((decl-ty (types:actual-ty (types:get-type type-env (first typ)))))
           (cond ((types:type-compatible init-ty decl-ty)
                  (setf final-ty decl-ty))
                 (t
                  (type-check-error
                   pos *line-map*
                   "The type of the init expression of the variable doesn't match the type ~A."
                   (symbol:sym-name (first typ)))))))
       (list type-env
             (insert-type-check-entry
              type-check-env name (type-check-entry-var final-ty nil)))))
    ((ast:decl-types decl-types)
     (let ((name-exists-table (make-hash-table)))
       (mapc (lambda (decl-type)
               (let ((decl-type-name (ast:decl-type-name decl-type))
                     (decl-type-pos (ast:decl-type-pos decl-type)))
                 (when (gethash decl-type-name name-exists-table)
                   (type-check-error
                    decl-type-pos *line-map*
                    "Duplicate names ~A in a consecutive type declarations."
                    (symbol:sym-name decl-type-name)))
                 (setf (gethash decl-type-name name-exists-table) t)
                 ;; (when (gethash decl-type-name types:*base-type-env*)
                 ;;   (type-check-error
                 ;;    decl-type-pos *line-map*
                 ;;    "Cannot shadow built-in type: ~A."
                 ;;    (symbol:sym-name decl-type-name)))
                 ))
             decl-types))
     (let ((new-type-env
             (reduce (lambda (acc-type-env decl-type)
                       (types:insert-type acc-type-env
                                          (ast:decl-type-name decl-type)
                                          (types:ty-name (ast:decl-type-name decl-type) (types:ty-ref nil))))
                     decl-types
                     :initial-value type-env)))
       (mapc (lambda (decl-type)
               (let ((ty (type-check-ty new-type-env (ast:decl-type-ty decl-type))))
                 (setf
                  (types:ty-ref-value (types:ty-name-ty-ref
                                       (types:get-type new-type-env (ast:decl-type-name decl-type))))
                  ty)
                 (check-type-circular-dependency ty (ast:decl-type-pos decl-type))))
             decl-types)
       (list new-type-env type-check-env)))
    ((ast:decl-functions decl-functions)
     (let ((name-exists-table (make-hash-table)))
       (mapc (lambda (decl-function)
               (let ((decl-function-name (ast:decl-function-name decl-function))
                     (decl-function-pos (ast:decl-function-pos decl-function)))
                 (when (gethash decl-function-name name-exists-table)
                   (type-check-error
                    decl-function-pos *line-map*
                    "Duplicate names ~A in a consecutive function declarations."
                    (symbol:sym-name decl-function-name)))
                 (setf (gethash decl-function-name name-exists-table) t)))
             decl-functions))
     (let ((new-type-check-env
             (reduce (lambda (acc-type-check-env decl-function)
                       (insert-type-check-entry
                        acc-type-check-env
                        (ast:decl-function-name decl-function)
                        (type-check-entry-fun
                         (mapcar (lambda (param-field)
                                   (let ((ty (types:get-type type-env (ast:field-type-id param-field))))
                                     (unless ty
                                       (type-check-error
                                        (ast:field-pos param-field) *line-map*
                                        "Undefined type ~A of param ~A of function ~A."
                                        (symbol:sym-name (ast:field-type-id param-field))
                                        (symbol:sym-name (ast:field-name param-field))
                                        (symbol:sym-name (ast:decl-function-name decl-function))))
                                     ty))
                                 (ast:decl-function-params decl-function))
                         (let ((decl-function-result (ast:decl-function-result decl-function)))
                           ;; decl-function-result form: (sym pos).
                           (let ((ty (if decl-function-result
                                         (types:get-type type-env (first decl-function-result))
                                         (types:get-unnamed-base-type (symbol:get-sym "unit")))))
                             (unless ty
                               (type-check-error
                                (second decl-function-result) *line-map*
                                "Undefined result type ~A of function ~A."
                                (symbol:sym-name (first decl-function-result))
                                (symbol:sym-name (ast:decl-function-name decl-function))))
                             ty)))))
                     decl-functions
                     :initial-value type-check-env)))
       (mapc (lambda (decl-function)
               (let ((type-check-entry
                       (get-type-check-entry new-type-check-env (ast:decl-function-name decl-function))))
                 (trivia:let-match1 (type-check-entry-fun formal-types result-ty) type-check-entry
                   (let ((body-ty
                           (type-check-expr
                            type-env
                            (loop with acc-type-check-env = new-type-check-env
                                  for formal-type in formal-types
                                  for param-field in (ast:decl-function-params decl-function)
                                  do (setf acc-type-check-env
                                           (insert-type-check-entry
                                            acc-type-check-env
                                            (ast:field-name param-field)
                                            (type-check-entry-var formal-type nil)))
                                  finally (return acc-type-check-env))
                            ;; Cannot break into the outer function.
                            nil
                            (ast:decl-function-body decl-function))))
                     (unless (types:type-compatible (types:actual-ty result-ty) body-ty)
                       (return-value-type-mismatch
                        (ast:decl-function-pos decl-function)
                        *line-map* result-ty body-ty
                        "Function ~A declared to return a value with type ~A, but actually return a value with type ~A."
                        (symbol:sym-name (ast:decl-function-name decl-function)) result-ty body-ty))))))
             decl-functions)
       (list type-env new-type-check-env)))))

(defun type-check-decls (type-env type-check-env within-loop decls)
  (reduce (lambda (acc decl)
            (type-check-decl (first acc) (second acc) within-loop decl))
          decls
          :initial-value (list type-env type-check-env)))

(defun type-check-expr (type-env type-check-env within-loop expr)
  (serapeum:match-of ast:expr expr
    ((ast:expr-var var)
     (trivia:let-match1 (type-check-var-result ty _)
         (type-check-var type-env type-check-env within-loop var)
       ty))
    ((ast:expr-nil)
     (types:get-unnamed-base-type (symbol:get-sym "nil")))
    ((ast:expr-int _)
     (types:get-type types:*base-type-env* (symbol:get-sym "int")))
    ((ast:expr-string _ _)
     (types:get-type types:*base-type-env* (symbol:get-sym "string")))
    ((ast:expr-call fun args pos)
     (alexandria:if-let (type-check-entry (get-type-check-entry type-check-env fun))
       (serapeum:match-of type-check-entry type-check-entry
         ((type-check-entry-fun formal-types result-type)
          (unless (eql (length args) (length formal-types))
            (type-check-error
             pos *line-map*
             "Function ~A expect ~A args but got ~A args."
             (symbol:sym-name fun) (length formal-types) (length args)))
          (loop for formal-type in formal-types
                for arg-type in (mapcar (lambda (arg)
                                          (type-check-expr type-env type-check-env within-loop arg))
                                        args)
                for i from 1
                do (unless (types:type-compatible (types:actual-ty formal-type) arg-type)
                     (type-check-error
                      pos *line-map*
                      "Function ~A ~Ath arg expect type ~A, but got a arg of type ~A."
                      (symbol:sym-name fun) i formal-type arg-type)))
          (types:actual-ty result-type))
         (_ (type-check-error
             pos *line-map*
             "You can only call a function.")))
       (undefined-fun
        pos *line-map* fun
        "Undefined function ~A." (symbol:sym-name fun))))
    ((ast:expr-op left op right pos)
     (let ((left-ty (type-check-expr type-env type-check-env within-loop left))
           (right-ty (type-check-expr type-env type-check-env within-loop right)))
       (trivia:match (list left-ty right-ty)
         ((list (types:ty-int) (types:ty-int))
          (types:get-type types:*base-type-env* (symbol:get-sym "int")))
         ((list (types:ty-string) (types:ty-string))
          (unless (serapeum:match-of ast:op op
                    ((or ast:op-eq ast:op-neq ast:op-lt ast:op-le ast:op-gt ast:op-ge)
                     t)
                    (_ nil))
            (unsupport-operation
             pos *line-map* op left-ty right-ty
             "Unsupport operation ~A on strings." op))
          (types:get-type types:*base-type-env* (symbol:get-sym "int")))
         (_
          (unless (and
                   (serapeum:match-of ast:op op
                     ((or ast:op-eq ast:op-neq)
                      t)
                     (_ nil))
                   (types:type-compatible left-ty right-ty))
            (unsupport-operation
             pos *line-map* op left-ty right-ty
             "Unsupport operation ~A." op))
          (types:get-type types:*base-type-env* (symbol:get-sym "int"))))))
    ((ast:expr-record type-id fields pos)
     (alexandria:if-let (ty (types:actual-ty (types:get-type type-env type-id)))
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
                      (let ((field-ty (type-check-expr
                                       type-env type-check-env within-loop (second field))))
                        (unless (types:type-compatible field-ty (types:actual-ty decl-field-ty))
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
           ty)
         (type-check-error
          pos *line-map*
          "Type ~A is not a record." (symbol:sym-name type-id)))
       (type-check-error
        pos *line-map*
        "Undefined type: ~A." (symbol:sym-name type-id))))
    ((ast:expr-seq exprs)
     (reduce (lambda (acc-type expr-with-pos)
               ;; expr-with-pos form: (expr pos).
               (declare (ignore acc-type))
               (type-check-expr type-env type-check-env within-loop (first expr-with-pos)))
             exprs
             :initial-value (types:get-unnamed-base-type (symbol:get-sym "unit"))))
    ((ast:expr-assign var expr pos)
     (trivia:let-match (((type-check-var-result var-ty is-index-var)
                         (type-check-var type-env type-check-env within-loop var))
                        (expr-ty (type-check-expr type-env type-check-env within-loop expr)))
       (when is-index-var
         (assign-index-var
          pos *line-map* var
          "Cannot assign an index variable."))
       (unless (types:type-compatible var-ty expr-ty)
         (type-mismatch-of-assignment
          pos *line-map* var-ty expr-ty
          "Type mismatch of the assignment, expected type (left): ~A, given value of type (right): ~A."
          var-ty expr-ty))
       (types:get-unnamed-base-type (symbol:get-sym "unit"))))
    ((ast:expr-if test then else pos)
     (let ((test-ty (type-check-expr type-env type-check-env within-loop test))
           (then-ty (type-check-expr type-env type-check-env within-loop then))
           (else-ty (when else (type-check-expr type-env type-check-env within-loop else))))
       (unless (types:type-compatible test-ty (types:get-type types:*base-type-env* (symbol:get-sym "int")))
         (type-check-error
          pos *line-map*
          "The type of the test expression of an if expression should be int."))
       (if else
           (unless (types:type-compatible then-ty else-ty)
             (then-else-types-of-if-mismatch
              pos *line-map*
              then-ty else-ty
              "The types of then and else branchs of an if expression should be the same."))
           (unless (types:type-compatible then-ty (types:get-unnamed-base-type (symbol:get-sym "unit")))
             (then-of-if-then-not-unit
              pos *line-map* then-ty
              "The then branch of an if-then expression should product no value.")))
       (if else (types:upgrade-from-compatible-types then-ty else-ty)
           (types:get-unnamed-base-type (symbol:get-sym "unit")))))
    ((ast:expr-while test body pos)
     (let ((test-ty (type-check-expr type-env type-check-env within-loop test))
           (body-ty (type-check-expr type-env type-check-env t body)))
       (unless (types:type-compatible test-ty (types:get-type types:*base-type-env* (symbol:get-sym "int")))
         (type-check-error
          pos *line-map*
          "The type of the test expression of a while expression should be int."))
       (unless (types:type-compatible body-ty (types:get-unnamed-base-type (symbol:get-sym "unit")))
         (body-of-while-not-unit
          pos *line-map* body-ty
          "The body expression of a while expression should product no value."))
       body-ty))
    ((ast:expr-for var low high body pos _)
     (let ((low-ty (type-check-expr type-env type-check-env within-loop low))
           (high-ty (type-check-expr type-env type-check-env within-loop high)))
       (let ((ty-int (types:get-type types:*base-type-env* (symbol:get-sym "int"))))
         (unless (types:type-compatible low-ty ty-int)
           (for-low-not-int
            pos *line-map* low-ty
            "The type of the low expression of a for expression should be int."))
         (unless (types:type-compatible high-ty ty-int)
           (for-high-not-int
            pos *line-map* high-ty
            "The type of the high expression of a for expression should be int."))
         (let ((new-type-check-env
                 (insert-type-check-entry type-check-env var (type-check-entry-var ty-int t))))
           (let ((body-ty (type-check-expr type-env new-type-check-env t body)))
             (unless (types:type-compatible body-ty (types:get-unnamed-base-type (symbol:get-sym "unit")))
               (type-check-error
                pos *line-map*
                "The body expression of a for expression should product no value."))
             body-ty)))))
    ((ast:expr-break pos)
     (unless within-loop
       (break-not-within-loop
        pos *line-map*
        "break expression not within a loop expression."))
     (types:get-unnamed-base-type (symbol:get-sym "unit")))
    ((ast:expr-array type-id size init pos)
     (let ((ty (types:actual-ty (types:get-type type-env type-id))))
       (trivia:if-match (types:ty-array base-type) ty
         (let ((size-ty (type-check-expr type-env type-check-env within-loop size))
               (init-ty (type-check-expr type-env type-check-env within-loop init)))
           (unless (types:type-compatible size-ty (types:get-type types:*base-type-env* (symbol:get-sym "int")))
             (type-check-error
              pos *line-map*
              "The type of size expression of array creation expression should be int."))
           (unless (types:type-compatible init-ty base-type)
             (type-check-error
              pos *line-map*
              "The type of init expression of array creation expression \
doesn't match the base type of the type ~A." type-id)))
         (type-check-error
          pos *line-map*
          "Type ~A is not an array." (symbol:sym-name type-id)))
       ty))
    ((ast:expr-let decls body _)
     (trivia:let-match1 (list new-type-env new-type-check-env)
         (type-check-decls type-env type-check-env within-loop decls)
       (type-check-expr new-type-env new-type-check-env within-loop body)))))

(defun type-check-program (prog &optional line-map)
  (let ((*line-map* line-map))
    (type-check-expr types:*base-type-env*
                     *base-type-check-env*
                     nil
                     prog)))
