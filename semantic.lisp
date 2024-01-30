(cl:defpackage :cl-tiger/semantic
  (:use :cl)
  (:local-nicknames
   (:symbol :cl-tiger/symbol)
   (:temp :cl-tiger/temp)
   (:utils :cl-tiger/utils)
   (:ast :cl-tiger/ast)
   (:types :cl-tiger/types)
   (:env :cl-tiger/env)
   (:trans :cl-tiger/translate))
  (:export
   #:type-check-program))

(cl:in-package :cl-tiger/semantic)

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

;; line-map can be nil
(defun type-check-error (pos line-map format &rest args)
  (error 'type-check-error :msg (apply #'format nil format args)
                           :pos pos :line-map line-map))

(defvar *line-map* nil)

(defun actual-ty (ty)
  (serapeum:match-of types:ty ty
    ((types:name-ty _ ty-ref)
     (if (null ty) ty (actual-ty (types:ty-ref-value ty-ref))))
    ((types:array-ty base-type) (types:array-ty (actual-ty base-type)))
    ;; NOTE: We don't need to recursive into record-ty since we use nominal type system.
    (_ ty)))

;; Compare two actual types.
(defun type-compatible (ty1 ty2)
  (or (eq ty1 ty2)
      (trivia:match (list ty1 ty2)
        ((list (types:array-ty base-type-1)
               (types:array-ty base-type-2))
         (type-compatible base-type-1 base-type-2))
        ((or (list (types:record-ty) (types:nil-ty))
             (list (types:nil-ty) (types:record-ty)))
         t)
        (_ nil))))

(defun upgarde-from-compatible-types (ty1 ty2)
  (trivia:match (list ty1 ty2)
    ((list (types:record-ty) (types:nil-ty))
     ty1)
    ((list (types:nil-ty) (types:record-ty))
     ty2)
    (_ ty1)))

(defun check-type-circular-dependencie (ty pos)
  (let ((visited (make-hash-table))
        (path nil))
    (labels ((rec (ty)
               (serapeum:match-of types:ty ty
                 ((types:name-ty sym ty-ref)
                  (push (symbol:sym-name sym) path)
                  (when (gethash sym visited)
                    (type-check-error
                     pos *line-map*
                     "Circular type dependencie: ~{~A~^ -> ~}." (reverse path)))
                  (alexandria:when-let (type-ref-value (types:ty-ref-value ty-ref))
                    (setf (gethash sym visited) t)
                    (rec type-ref-value)))
                 (_ nil))))
      (rec ty)
      (values))))

(defun type-check-ty (type-env ty)
  (serapeum:match-of ast:ty ty
    ((ast:name-ty name pos)
     (let ((ty (env:get-type type-env name)))
       (unless ty
         (type-check-error pos *line-map* "Undefined type: ~A." (symbol:sym-name name)))
       ty))
    ((ast:record-ty fields)
     (types:record-ty
      (mapcar (lambda  (field)
                (trivia:let-match1 (ast:field name type-id pos _) field
                  (let ((ty (env:get-type type-env type-id)))
                    (unless ty
                      (type-check-error
                       pos *line-map*
                       "The type of field ~A is an undefined type: ~A."
                       (symbol:sym-name name)
                       (symbol:sym-name type-id)))
                    (list name ty))))
              fields)))
    ((ast:array-ty base-type-id pos)
     (let ((ty (env:get-type type-env base-type-id)))
       (unless ty
         (type-check-error
          pos
          *line-map*
          "Undefined base type of an array: ~A."
          (symbol:sym-name base-type-id)))
       (types:array-ty ty)))))

(defun type-check-and-translate-var (value-env level target var)
  (serapeum:match-of ast:var var
    ((ast:simple-var sym pos)
     (alexandria:if-let (value-entry (env:get-value value-env sym))
       (serapeum:match-of env:value-entry value-entry
         ((env:var-entry ty _)
          (actual-ty ty))
         ((env:fun-entry _ _ _ _)
          (type-check-error
           pos *line-map*
           "simple-var cannot reference to a function.")))
       (type-check-error
        pos *line-map*
        "Undefined variable: ~A." (symbol:sym-name sym))))
    ((ast:field-var var sym pos)
     (let ((type (type-check-and-translate-var value-env level target var)))
       (serapeum:match-of types:ty type
         ((types:record-ty fields)
          (alexandria:if-let
              (field
               (find-if (lambda (field)
                          ;; field is of the form (sym ty)
                          (eq (first field) sym))
                        fields))
            (actual-ty (second field))
            (type-check-error
             pos *line-map*
             "Unknow field of the record.")))
         (_ (type-check-error
             pos *line-map*
             "You can only access the field of a record.")))))
    ((ast:subscript-var var _ pos)
     (let ((type (type-check-and-translate-var value-env level target var)))
       (serapeum:match-of types:ty type
         ((types:array-ty base-type)
          (actual-ty base-type))
         (_ (type-check-error
             pos *line-map*
             "You can only subscript an array.")))))))

(defun type-check-and-translate-decl (type-env value-env level target decl within-loop)
  (serapeum:match-of ast:decl decl
    ((ast:var-decl name typ init pos escape-ref)
     ;; typ form: (sym pos) or nil.
     (when typ
       (unless (env:get-type type-env (first typ))
         (type-check-error
          (second typ) *line-map*
          "Undefined type: ~A." (symbol:sym-name (first typ)))))
     (let ((init-ty (type-check-and-translate-expr type-env value-env init within-loop)))
       (when typ
         (unless (type-compatible init-ty (actual-ty (env:get-type type-env (first typ))))
           (type-check-error
            pos *line-map*
            "The type of the init expression of the variable doesn't the type ~A."
            (symbol:sym-name (first typ)))))
       (list type-env
             (env:insert-value
              value-env name
              (env:var-entry
               init-ty
               (trans:alloc-local
                level (ast:escape-ref-value escape-ref) target))))))
    ((ast:type-decls type-decls)
     (let ((name-exists-table (make-hash-table)))
       (mapc (lambda (type-decl)
               (let ((type-decl-name (ast:type-decl-name type-decl))
                     (type-decl-pos (ast:type-decl-pos type-decl)))
                 (when (gethash type-decl-name name-exists-table)
                   (type-check-error
                    type-decl-pos *line-map*
                    "Duplicate names ~A in a consecutive type declarations."
                    (symbol:sym-name type-decl-name)))
                 (setf (gethash type-decl-name name-exists-table) t)
                 ;; (when (gethash type-decl-name env:*base-type-env*)
                 ;;   (type-check-error
                 ;;    type-decl-pos *line-map*
                 ;;    "Cannot shadow built-in type: ~A."
                 ;;    (symbol:sym-name type-decl-name)))
                 ))
             type-decls))
     (let ((new-type-env
             (reduce (lambda (acc-type-env type-decl)
                       (env:insert-type acc-type-env
                                        (ast:type-decl-name type-decl)
                                        (types:name-ty (ast:type-decl-name type-decl) (types:ty-ref nil))))
                     type-decls
                     :initial-value type-env)))
       (mapc (lambda (type-decl)
               (let ((ty (type-check-ty new-type-env (ast:type-decl-ty type-decl))))
                 (setf
                  (types:ty-ref-value (types:name-ty-ty-ref
                                       (env:get-type new-type-env (ast:type-decl-name type-decl))))
                  ty)
                 (check-type-circular-dependencie ty (ast:type-decl-pos type-decl))))
             type-decls)
       (list new-type-env value-env)))
    ((ast:function-decls function-decls)
     (let ((name-exists-table (make-hash-table)))
       (mapc (lambda (function-decl)
               (let ((function-decl-name (ast:function-decl-name function-decl))
                     (function-decl-pos (ast:function-decl-pos function-decl)))
                 (when (gethash function-decl-name name-exists-table)
                   (type-check-error
                    function-decl-pos *line-map*
                    "Duplicate names ~A in a consecutive function declarations."
                    (symbol:sym-name function-decl-name)))
                 (setf (gethash function-decl-name name-exists-table) t)))
             function-decls))
     (let ((new-value-env
             (reduce (lambda (acc-value-env function-decl)
                       (let ((name (temp:new-label (symbol:sym-name (ast:function-decl-name function-decl)))))
                         (env:insert-value
                          acc-value-env
                          (ast:function-decl-name function-decl)
                          (env:fun-entry
                           (mapcar (lambda (param-field)
                                     (let ((ty (env:get-type type-env (ast:field-type-id param-field))))
                                       (unless ty
                                         (type-check-error
                                          (ast:field-pos param-field) *line-map*
                                          "Undefined type ~A of param ~A of function ~A."
                                          (symbol:sym-name (ast:field-type-id param-field))
                                          (symbol:sym-name (ast:field-name param-field))
                                          (symbol:sym-name (ast:function-decl-name function-decl))))
                                       ty))
                                   (ast:function-decl-params function-decl))
                           (let ((function-decl-result (ast:function-decl-result function-decl)))
                             ;; function-decl-result form: (sym pos).
                             (let ((ty (if function-decl-result
                                           (env:get-type type-env (first function-decl-result))
                                           (env:get-unnamed-base-type (symbol:get-sym "unit")))))
                               (unless ty
                                 (type-check-error
                                  (second function-decl-result) *line-map*
                                  "Undefined result type ~A of function ~A."
                                  (symbol:sym-name (first function-decl-result))
                                  (symbol:sym-name (ast:function-decl-name function-decl))))
                               ty))
                           name
                           (trans:new-level level name
                                            (mapcar (lambda (param-field)
                                                      (ast:escape-ref-value
                                                       (ast:field-escape-ref param-field)))
                                                    (ast:function-decl-params function-decl))
                                            target)))))
                     function-decls
                     :initial-value value-env)))
       (mapc (lambda (function-decl)
               (let ((value-entry (env:get-value new-value-env (ast:function-decl-name function-decl))))
                 (trivia:let-match1 (env:fun-entry formal-types _ _ level) value-entry
                   (type-check-and-translate-expr
                    type-env
                    (loop with acc-value-env = new-value-env
                          for formal-type in formal-types
                          for param-field in (ast:function-decl-params function-decl)
                          for formal-access in (trans:level-formals level)
                          do (setf acc-value-env
                                   (env:insert-value acc-value-env
                                                     (ast:field-name param-field)
                                                     (env:var-entry
                                                      formal-type
                                                      formal-access)))
                          finally (return acc-value-env))
                    level
                    target
                    (ast:function-decl-body function-decl)
                    ;; Cannot break into the outer function.
                    nil))))
             function-decls)
       (list type-env new-value-env)))))

(defun type-check-and-translate-decls (type-env value-env level target decls within-loop)
  (reduce (lambda (acc decl)
            (type-check-and-translate-decl (first acc) (second acc) level target decl within-loop))
          decls
          :initial-value (list type-env value-env)))

(defun type-check-and-translate-expr (type-env value-env level target expr within-loop)
  (serapeum:match-of ast:expr expr
    ((ast:var-expr var)
     (type-check-and-translate-var value-env level target var))
    ((ast:nil-expr)
     (env:get-unnamed-base-type (symbol:get-sym "nil")))
    ((ast:int-expr _)
     (env:get-type env:*base-type-env* (symbol:get-sym "int")))
    ((ast:string-expr _ _)
     (env:get-type env:*base-type-env* (symbol:get-sym "string")))
    ((ast:call-expr fun args pos)
     (alexandria:if-let (value-entry (env:get-value value-env fun))
       (serapeum:match-of env:value-entry value-entry
         ((env:fun-entry formal-types result-type _ _)
          (unless (eql (length args) (length formal-types))
            (type-check-error
             pos *line-map*
             "Function ~A expect ~A args but got ~A args."
             (symbol:sym-name fun) (length formal-types) (length args)))
          (loop for formal-type in formal-types
                for arg-type in (mapcar (lambda (arg)
                                          (type-check-and-translate-expr type-env value-env level target arg within-loop))
                                        args)
                for i from 1
                do (unless (type-compatible (actual-ty formal-type) arg-type)
                     (type-check-error
                      pos *line-map*
                      "Function ~A ~Ath arg expect type ~A, but got a arg of type ~A."
                      (symbol:sym-name fun) i formal-type arg-type)))
          (actual-ty result-type))
         (_ (type-check-error
             pos *line-map*
             "You can only call a function.")))
       (type-check-error
        pos *line-map*
        "Undefined function ~A." (symbol:sym-name fun))))
    ((ast:op-expr left op right pos)
     (let ((left-ty (type-check-and-translate-expr type-env value-env level target left within-loop))
           (right-ty (type-check-and-translate-expr type-env value-env level target right within-loop)))
       (trivia:match (list left-ty right-ty)
         ((list (types:int-ty) (types:int-ty))
          (env:get-type env:*base-type-env* (symbol:get-sym "int")))
         ((list (types:string-ty) (types:string-ty))
          (unless (serapeum:match-of ast:op op
                    ((or ast:eq-op ast:neq-op ast:lt-op ast:le-op ast:gt-op ast:ge-op)
                     t)
                    (_ nil))
            (type-check-error
             pos *line-map*
             "Unsupport operation ~A on strings." op))
          (env:get-type env:*base-type-env* (symbol:get-sym "int")))
         (_
          (unless (and
                   (serapeum:match-of ast:op op
                     ((or ast:eq-op ast:neq-op)
                      t)
                     (_ nil))
                   (type-compatible left-ty right-ty))
            (type-check-error
             pos *line-map*
             "Unsupport operation ~A." op))
          (env:get-type env:*base-type-env* (symbol:get-sym "int"))))))
    ((ast:record-expr type-id fields pos)
     (alexandria:if-let (ty (actual-ty (env:get-type type-env type-id)))
       (trivia:if-match (types:record-ty decl-fields) ty
         (progn
           (loop for decl-field in decl-fields
                 ;; decl-field form: (sym ty).
                 for decl-field-sym = (first decl-field)
                 for decl-field-ty = (second decl-field)
                 do
                    (alexandria:if-let
                        (field (find-if (lambda (field)
                                          ;; field form: (sym expr pos)
                                          (eql (first field) decl-field-sym))
                                        fields))
                      (let ((field-ty (type-check-and-translate-expr
                                       type-env value-env level target (second field) within-loop)))
                        (unless (type-compatible field-ty (actual-ty decl-field-ty))
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
                       pos  *line-map*
                       "Unknown field ~A of the record." (symbol:sym-name field-sym))))
           ty)
         (type-check-error
          pos *line-map*
          "Type ~A is not a record." (symbol:sym-name type-id)))
       (type-check-error
        pos *line-map*
        "Undefined type: ~A." (symbol:sym-name type-id))))
    ((ast:seq-expr exprs)
     (reduce (lambda (acc-type expr-with-pos)
               ;; expr-with-pos form: (expr pos).
               (declare (ignore acc-type))
               (type-check-and-translate-expr type-env value-env level target (first expr-with-pos) within-loop))
             exprs
             :initial-value (env:get-unnamed-base-type (symbol:get-sym "unit"))))
    ((ast:assign-expr var expr pos)
     (let ((var-ty (type-check-and-translate-var value-env level target var))
           (expr-ty (type-check-and-translate-expr type-env value-env level target expr within-loop)))
       (unless (type-compatible var-ty expr-ty)
         (type-check-error
          pos *line-map*
          "Assignment type mismatch."))
       (env:get-unnamed-base-type (symbol:get-sym "unit"))))
    ((ast:if-expr test then else pos)
     (let ((test-ty (type-check-and-translate-expr type-env value-env level target test within-loop))
           (then-ty (type-check-and-translate-expr type-env value-env level target then within-loop))
           (else-ty (when else (type-check-and-translate-expr type-env value-env level target else within-loop))))
       (unless (type-compatible test-ty (env:get-type env:*base-type-env* (symbol:get-sym "int")))
         (type-check-error
          pos *line-map*
          "The type of the test expression of an if expression should be int."))
       (if else
           (unless (type-compatible then-ty else-ty)
             (type-check-error
              pos *line-map*
              "The types of then and else branchs of an if expression should be the same."))
           (unless (type-compatible then-ty (env:get-unnamed-base-type (symbol:get-sym "unit")))
             (type-check-error
              pos *line-map*
              "The then branch of an if-then expression should product no value.")))
       (if else (upgarde-from-compatible-types then-ty else-ty)
           (env:get-unnamed-base-type (symbol:get-sym "unit")))))
    ((ast:while-expr test body pos)
     (let ((test-ty (type-check-and-translate-expr type-env value-env level target test within-loop))
           (body-ty (type-check-and-translate-expr type-env value-env level target body t)))
       (unless (type-compatible test-ty (env:get-type env:*base-type-env* (symbol:get-sym "int")))
         (type-check-error
          pos *line-map*
          "The type of the test expression of a while expression should be int."))
       (unless (type-compatible body-ty (env:get-unnamed-base-type (symbol:get-sym "unit")))
         (type-check-error
          pos *line-map*
          "The body expression of a while expression should product no value."))
       body-ty))
    ((ast:for-expr var low high body pos escape-ref)
     (let ((low-ty (type-check-and-translate-expr type-env value-env level target low within-loop))
           (high-ty (type-check-and-translate-expr type-env value-env level target high within-loop)))
       (let ((int-ty (env:get-type env:*base-type-env* (symbol:get-sym "int"))))
         (unless (type-compatible low-ty int-ty)
           (type-check-error
            pos *line-map*
            "The type of the low expression of a for expression should be int."))
         (unless (type-compatible high-ty int-ty)
           (type-check-error
            pos *line-map*
            "The type of the high expression of a for expression should be int."))
         (let ((new-value-env
                 (env:insert-type
                  value-env var
                  (env:var-entry
                   int-ty
                   (trans:alloc-local level (ast:escape-ref-value escape-ref) target)))))
           (let ((body-ty (type-check-and-translate-expr type-env new-value-env level target body t)))
             (unless (type-compatible body-ty (env:get-unnamed-base-type (symbol:get-sym "unit")))
               (type-check-error
                pos *line-map*
                "The body expression of a for expression should product no value."))
             body-ty)))))
    ((ast:break-expr pos)
     (unless within-loop
       (type-check-error
        pos *line-map*
        "break expression not within a loop expression."))
     (env:get-unnamed-base-type (symbol:get-sym "unit")))
    ((ast:array-expr type-id size init pos)
     (let ((ty (actual-ty (env:get-type type-env type-id))))
       (trivia:if-match (types:array-ty base-type) ty
         (let ((size-ty (type-check-and-translate-expr type-env value-env level target size within-loop))
               (init-ty (type-check-and-translate-expr type-env value-env level target init within-loop)))
           (unless (type-compatible size-ty (env:get-type env:*base-type-env* (symbol:get-sym "int")))
             (type-check-error
              pos *line-map*
              "The type of size expression of array creation expression should be int."))
           (unless (type-compatible init-ty base-type)
             (type-check-error
              pos *line-map*
              "The type of init expression of array creation expression \
doesn't match the base type of the type ~A." type-id)))
         (type-check-error
          pos *line-map*
          "Type ~A is not an array." (symbol:sym-name type-id)))
       ty))
    ((ast:let-expr decls body _)
     (trivia:let-match1 (list new-type-env new-value-env)
         (type-check-and-translate-decls type-env value-env level target decls within-loop)
       (type-check-and-translate-expr new-type-env new-value-env level target body within-loop)))))

(defun type-check-program (prog target &optional line-map)
  (let ((*line-map* line-map)) 
    (type-check-and-translate-expr env:*base-type-env*
                     (env:base-value-env target)
                     (trans:new-level trans:top-level (temp:new-label "_tiger_program") nil target)
                     target
                     prog
                     nil)))
