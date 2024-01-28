(cl:defpackage :cl-tiger/type-check
  (:use :cl)
  (:local-nicknames
   (:symbol :cl-tiger/symbol)
   (:ast :cl-tiger/ast)
   (:types :cl-tiger/types)
   (:env :cl-tiger/env))
  (:export
   #:type-check-program))

(cl:in-package :cl-tiger/type-check)

(defun actual-ty (ty)
  (trivia:match ty
    ((types:name-ty :ty ty)
     (if (null ty) ty (actual-ty ty)))
    ((types:array-ty :base-type base-type) (types:make-array-ty (actual-ty base-type)))
    ;; NOTE: We don't need to recursive into record-ty since we use nominal type system.
    (_ ty)))

;; Compare two actual types.
(defun type-compatible (ty1 ty2)
  (or (eq ty1 ty2)
      (trivia:match (list ty1 ty2)
        ((list (types:array-ty :base-type base-type-1)
               (types:array-ty :base-type base-type-2))
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

(defun check-type-circular-dependencie (ty)
  (let ((visited (make-hash-table))
        (path nil))
    (labels ((rec (ty)
               (trivia:match ty
                 ((types:name-ty :sym sym :ty ty)
                  (push (symbol:sym-name sym) path)
                  (when (gethash sym visited)
                    (error "Circular type dependencie: ~{~A~^ -> ~}" (reverse path)))
                  (when ty
                    (setf (gethash sym visited) t)
                    (rec ty)))
                 (_ nil))))
      (rec ty)
      (values))))

(defun type-check-ty (type-env ty)
  (trivia:match ty
    ((ast:name-ty :name name :pos pos)
     (let ((ty (env:get-type type-env name)))
       (unless ty
         (error "Undefined type: ~A (pos: ~A)."
                (symbol:sym-name name)
                pos))
       ty))
    ((ast:record-ty :fields fields)
     (types:make-record-ty
      (mapcar (lambda  (field)
                (trivia:let-match1 (ast:field :name name :type-id type-id :pos pos) field
                  (let ((ty (env:get-type type-env type-id)))
                    (unless ty
                      (error "The type of field ~A is an undefined type: ~A (pos: ~A)."
                             (symbol:sym-name name)
                             (symbol:sym-name type-id)
                             pos))
                    (list name ty))))
              fields)))
    ((ast:array-ty :base-type-id base-type-id :pos pos)
     (let ((ty (env:get-type type-env base-type-id)))
       (unless ty
         (error "Undefined base type of an array: ~A (pos: ~A)."
                (symbol:sym-name base-type-id)
                pos))
       (types:make-array-ty ty)))))

(defun type-check-var (value-env var)
  (trivia:ematch var
    ((ast:simple-var :sym sym :pos pos)
     (alexandria:if-let (value-entry (env:get-value value-env sym))
       (trivia:ematch value-entry
         ((env:var-entry :ty ty)
          (actual-ty ty))
         ((env:fun-entry)
          (error "simple-var cannot reference to a function (pos: ~A)." pos)))
       (error "Undefined variable: ~A (pos: ~A)." (symbol:sym-name sym) pos)))
    ((ast:field-var :var var :sym sym :pos pos)
     (let ((type (type-check-var value-env var)))
       (trivia:match type
         ((types:record-ty :fields fields)
          (alexandria:if-let
              (field
               (find-if (lambda (field)
                          ;; field is of the form (sym ty)
                          (eq (first field) sym))
                        fields))
            (actual-ty (second field))
            (error "Unknow field of the record (pos: ~A)." pos)))
         (_ (error "You can only access the field of a record (pos: ~A)." pos)))))
    ((ast:subscript-var :var var :pos pos)
     (let ((type (type-check-var value-env var)))
       (trivia:match type
         ((types:array-ty :base-type base-type)
          (actual-ty base-type))
         (_ (error "You can only subscript an array (pos: ~A)." pos)))))))

(defun type-check-decl (type-env value-env decl within-loop)
  (trivia:ematch decl
    ((ast:var-decl :name name :typ typ :init init :pos pos)
     ;; typ form: (sym pos) or nil.
     (when typ
       (unless (env:get-type type-env (first typ))
         (error "Undefined type: ~A (pos: ~A)." (symbol:sym-name (first typ)) (second typ))))
     (let ((init-ty (type-check-expr type-env value-env init within-loop)))
       (when typ
         (unless (type-compatible init-ty (actual-ty (env:get-type type-env (first typ))))
           (error "The type of the init expression of the variable doesn't the type ~A (pos: ~A)."
                  (symbol:sym-name (first typ)) pos)))
       (list type-env (env:insert-value value-env name (env:make-var-entry init-ty)))))
    ((ast:type-decls :decls type-decls)
     (let ((name-exists-table (make-hash-table)))
       (mapc (lambda (type-decl)
               (let ((type-decl-name (ast:type-decl-name type-decl))
                     (type-decl-pos (ast:type-decl-pos type-decl)))
                 (when (gethash type-decl-name name-exists-table)
                   (error "Duplicate names ~A in a consecutive type declarations (pos: ~A)."
                          (symbol:sym-name type-decl-name)
                          type-decl-pos))
                 (setf (gethash type-decl-name name-exists-table) t)
                 ;; (when (gethash type-decl-name env:*base-type-env*)
                 ;;   (error "Cannot shadow built-in type: ~A (pos: ~A)."
                 ;;          (symbol:sym-name type-decl-name)
                 ;;          type-decl-pos))
                 ))
             type-decls))
     (let ((new-type-env
             (reduce (lambda (acc-type-env type-decl)
                       (env:insert-type acc-type-env
                                        (ast:type-decl-name type-decl)
                                        (types:make-name-ty (ast:type-decl-name type-decl))))
                     type-decls
                     :initial-value type-env)))
       (mapc (lambda (type-decl)
               (let ((type-decl-ty (ast:type-decl-ty type-decl)))
                 (let ((ty (type-check-ty new-type-env type-decl-ty)))
                   (setf (types:name-ty-ty (env:get-type new-type-env (ast:type-decl-name type-decl)))
                         ty)
                   (check-type-circular-dependencie ty))))
             type-decls)
       (list new-type-env value-env)))
    ((ast:function-decls :decls function-decls)
     (let ((name-exists-table (make-hash-table)))
       (mapc (lambda (function-decl)
               (let ((function-decl-name (ast:function-decl-name function-decl))
                     (function-decl-pos (ast:function-decl-pos function-decl)))
                 (when (gethash function-decl-name name-exists-table)
                   (error "Duplicate names ~A in a consecutive function declarations (pos: ~A)."
                          (symbol:sym-name function-decl-name)
                          function-decl-pos))
                 (setf (gethash function-decl-name name-exists-table) t)))
             function-decls))
     (let ((new-value-env
             (reduce (lambda (acc-value-env function-decl)
                       (env:insert-value
                        acc-value-env
                        (ast:function-decl-name function-decl)
                        (env:make-fun-entry
                         (mapcar (lambda (param-field)
                                   (let ((ty (env:get-type type-env (ast:field-type-id param-field))))
                                     (unless ty
                                       (error "Undefined type ~A of param ~A of function ~A (pos: ~A)."
                                              (symbol:sym-name (ast:field-type-id param-field))
                                              (symbol:sym-name (ast:field-name param-field))
                                              (symbol:sym-name (ast:function-decl-name function-decl))
                                              (ast:field-pos param-field)))
                                     ty))
                                 (ast:function-decl-params function-decl))
                         (let ((function-decl-result (ast:function-decl-result function-decl)))
                           ;; function-decl-result form: (sym pos).
                           (let ((ty (if function-decl-result
                                         (env:get-type type-env (first function-decl-result))
                                         (env:get-unnamed-base-type (symbol:get-sym "unit")))))
                             (unless ty
                               (error "Undefined result type ~A of function ~A (pos: ~A)."
                                       (symbol:sym-name (first function-decl-result))
                                       (symbol:sym-name (ast:function-decl-name function-decl))
                                       (second function-decl-result)))
                             ty)))))
                     function-decls
                     :initial-value value-env)))
       (mapc (lambda (function-decl)
               (let ((value-entry (env:get-value new-value-env (ast:function-decl-name function-decl))))
                 (trivia:let-match1 (env:fun-entry :formal-types formal-types) value-entry
                   (type-check-expr
                    type-env
                    (loop with acc-value-env = new-value-env
                          for formal-type in formal-types
                          for param-field in (ast:function-decl-params function-decl)
                          do (setf acc-value-env
                                   (env:insert-value acc-value-env
                                                     (ast:field-name param-field)
                                                     (env:make-var-entry formal-type)))
                          finally (return acc-value-env))
                    (ast:function-decl-body function-decl)
                    ;; Cannot break into the outer function.
                    nil))))
             function-decls)
       (list type-env new-value-env)))))

(defun type-check-decls (type-env value-env decls within-loop)
  (reduce (lambda (acc decl)
            (type-check-decl (first acc) (second acc) decl within-loop))
          decls
          :initial-value (list type-env value-env)))

(defun type-check-expr (type-env value-env expr within-loop)
  (trivia:ematch expr
    ((ast:var-expr :var var)
     (type-check-var value-env var))
    ((ast:nil-expr)
     (env:get-unnamed-base-type (symbol:get-sym "nil")))
    ((ast:int-expr)
     (env:get-type env:*base-type-env* (symbol:get-sym "int")))
    ((ast:string-expr)
     (env:get-type env:*base-type-env* (symbol:get-sym "string")))
    ((ast:call-expr :fun fun :args args :pos pos)
     (alexandria:if-let (value-entry (env:get-value value-env fun))
       (trivia:match value-entry
         ((env:fun-entry :formal-types formal-types :result-type result-type)
          (unless (eql (length args) (length formal-types))
            (error "Function ~A expect ~A args but got ~A args (pos: ~A)."
                   (symbol:sym-name fun) (length formal-types) (length args) pos))
          (loop for formal-type in formal-types
                for arg-type in (mapcar (lambda (arg) (type-check-expr type-env value-env arg within-loop)) args)
                for i from 1
                do (unless (type-compatible (actual-ty formal-type) arg-type)
                     (error "Function ~A ~Ath arg expect type ~A, but got a arg of type ~A (pos: ~A)"
                            (symbol:sym-name fun) i formal-type arg-type pos)))
          (actual-ty result-type))
         (_ (error "You can only call a function (pos: ~A)." pos)))
       (error "Undefined function ~A (pos: ~A)." (symbol:sym-name fun) pos)))
    ((ast:op-expr :left left :op op :right right :pos pos)
     (let ((left-ty (type-check-expr type-env value-env left within-loop))
           (right-ty (type-check-expr type-env value-env right within-loop)))
       (trivia:match (list left-ty right-ty)
         ((list (types:int-ty) (types:int-ty))
          (env:get-type env:*base-type-env* (symbol:get-sym "int")))
         ((list (types:string-ty) (types:string-ty))
          (unless (member op '(:eq :neq :lt :le :gt :ge))
            (error "Unsupport operation ~A on strings (pos: ~A)." op pos))
          (env:get-type env:*base-type-env* (symbol:get-sym "int")))
         (_
          (unless (and (member op '(:eq :neq))
                       (type-compatible left-ty right-ty))
            (error "Unsupport operation ~A (pos: ~A)." op pos))
          (env:get-type env:*base-type-env* (symbol:get-sym "int"))))))
    ((ast:record-expr :type-id type-id :fields fields :pos pos)
     (alexandria:if-let (ty (actual-ty (env:get-type type-env type-id)))
       (trivia:if-match (types:record-ty :fields decl-fields) ty
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
                      (let ((field-ty (type-check-expr type-env value-env (second field) within-loop)))
                        (unless (type-compatible field-ty (actual-ty decl-field-ty))
                          (error "The type of the init expression of the field ~A \
doesn't match the expected type (pos: ~A)."
                                 (symbol:sym-name decl-field-sym)
                                 (third field))))
                      (error "Missing the init expression of the field: ~A (pos: ~A)."
                             (symbol:sym-name decl-field-sym) pos)))
           (loop for field in fields
                 ;; field form: (sym expr pos)
                 for field-sym = (first field)
                 do (unless (find-if (lambda (decl-field)
                                       ;; decl-field form: (sym ty).
                                       (eql (first decl-field) field-sym))
                                     decl-fields)
                      (error "Unknown field ~A of the record (pos: ~A)." (symbol:sym-name field-sym) pos)))
           ty)
         (error "Type ~A is not a record (pos: ~A)." (symbol:sym-name type-id) pos))
       (error "Undefined type: ~A (pos: ~A)." (symbol:sym-name type-id) pos)))
    ((ast:seq-expr :exprs exprs)
     (reduce (lambda (acc-type expr-with-pos)
               ;; expr-with-pos form: (expr pos).
               (declare (ignore acc-type))
               (type-check-expr type-env value-env (first expr-with-pos) within-loop))
             exprs
             :initial-value (env:get-unnamed-base-type (symbol:get-sym "unit"))))
    ((ast::assign-expr :var var :expr expr :pos pos)
     (let ((var-ty (type-check-var value-env var))
           (expr-ty (type-check-expr type-env value-env expr within-loop)))
       (unless (type-compatible var-ty expr-ty)
         (error "Assignment type mismatch (pos: ~A)." pos))
       (env:get-unnamed-base-type (symbol:get-sym "unit"))))
    ((ast:if-expr :test test :then then :else else :pos pos)
     (let ((test-ty (type-check-expr type-env value-env test within-loop))
           (then-ty (type-check-expr type-env value-env then within-loop))
           (else-ty (when else (type-check-expr type-env value-env else within-loop))))
       (unless (type-compatible test-ty (env:get-type env:*base-type-env* (symbol:get-sym "int")))
         (error "The type of the test expression of an if expression should be int (pos: ~A)." pos))
       (if else
           (unless (type-compatible then-ty else-ty)
             (error "The types of then and else branchs of an if expression should be the same (pos: ~A)." pos))
           (unless (type-compatible then-ty (env:get-unnamed-base-type (symbol:get-sym "unit")))
             (error "The then branch of an if-then expression should product no value (pos: ~A)." pos)))
       (if else (upgarde-from-compatible-types then-ty else-ty)
           (env:get-unnamed-base-type (symbol:get-sym "unit")))))
    ((ast:while-expr :test test :body body :pos pos)
     (let ((test-ty (type-check-expr type-env value-env test within-loop))
           (body-ty (type-check-expr type-env value-env body t)))
       (unless (type-compatible test-ty (env:get-type env:*base-type-env* (symbol:get-sym "int")))
         (error "The type of the test expression of a while expression should be int (pos: ~A)" pos))
       (unless (type-compatible body-ty (env:get-unnamed-base-type (symbol:get-sym "unit")))
         (error "The body expression of a while expression should product no value (pos: ~A)." pos))
       body-ty))
    ((ast:for-expr :var var :low low :high high :body body :pos pos)
     (let ((low-ty (type-check-expr type-env value-env low within-loop))
           (high-ty (type-check-expr type-env value-env high within-loop)))
       (let ((int-ty (env:get-type env:*base-type-env* (symbol:get-sym "int"))))
         (unless (type-compatible low-ty int-ty)
           (error "The type of the low expression of a for expression should be int (pos: ~A)." pos))
         (unless (type-compatible high-ty int-ty)
           (error "The type of the high expression of a for expression should be int (pos: ~A)." pos))
         (let ((new-value-env (env:insert-type value-env var (env:make-var-entry int-ty))))
           (let ((body-ty (type-check-expr type-env new-value-env body t)))
             (unless (type-compatible body-ty (env:get-unnamed-base-type (symbol:get-sym "unit")))
               (error "The body expression of a for expression should product no value (pos: ~A)." pos))
             body-ty)))))
    ((ast:break-expr :pos pos)
     (unless within-loop
       (error "break expression not within a loop expression (pos: ~A)." pos))
     (env:get-unnamed-base-type (symbol:get-sym "unit")))
    ((ast:array-expr :type-id type-id :size size :init init :pos pos)
     (let ((ty (actual-ty (env:get-type type-env type-id))))
       (trivia:if-match (types:array-ty :base-type base-type) ty
         (let ((size-ty (type-check-expr type-env value-env size within-loop))
               (init-ty (type-check-expr type-env value-env init within-loop)))
           (unless (type-compatible size-ty (env:get-type env:*base-type-env* (symbol:get-sym "int")))
             (error "The type of size expression of array creation expression should be int (pos: ~A)." pos))
           (unless (type-compatible init-ty base-type)
             (error "The type of init expression of array creation expression \
doesn't match the base type of the type ~A (pos: ~A)." type-id pos)))
         (error "Type ~A is not an array (pos: ~A)." (symbol:sym-name type-id) pos))
       ty))
    ((ast:let-expr :decls decls :body body)
     (trivia:let-match1
         (list new-type-env new-value-env) (type-check-decls type-env value-env decls within-loop)
       (type-check-expr new-type-env new-value-env body within-loop)))))

(defun type-check-program (prog)
  (type-check-expr env:*base-type-env*  env:*base-value-env* prog nil))
