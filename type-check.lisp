(cl:defpackage :cl-tiger/type-check
  (:use :cl)
  (:local-nicknames
   (:symbol :cl-tiger/symbol)
   (:utils :cl-tiger/utils)
   (:ast :cl-tiger/ast)
   (:types :cl-tiger/types)
   (:cl-ds :cl-data-structures)
   (:cl-ds.hamt :cl-data-structures.dicts.hamt))
  (:export
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

;; line-map can be nil
(defun type-check-error (pos line-map format &rest args)
  (error 'type-check-error :msg (apply #'format nil format args)
                           :pos pos :line-map line-map))

(defvar *line-map* nil)

(serapeum:defunion type-check-entry
  (type-check-var-entry
   (ty types:ty))
  (type-check-fun-entry
   ;; A list of types:ty
   (formal-types list)
   (result-type types:ty)))

(defun get-type-check-entry (type-check-env sym)
  (cl-ds:at type-check-env sym))

(defun insert-type-check-entry (type-check-env sym type-check-entry)
  (cl-ds:insert type-check-env sym type-check-entry))

(defvar *base-type-check-env*
  (let ((env (cl-ds.hamt:make-functional-hamt-dictionary #'sxhash #'eq)))
    (reduce (lambda (env binding)
              (trivia:let-match1 (list name formal-types result-type) binding
                (cl-ds:insert env
                              (symbol:get-sym name)
                              (type-check-fun-entry formal-types result-type))))
            types:*built-in-function-bindings*
            :initial-value env)))

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
                     "Circular type dependencie: ~{~A~^ -> ~}." (nreverse path)))
                  (alexandria:when-let (type-ref-value (types:ty-ref-value ty-ref))
                    (setf (gethash sym visited) t)
                    (rec type-ref-value)))
                 (_ nil))))
      (rec ty)
      (values))))

(defun type-check-ty (type-env ty)
  (serapeum:match-of ast:ty ty
    ((ast:name-ty name pos)
     (let ((ty (types:get-type type-env name)))
       (unless ty
         (type-check-error pos *line-map* "Undefined type: ~A." (symbol:sym-name name)))
       ty))
    ((ast:record-ty fields)
     (types:record-ty
      (mapcar (lambda (field)
                (trivia:let-match1 (ast:field name type-id pos _) field
                  (let ((ty (types:get-type type-env type-id)))
                    (unless ty
                      (type-check-error
                       pos *line-map*
                       "The type of field ~A is an undefined type: ~A."
                       (symbol:sym-name name)
                       (symbol:sym-name type-id)))
                    (list name ty))))
              fields)))
    ((ast:array-ty base-type-id pos)
     (let ((ty (types:get-type type-env base-type-id)))
       (unless ty
         (type-check-error
          pos
          *line-map*
          "Undefined base type of an array: ~A."
          (symbol:sym-name base-type-id)))
       (types:array-ty ty)))))

(defun type-check-var (type-env type-check-env within-loop var)
  (serapeum:match-of ast:var var
    ((ast:simple-var sym pos)
     (alexandria:if-let (type-check-entry (get-type-check-entry type-check-env sym))
       (serapeum:match-of type-check-entry type-check-entry
         ((type-check-var-entry ty)
          (types:actual-ty ty))
         ((type-check-fun-entry _ _)
          (type-check-error
           pos *line-map*
           "simple-var cannot reference to a function.")))
       (type-check-error
        pos *line-map*
        "Undefined variable: ~A." (symbol:sym-name sym))))
    ((ast:field-var var sym pos)
     (let ((ty (type-check-var type-env type-check-env within-loop var)))
       (serapeum:match-of types:ty ty
         ((types:record-ty fields)
          (alexandria:if-let
              (field
               (find-if (lambda (field)
                          ;; field is of the form (sym ty)
                          (eq (first field) sym))
                        fields))
            (types:actual-ty (second field))
            (type-check-error
             pos *line-map*
             "Unknow field of the record.")))
         (_ (type-check-error
             pos *line-map*
             "You can only access the field of a record.")))))
    ((ast:subscript-var var expr pos)
     (prog1
         (let ((ty (type-check-var type-env type-check-env within-loop var)))
           (serapeum:match-of types:ty ty
             ((types:array-ty base-type)
              (types:actual-ty base-type))
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
    ((ast:var-decl name typ init pos _)
     ;; typ form: (sym pos) or nil.
     (when typ
       (unless (types:get-type type-env (first typ))
         (type-check-error
          (second typ) *line-map*
          "Undefined type: ~A." (symbol:sym-name (first typ)))))
     (let ((init-ty (type-check-expr type-env type-check-env within-loop init)))
       (when typ
         (unless (types:type-compatible init-ty (types:actual-ty (types:get-type type-env (first typ))))
           (type-check-error
            pos *line-map*
            "The type of the init expression of the variable doesn't match the type ~A."
            (symbol:sym-name (first typ)))))
       (list type-env
             (insert-type-check-entry type-check-env name (type-check-var-entry init-ty)))))
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
                 ;; (when (gethash type-decl-name types:*base-type-env*)
                 ;;   (type-check-error
                 ;;    type-decl-pos *line-map*
                 ;;    "Cannot shadow built-in type: ~A."
                 ;;    (symbol:sym-name type-decl-name)))
                 ))
             type-decls))
     (let ((new-type-env
             (reduce (lambda (acc-type-env type-decl)
                       (types:insert-type acc-type-env
                                          (ast:type-decl-name type-decl)
                                          (types:name-ty (ast:type-decl-name type-decl) (types:ty-ref nil))))
                     type-decls
                     :initial-value type-env)))
       (mapc (lambda (type-decl)
               (let ((ty (type-check-ty new-type-env (ast:type-decl-ty type-decl))))
                 (setf
                  (types:ty-ref-value (types:name-ty-ty-ref
                                       (types:get-type new-type-env (ast:type-decl-name type-decl))))
                  ty)
                 (check-type-circular-dependencie ty (ast:type-decl-pos type-decl))))
             type-decls)
       (list new-type-env type-check-env)))
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
     (let ((new-type-check-env
             (reduce (lambda (acc-type-check-env function-decl)
                       (insert-type-check-entry
                        acc-type-check-env
                        (ast:function-decl-name function-decl)
                        (type-check-fun-entry
                         (mapcar (lambda (param-field)
                                   (let ((ty (types:get-type type-env (ast:field-type-id param-field))))
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
                                         (types:get-type type-env (first function-decl-result))
                                         (types:get-unnamed-base-type (symbol:get-sym "unit")))))
                             (unless ty
                               (type-check-error
                                (second function-decl-result) *line-map*
                                "Undefined result type ~A of function ~A."
                                (symbol:sym-name (first function-decl-result))
                                (symbol:sym-name (ast:function-decl-name function-decl))))
                             ty)))))
                     function-decls
                     :initial-value type-check-env)))
       (mapc (lambda (function-decl)
               (let ((type-check-entry (get-type-check-entry new-type-check-env (ast:function-decl-name function-decl))))
                 (trivia:let-match1 (type-check-fun-entry formal-types _) type-check-entry
                   (type-check-expr
                    type-env
                    (loop with acc-type-check-env = new-type-check-env
                          for formal-type in formal-types
                          for param-field in (ast:function-decl-params function-decl)
                          do (setf acc-type-check-env
                                   (insert-type-check-entry
                                    acc-type-check-env
                                    (ast:field-name param-field)
                                    (type-check-var-entry formal-type)))
                          finally (return acc-type-check-env))
                    ;; Cannot break into the outer function.
                    nil
                    (ast:function-decl-body function-decl)))))
             function-decls)
       (list type-env new-type-check-env)))))

(defun type-check-decls (type-env type-check-env within-loop decls)
  (reduce (lambda (acc decl)
            (type-check-decl (first acc) (second acc) within-loop decl))
          decls
          :initial-value (list type-env type-check-env)))

(defun type-check-expr (type-env type-check-env within-loop expr)
  (serapeum:match-of ast:expr expr
    ((ast:var-expr var)
     (type-check-var type-env type-check-env within-loop var))
    ((ast:nil-expr)
     (types:get-unnamed-base-type (symbol:get-sym "nil")))
    ((ast:int-expr _)
     (types:get-type types:*base-type-env* (symbol:get-sym "int")))
    ((ast:string-expr _ _)
     (types:get-type types:*base-type-env* (symbol:get-sym "string")))
    ((ast:call-expr fun args pos)
     (alexandria:if-let (type-check-entry (get-type-check-entry type-check-env fun))
       (serapeum:match-of type-check-entry type-check-entry
         ((type-check-fun-entry formal-types result-type)
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
       (type-check-error
        pos *line-map*
        "Undefined function ~A." (symbol:sym-name fun))))
    ((ast:op-expr left op right pos)
     (let ((left-ty (type-check-expr type-env type-check-env within-loop left))
           (right-ty (type-check-expr type-env type-check-env within-loop right)))
       (trivia:match (list left-ty right-ty)
         ((list (types:int-ty) (types:int-ty))
          (types:get-type types:*base-type-env* (symbol:get-sym "int")))
         ((list (types:string-ty) (types:string-ty))
          (unless (serapeum:match-of ast:op op
                    ((or ast:eq-op ast:neq-op ast:lt-op ast:le-op ast:gt-op ast:ge-op)
                     t)
                    (_ nil))
            (type-check-error
             pos *line-map*
             "Unsupport operation ~A on strings." op))
          (types:get-type types:*base-type-env* (symbol:get-sym "int")))
         (_
          (unless (and
                   (serapeum:match-of ast:op op
                     ((or ast:eq-op ast:neq-op)
                      t)
                     (_ nil))
                   (types:type-compatible left-ty right-ty))
            (type-check-error
             pos *line-map*
             "Unsupport operation ~A." op))
          (types:get-type types:*base-type-env* (symbol:get-sym "int"))))))
    ((ast:record-expr type-id fields pos)
     (alexandria:if-let (ty (types:actual-ty (types:get-type type-env type-id)))
       (trivia:if-match (types:record-ty decl-fields) ty
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
    ((ast:seq-expr exprs)
     (reduce (lambda (acc-type expr-with-pos)
               ;; expr-with-pos form: (expr pos).
               (declare (ignore acc-type))
               (type-check-expr type-env type-check-env within-loop (first expr-with-pos)))
             exprs
             :initial-value (types:get-unnamed-base-type (symbol:get-sym "unit"))))
    ((ast:assign-expr var expr pos)
     (let ((var-ty (type-check-var type-env type-check-env within-loop var))
           (expr-ty (type-check-expr type-env type-check-env within-loop expr)))
       (unless (types:type-compatible var-ty expr-ty)
         (type-check-error
          pos *line-map*
          "Assignment type mismatch."))
       (types:get-unnamed-base-type (symbol:get-sym "unit"))))
    ((ast:if-expr test then else pos)
     (let ((test-ty (type-check-expr type-env type-check-env within-loop test))
           (then-ty (type-check-expr type-env type-check-env within-loop then))
           (else-ty (when else (type-check-expr type-env type-check-env within-loop else))))
       (unless (types:type-compatible test-ty (types:get-type types:*base-type-env* (symbol:get-sym "int")))
         (type-check-error
          pos *line-map*
          "The type of the test expression of an if expression should be int."))
       (if else
           (unless (types:type-compatible then-ty else-ty)
             (type-check-error
              pos *line-map*
              "The types of then and else branchs of an if expression should be the same."))
           (unless (types:type-compatible then-ty (types:get-unnamed-base-type (symbol:get-sym "unit")))
             (type-check-error
              pos *line-map*
              "The then branch of an if-then expression should product no value.")))
       (if else (types:upgrade-from-compatible-types then-ty else-ty)
           (types:get-unnamed-base-type (symbol:get-sym "unit")))))
    ((ast:while-expr test body pos)
     (let ((test-ty (type-check-expr type-env type-check-env within-loop test))
           (body-ty (type-check-expr type-env type-check-env t body)))
       (unless (types:type-compatible test-ty (types:get-type types:*base-type-env* (symbol:get-sym "int")))
         (type-check-error
          pos *line-map*
          "The type of the test expression of a while expression should be int."))
       (unless (types:type-compatible body-ty (types:get-unnamed-base-type (symbol:get-sym "unit")))
         (type-check-error
          pos *line-map*
          "The body expression of a while expression should product no value."))
       body-ty))
    ((ast:for-expr var low high body pos _)
     (let ((low-ty (type-check-expr type-env type-check-env within-loop low))
           (high-ty (type-check-expr type-env type-check-env within-loop high)))
       (let ((int-ty (types:get-type types:*base-type-env* (symbol:get-sym "int"))))
         (unless (types:type-compatible low-ty int-ty)
           (type-check-error
            pos *line-map*
            "The type of the low expression of a for expression should be int."))
         (unless (types:type-compatible high-ty int-ty)
           (type-check-error
            pos *line-map*
            "The type of the high expression of a for expression should be int."))
         (let ((new-type-check-env
                 (insert-type-check-entry type-check-env var (type-check-var-entry int-ty))))
           (let ((body-ty (type-check-expr type-env new-type-check-env t body)))
             (unless (types:type-compatible body-ty (types:get-unnamed-base-type (symbol:get-sym "unit")))
               (type-check-error
                pos *line-map*
                "The body expression of a for expression should product no value."))
             body-ty)))))
    ((ast:break-expr pos)
     (unless within-loop
       (type-check-error
        pos *line-map*
        "break expression not within a loop expression."))
     (types:get-unnamed-base-type (symbol:get-sym "unit")))
    ((ast:array-expr type-id size init pos)
     (let ((ty (types:actual-ty (types:get-type type-env type-id))))
       (trivia:if-match (types:array-ty base-type) ty
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
    ((ast:let-expr decls body _)
     (trivia:let-match1 (list new-type-env new-type-check-env)
         (type-check-decls type-env type-check-env within-loop decls)
       (type-check-expr new-type-env new-type-check-env within-loop body)))))

(defun type-check-program (prog &optional line-map)
  (let ((*line-map* line-map))
    (type-check-expr types:*base-type-env*
                     *base-type-check-env*
                     nil
                     prog)))
