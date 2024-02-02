(cl:defpackage :cl-tiger/translate
  (:use :cl)
  (:local-nicknames
   (:symbol :cl-tiger/symbol)
   (:temp :cl-tiger/temp)
   (:target :cl-tiger/target)
   (:ir :cl-tiger/ir)
   (:frame :cl-tiger/frame)
   (:ast :cl-tiger/ast)
   (:types :cl-tiger/types)
   (:cl-ds :cl-data-structures)
   (:cl-ds.hamt :cl-data-structures.dicts.hamt))
  (:export
   #:find-and-fill-escapes
   #:translate-program))

(cl:in-package :cl-tiger/translate)

;; A map from symbol:sym to (depth ast:escape-ref)
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
  (tagged-expr
   (value ir:expr))
  (tagged-stm
   (value ir:stm))
  (tagged-condi
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

(defun stms->compound-stm (&rest stms)
  (cond ((null stms)
         (ir:expr-stm (ir:int-expr 0)))
        ((null (cdr stms))
         ;; A list of length 1.
         (car stms))
        (t
         (ir:compound-stm
          (car stms) (apply #'stms->compound-stm (rest stms))))))

(defun tagged-ir->expr (tagged-ir)
  (serapeum:match-of tagged-ir tagged-ir
    ((tagged-expr value)
     value)
    ((tagged-stm value)
     (ir:stm-then-expr value (ir:int-expr 0)))
    ((tagged-condi value)
     (let ((result-temp (temp:new-temp "result"))
           (true-label (temp:new-label "true"))
           (false-label (temp:new-label "false")))
       (ir:stm-then-expr
        (stms->compound-stm
         (ir:move-stm (ir:temp-expr result-temp) (ir:int-expr 1))
         (funcall value true-label false-label)
         (ir:label-stm false-label)
         (ir:move-stm (ir:temp-expr result-temp) (ir:int-expr 0))
         (ir:label-stm true-label))
        (ir:temp-expr result-temp))))))

(defun tagged-ir->stm (tagged-ir)
  (serapeum:match-of tagged-ir tagged-ir
    ((tagged-expr value)
     (ir:expr-stm value))
    ((tagged-stm value)
     value)
    ((tagged-condi value)
     (let ((true-label (temp:new-label "true"))
           (false-label (temp:new-label "false")))
       (stms->compound-stm
        (funcall value true-label false-label)
        (ir:label-stm true-label)
        (ir:label-stm false-label))))))

(defun tagged-ir->condi (tagged-ir)
  (serapeum:match-of tagged-ir tagged-ir
    ((tagged-expr value)
     (serapeum:match-of ir:expr value
       ((ir:int-expr 0)
        (lambda (true-label false-label)
          (declare (ignore true-label))
          (ir:jump-stm (ir:label-expr false-label) (list false-label))))
       ((ir:int-expr _)
        (lambda (true-label false-label)
          (declare (ignore false-label))
          (ir:jump-stm (ir:label-expr true-label) (list true-label))))
       (_
        (lambda (true-label false-label)
          (ir:cjump-stm value ir:neq-rel-op (ir:int-expr 0)
                        true-label false-label)))))
    ((tagged-stm _)
     (error "Cannot convert a ir:stm which has no value to a conditional jump generate function."))
    ((tagged-condi value)
     value)))

;; cur-level should be a descendant of target-level.
(defun fp-expr (cur-level target-level target)
  (when (typep target-level 'top-level)
    (error "target-level argument of fp-expr should not be top-level."))
  (labels ((rec (cur-level acc-fp-expr)
             (when (typep cur-level 'top-level)
               (error "cur-level argument of fp-expr should not be top-level."))
             (if (eq cur-level target-level)
                 acc-fp-expr
                 (trivia:let-match1 (inner-level parent _ frame) cur-level
                   (rec parent
                        (frame:access-expr
                         (or (car (frame:frame-formals frame))
                             (error "Missing static link in the frame: ~S" frame))
                         acc-fp-expr
                         target))))))
    (rec cur-level (ir:temp-expr (frame:fp target)))))

(serapeum:defunion ir-entry
  (ir-var-entry
   (ty types:ty)
   (access access))
  (ir-fun-entry
   ;; A list of types:ty
   (formal-types list)
   (result-type types:ty)
   (label temp:label)
   (level level)))

(defun base-ir-env (target)
  (let ((env (cl-ds.hamt:make-functional-hamt-dictionary #'sxhash #'eq)))
    (reduce (lambda (env binding)
              (trivia:let-match1 (list name formal-types result-type) binding
                (let ((level (new-level top-level
                                        (temp:new-named-label name)
                                        (mapcar (lambda (formal-type)
                                                  (declare (ignore formal-type)) nil)
                                                formal-types)
                                        target)))
                  (cl-ds:insert env
                                (symbol:get-sym name)
                                (ir-fun-entry formal-types
                                              result-type
                                              (temp:new-named-label name)
                                              level)))))
            types:*built-in-function-bindings*
            :initial-value env)))

(defun get-ir-entry (ir-env sym)
  (cl-ds:at ir-env sym))

(defun insert-ir-entry (ir-env sym ir-entry)
  (cl-ds:insert ir-env sym ir-entry))

(defun translate-ty (type-env ty)
  (serapeum:match-of ast:ty ty
    ((ast:name-ty name _)
     (types:get-type type-env name))
    ((ast:record-ty fields)
     (types:record-ty
      (mapcar (lambda (field)
                (trivia:let-match1 (ast:field name type-id _ _) field
                  (list name (types:get-type type-env type-id))))
              fields)))
    ((ast:array-ty base-type-id _)
     (types:array-ty (types:get-type type-env base-type-id)))))

(defun translate-var (type-env ir-env level target var)
  (serapeum:match-of ast:var var
    ((ast:simple-var sym _)
     (trivia:let-match1 (ir-var-entry ty access) (get-ir-entry ir-env sym)
       (list
        (types:actual-ty ty)
        (tagged-expr (frame:access-expr (access-frame-access access)
                                        (fp-expr level (access-level access) target)
                                        target)))))
    ((ast:field-var var sym _)
     (trivia:let-match1 (list (types:record-ty fields) var-tagged-ir)
         (translate-var type-env ir-env level target var)
       (trivia:let-match1 (list _ ty index)
           (loop for (field-sym field-ty) in fields
                 for index from 0
                 when (eq field-sym sym)
                   return (list field-sym field-ty index))
         (list (types:actual-ty ty)
               (tagged-expr (ir:mem-expr
                             (ir:bin-op-expr
                              (tagged-ir->expr var-tagged-ir)
                              ir:plus-bin-op
                              (ir:int-expr (* index (frame:word-size target))))))))))
    ((ast:subscript-var var expr _)
     (trivia:let-match (((list (types:array-ty base-type) var-tagged-ir)
                         (translate-var type-env ir-env level target var))
                        ((list _ expr-tagged-ir)
                         (translate-expr type-env ir-env level target expr)))
       (list (types:actual-ty base-type)
             (tagged-expr (ir:mem-expr
                           (ir:bin-op-expr
                            (tagged-ir->expr var-tagged-ir)
                            ir:plus-bin-op
                            (ir:bin-op-expr (tagged-ir->expr expr-tagged-ir)
                                            ir:times-bin-op
                                            (frame:word-size target))))))))))

(defun translate-decl (type-env ir-env level target decl)
  (serapeum:match-of ast:decl decl
    ((ast:var-decl name _ init _ escape-ref)
     (trivia:let-match1 (list init-ty init-tagged-ir)
         (translate-expr type-env ir-env level target init)
       (declare (ignore init-tagged-ir))
       (list type-env
             (insert-ir-entry
              ir-env name
              (ir-var-entry
               init-ty
               (alloc-local
                level (ast:escape-ref-value escape-ref) target)))
             (list
              (let ((var-access (alloc-local level (ast:escape-ref-value escape-ref) target)))
                (tagged-stm
                 (ir:move-stm
                  (frame:access-expr (access-frame-access var-access)
                                     (fp-expr level (access-level var-access) target)
                                     target)
                  (tagged-ir->expr init-tagged-ir))))))))
    ((ast:type-decls type-decls)
     (let ((new-type-env
             (reduce (lambda (acc-type-env type-decl)
                       (types:insert-type acc-type-env
                                          (ast:type-decl-name type-decl)
                                          (types:name-ty (ast:type-decl-name type-decl) (types:ty-ref nil))))
                     type-decls
                     :initial-value type-env)))
       (mapc (lambda (type-decl)
               (setf
                (types:ty-ref-value (types:name-ty-ty-ref
                                     (types:get-type new-type-env (ast:type-decl-name type-decl))))
                (translate-ty new-type-env (ast:type-decl-ty type-decl))))
             type-decls)
       (list new-type-env ir-env nil)))
    ((ast:function-decls function-decls)
     (let ((new-ir-env
             (reduce (lambda (acc-ir-env function-decl)
                       (let ((name (temp:new-label (symbol:sym-name (ast:function-decl-name function-decl)))))
                         (insert-ir-entry
                          acc-ir-env
                          (ast:function-decl-name function-decl)
                          (ir-fun-entry
                           (mapcar (lambda (param-field)
                                     (types:get-type type-env (ast:field-type-id param-field)))
                                   (ast:function-decl-params function-decl))
                           (let ((function-decl-result (ast:function-decl-result function-decl)))
                             ;; function-decl-result form: (sym pos).
                             (if function-decl-result
                                 (types:get-type type-env (first function-decl-result))
                                 (types:get-unnamed-base-type (symbol:get-sym "unit"))))
                           name
                           (new-level level name
                                      (mapcar (lambda (param-field)
                                                (ast:escape-ref-value
                                                 (ast:field-escape-ref param-field)))
                                              (ast:function-decl-params function-decl))
                                      target)))))
                     function-decls
                     :initial-value ir-env)))
       (mapc (lambda (function-decl)
               (let ((ir-entry (get-ir-entry new-ir-env (ast:function-decl-name function-decl))))
                 (trivia:let-match1 (ir-fun-entry formal-types _ _ level) ir-entry
                   (translate-expr
                    type-env
                    (loop with acc-ir-env = new-ir-env
                          for formal-type in formal-types
                          for param-field in (ast:function-decl-params function-decl)
                          for formal-access in (level-formals level)
                          do (setf acc-ir-env
                                   (insert-ir-entry acc-ir-env
                                                    (ast:field-name param-field)
                                                    (ir-var-entry
                                                     formal-type
                                                     formal-access)))
                          finally (return acc-ir-env))
                    level
                    target
                    (ast:function-decl-body function-decl)))))
             function-decls)
       (list type-env new-ir-env nil)))))

(defun translate-decls (type-env ir-env level target decls)
  (reduce (lambda (acc decl)
            (translate-decl (first acc) (second acc) level target decl))
          decls
          :initial-value (list type-env ir-env)))

(defun translate-expr (type-env ir-env level target expr)
  (serapeum:match-of ast:expr expr
    ((ast:var-expr var)
     (translate-var type-env ir-env level target var))
    ((ast:nil-expr)
     (list
      (types:get-unnamed-base-type (symbol:get-sym "nil"))
      (tagged-expr (ir:mem-expr (ir:int-expr 0)))))
    ((ast:int-expr value)
     (list
      (types:get-type types:*base-type-env* (symbol:get-sym "int"))
      (tagged-expr (ir:int-expr value))))
    ((ast:string-expr _ _)
     (list (types:get-type types:*base-type-env* (symbol:get-sym "string")) (tagged-expr (ir:int-expr 0))))
    ((ast:call-expr fun args _)
     (let ((ir-entry (get-ir-entry ir-env fun)))
       (trivia:let-match1 (ir-fun-entry _ result-type _ _) ir-entry
         (mapcar (lambda (arg)
                   (translate-expr type-env ir-env level target arg))
                 args)
         (list (types:actual-ty result-type) (tagged-expr (ir:int-expr 0))))))
    ((ast:op-expr left op right _)
     (trivia:let-match (((list left-ty left-tagged-ir) (translate-expr type-env ir-env level target left))
                        ((list right-ty right-tagged-ir) (translate-expr type-env ir-env level target right)))
       (list
        (types:get-type types:*base-type-env* (symbol:get-sym "int"))
        (cond ((member op (list ast:plus-op ast:minus-op ast:times-op ast:div-op))
               (tagged-expr
                (ir:bin-op-expr (tagged-ir->expr left-tagged-ir)
                                (ecase op
                                  (ast:plus-op ir:plus-bin-op)
                                  (ast:minus-op ir:minus-bin-op)
                                  (ast:times-op ir:times-bin-op)
                                  (ast:div-op ir:div-bin-op))
                                (tagged-ir->expr right-tagged-ir))))
              (t
               ;; TODO test if string or int
               (tagged-expr (ir:int-expr 0)))))))
    ((ast:record-expr type-id fields _)
     (list
      (let ((ty (types:actual-ty (types:get-type type-env type-id))))
        (trivia:let-match1 (types:record-ty _) ty
          (progn
            (loop for field in fields
                  do (translate-expr type-env ir-env level target (second field)))
            ty)))
      (tagged-expr (ir:int-expr 0))))
    ((ast:seq-expr exprs)
     (trivia:let-match1 (list ty tagged-irs)
         (reduce (lambda (acc expr-with-pos)
                   ;; expr-with-pos form: (expr pos).
                   (trivia:let-match1 (list _ acc-tagged-irs) acc
                     (trivia:let-match1 (list ty tagged-ir)
                         (translate-expr type-env ir-env level target (first expr-with-pos))
                       (list ty (cons tagged-ir acc-tagged-irs)))))
                 exprs
                 :initial-value (types:get-unnamed-base-type (symbol:get-sym "unit")))
       (list ty (reverse tagged-irs))))
    ((ast:assign-expr var expr _)
     (trivia:let-match (((list _ _) (translate-var type-env ir-env level target var))
                        ((list _ _) (translate-expr type-env ir-env level target expr)))
       (list (types:get-unnamed-base-type (symbol:get-sym "unit")) (tagged-expr (ir:int-expr 0)))))
    ((ast:if-expr test then else _)
     (trivia:let-match (((list _ _) (translate-expr type-env ir-env level target test))
                        ((list then-ty _) (translate-expr type-env ir-env level target then))
                        ((list else-ty _) (if else
                                              (translate-expr type-env ir-env level target else)
                                              (list nil (tagged-expr (ir:int-expr 0))))))
       (list
        (if else
            (types:upgrade-from-compatible-types then-ty else-ty)
            (types:get-unnamed-base-type (symbol:get-sym "unit")))
        (tagged-expr (ir:int-expr 0)))))
    ((ast:while-expr test body _)
     (trivia:let-match (((list _ _) (translate-expr type-env ir-env level target test))
                        ((list body-ty _) (translate-expr type-env ir-env level target body)))
       (list body-ty (tagged-expr (ir:int-expr 0)))))
    ((ast:for-expr var low high body _ escape-ref)
     (trivia:let-match (((list _ _) (translate-expr type-env ir-env level target low))
                        ((list _ _) (translate-expr type-env ir-env level target high)))
       (let ((int-ty (types:get-type types:*base-type-env* (symbol:get-sym "int"))))
         (let ((new-ir-env
                 (insert-ir-entry
                  ir-env var
                  (ir-var-entry
                   int-ty
                   (alloc-local level (ast:escape-ref-value escape-ref) target)))))
           (translate-expr type-env new-ir-env level target body)))))
    ((ast:break-expr _)
     (list (types:get-unnamed-base-type (symbol:get-sym "unit")) (tagged-expr (ir:int-expr 0))))
    ((ast:array-expr type-id size init _)
     (list
      (let ((ty (types:actual-ty (types:get-type type-env type-id))))
        (trivia:let-match1 (types:array-ty _) ty
          (trivia:let-match (((list _ _) (translate-expr type-env ir-env level target size))
                             ((list _ _) (translate-expr type-env ir-env level target init)))))
        ty)
      (tagged-expr (ir:int-expr 0))))
    ((ast:let-expr decls body _)
     (trivia:let-match1 (list new-type-env new-ir-env _)
         (translate-decls type-env ir-env level target decls)
       (translate-expr new-type-env new-ir-env level target body)))))

(defun translate-program (prog target)
  (translate-expr types:*base-type-env*
                  (base-ir-env target)
                  (new-level top-level (temp:new-label "_tiger_program") nil target)
                  target
                  prog))
