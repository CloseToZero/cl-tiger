(cl:defpackage :cl-tiger/translate
  (:use :cl)
  (:local-nicknames
   (:symbol :cl-tiger/symbol)
   (:temp :cl-tiger/temp)
   (:target :cl-tiger/target)
   (:ir :cl-tiger/ir)
   (:frame :cl-tiger/frame)
   (:ast :cl-tiger/ast)
   (:type :cl-tiger/type))
  (:export
   #:translate-program))

(cl:in-package :cl-tiger/translate)

;; A map from symbol:sym to (depth ast:escape-ref)
(defvar *base-escape-ref-env* (fset:empty-map))

(defun insert-escape-ref (escape-ref-env sym depth escape-ref)
  (fset:with escape-ref-env sym (list depth escape-ref)))

(defun get-escape-ref (escape-ref-env sym)
  (fset:@ escape-ref-env sym))

;; prog is the expression of the whole program with the type
;; of ast:expr.
(defun find-and-fill-escapes (prog)
  (find-and-fill-escapes-expr *base-escape-ref-env* 0 prog)
  (values))

(defun find-and-fill-escapes-expr (escape-ref-env depth expr)
  (serapeum:match-of ast:expr expr
    ((ast:expr-var var)
     (find-and-fill-escapes-var escape-ref-env depth var))
    ((ast:expr-nil) nil)
    ((ast:expr-int _) nil)
    ((ast:expr-string _ _) nil)
    ((ast:expr-call _ args _)
     (mapc (lambda (arg)
             (find-and-fill-escapes-expr escape-ref-env depth arg))
           args))
    ((ast:expr-op left _ right _)
     (find-and-fill-escapes-expr escape-ref-env depth left)
     (find-and-fill-escapes-expr escape-ref-env depth right))
    ((ast:expr-record _ fields _)
     (mapc (lambda (field)
             ;; form: (sym expr pos).
             (find-and-fill-escapes-expr escape-ref-env depth (second field)))
           fields))
    ((ast:expr-seq exprs)
     (mapc (lambda (expr-with-pos)
             ;; expr-with-pos form: (expr pos).
             (find-and-fill-escapes-expr escape-ref-env depth (first expr-with-pos)))
           exprs))
    ((ast:expr-assign _ expr _)
     (find-and-fill-escapes-expr escape-ref-env depth expr))
    ((ast:expr-if test then else _)
     (find-and-fill-escapes-expr escape-ref-env depth test)
     (find-and-fill-escapes-expr escape-ref-env depth then)
     (when else (find-and-fill-escapes-expr escape-ref-env depth else)))
    ((ast:expr-while test body _)
     (find-and-fill-escapes-expr escape-ref-env depth test)
     (find-and-fill-escapes-expr escape-ref-env depth body))
    ((ast:expr-for var low high body _ escape-ref)
     (find-and-fill-escapes-expr escape-ref-env depth low)
     (find-and-fill-escapes-expr escape-ref-env depth high)
     (find-and-fill-escapes-expr (insert-escape-ref
                                  escape-ref-env var depth
                                  (progn (setf (ast:escape-ref-value escape-ref) nil)
                                         escape-ref))
                                 depth body))
    ((ast:expr-break _)
     nil)
    ((ast:expr-array _ size init _)
     (find-and-fill-escapes-expr escape-ref-env depth size)
     (find-and-fill-escapes-expr escape-ref-env depth init))
    ((ast:expr-let decls body _)
     (let ((new-escape-ref-env (find-and-fill-escapes-decls escape-ref-env depth decls)))
       (find-and-fill-escapes-expr new-escape-ref-env depth body)))))

(defun find-and-fill-escapes-var (escape-ref-env depth var)
  (serapeum:match-of ast:var var
    ((ast:var-simple sym _)
     (trivia:let-match1 (list escape-ref-depth escape-ref) (get-escape-ref escape-ref-env sym)
       (when (> depth escape-ref-depth)
         (setf (ast:escape-ref-value escape-ref) t)))
     nil)
    ((ast:var-field var _ _)
     (find-and-fill-escapes-var escape-ref-env depth var))
    ((ast:var-subscript var expr _)
     (find-and-fill-escapes-expr escape-ref-env depth expr)
     (find-and-fill-escapes-var escape-ref-env depth var))))

(defun find-and-fill-escapes-decl (escape-ref-env depth decl)
  (serapeum:match-of ast:decl decl
    ((ast:decl-var name _ init _ escape-ref)
     (find-and-fill-escapes-expr escape-ref-env depth init)
     (insert-escape-ref escape-ref-env name depth
                        (progn (setf (ast:escape-ref-value escape-ref) nil)
                               escape-ref)))
    ((ast:decl-types _) escape-ref-env)
    ((ast:decl-functions decl-functions)
     (mapc (lambda (decl-function)
             (find-and-fill-escapes-expr
              (loop with acc-escape-ref-env = escape-ref-env
                    for param-field in (ast:decl-function-params decl-function)
                    do (setf acc-escape-ref-env
                             (insert-escape-ref acc-escape-ref-env
                                                (ast:field-name param-field)
                                                (+ depth 1)
                                                (let ((escape-ref (ast:field-escape-ref param-field)))
                                                  (setf (ast:escape-ref-value escape-ref) nil)
                                                  escape-ref)))
                    finally (return acc-escape-ref-env))
              (+ depth 1) (ast:decl-function-body decl-function)))
           decl-functions)
     escape-ref-env)))

(defun find-and-fill-escapes-decls (escape-ref-env depth decls)
  (reduce (lambda (acc-escape-ref-env decl)
            (find-and-fill-escapes-decl acc-escape-ref-env depth decl))
          decls
          :initial-value escape-ref-env))

(serapeum:defunion level
  level-top
  (level-inner
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

(defun new-level (parent name formals is-pointer-table target)
  (let ((frame (frame:new-frame name (cons t formals) is-pointer-table target)))
    (level-inner parent name frame)))

(defun level-formals (level)
  (rest (mapcar (lambda (frame-access)
                  (access level frame-access))
                (frame:frame-formals (level-inner-frame level)))))

(defun alloc-local (level escape target)
  (serapeum:match-of level level
    (level-top (error "Cannot alloc a local variable in the top level."))
    ((level-inner _ _ frame)
     (access level (frame:alloc-local frame escape target)))))

(defun tagged-ir->expr (tagged-ir)
  (serapeum:match-of tagged-ir tagged-ir
    ((tagged-expr value)
     value)
    ((tagged-stm value)
     (ir:expr-stm-then-expr value (ir:expr-int 0)))
    ((tagged-condi value)
     (let ((result-temp (temp:new-temp "result"))
           (true-label (temp:new-label "true"))
           (false-label (temp:new-label "false")))
       (ir:expr-stm-then-expr
        (ir:stms->stm-compound
         (ir:stm-move (ir:expr-temp result-temp) (ir:expr-int 1))
         (funcall value true-label false-label)
         (ir:stm-label false-label)
         (ir:stm-move (ir:expr-temp result-temp) (ir:expr-int 0))
         (ir:stm-label true-label))
        (ir:expr-temp result-temp))))))

(defun tagged-ir->stm (tagged-ir)
  (serapeum:match-of tagged-ir tagged-ir
    ((tagged-expr value)
     (ir:stm-expr value))
    ((tagged-stm value)
     value)
    ((tagged-condi value)
     (let ((true-label (temp:new-label "true"))
           (false-label (temp:new-label "false")))
       (ir:stms->stm-compound
        (funcall value true-label false-label)
        (ir:stm-label true-label)
        (ir:stm-label false-label))))))

(defun tagged-ir->condi (tagged-ir)
  (serapeum:match-of tagged-ir tagged-ir
    ((tagged-expr value)
     (serapeum:match-of ir:expr value
       ((ir:expr-int 0)
        (lambda (true-label false-label)
          (declare (ignore true-label))
          (ir:stm-jump (ir:expr-label false-label) (list false-label))))
       ((ir:expr-int _)
        (lambda (true-label false-label)
          (declare (ignore false-label))
          (ir:stm-jump (ir:expr-label true-label) (list true-label))))
       (_
        (lambda (true-label false-label)
          (ir:stm-cjump value ir:rel-op-neq (ir:expr-int 0)
                        true-label false-label)))))
    ((tagged-stm _)
     (error "Cannot convert a ir:stm which has no value to a conditional jump generate function."))
    ((tagged-condi value)
     value)))

;; cur-level should be a descendant of target-level.
(defun fp-expr (cur-level target-level target)
  (when (typep target-level 'level-top)
    (error "target-level argument of fp-expr should not be level-top."))
  (labels ((rec (cur-level acc-fp-expr)
             (when (typep cur-level 'level-top)
               (error "cur-level argument of fp-expr should not be level-top."))
             (if (eq cur-level target-level)
                 acc-fp-expr
                 (trivia:let-match1 (level-inner parent _ frame) cur-level
                   (rec parent
                        (frame:access-expr
                         (or (car (frame:frame-formals frame))
                             (error "Missing static link in the frame: ~S" frame))
                         acc-fp-expr
                         target))))))
    (rec cur-level (ir:expr-temp (frame:fp target)))))

(serapeum:defunion ir-entry
  (ir-entry-var
   (ty type:ty)
   (access access))
  (ir-entry-fun
   ;; A list of type:ty
   (formal-tys list)
   (result-ty type:ty)
   (label temp:label)
   (level level)))

(defun mark-access-as-pointer (level access)
  (setf (gethash (access-frame-access access)
                 (frame:frame-is-pointer-table (level-inner-frame level)))
        t))

(defun is-ty-pointer (ty)
  (serapeum:match-of type:ty (type:actual-ty ty)
    ((or (type:ty-record _)
         (type:ty-array _))
     t)
    (_ nil)))

(defun base-ir-env (target)
  (let ((env (fset:empty-map)))
    (reduce (lambda (env binding)
              (trivia:let-match1 (list name formal-tys result-ty) binding
                (let ((level (new-level level-top
                                        (temp:new-named-label name)
                                        (mapcar (lambda (formal-ty)
                                                  (declare (ignore formal-ty)) nil)
                                                formal-tys)
                                        (frame:new-is-pointer-table)
                                        target)))
                  (fset:with env
                             (symbol:get-sym name)
                             (ir-entry-fun formal-tys
                                           result-ty
                                           (temp:new-named-label name)
                                           level)))))
            type:*built-in-function-bindings*
            :initial-value env)))

(defun get-ir-entry (ir-env sym)
  (fset:@ ir-env sym))

(defun insert-ir-entry (ir-env sym ir-entry)
  (fset:with ir-env sym ir-entry))

(defun translate-ty (ty-env ty)
  (serapeum:match-of ast:ty ty
    ((ast:ty-name name _)
     (type:get-ty ty-env name))
    ((ast:ty-record fields)
     (type:ty-record
      (mapcar (lambda (field)
                (trivia:let-match1 (ast:field name type-id _ _) field
                  (list name (type:get-ty ty-env type-id))))
              fields)))
    ((ast:ty-array base-type-id _)
     (type:ty-array (type:get-ty ty-env base-type-id)))))

(defun translate-var (ty-env ir-env level target break-target var)
  (serapeum:match-of ast:var var
    ((ast:var-simple sym _)
     (trivia:let-match1 (ir-entry-var ty access) (get-ir-entry ir-env sym)
       (list
        (type:actual-ty ty)
        (tagged-expr (frame:access-expr
                      (access-frame-access access)
                      (fp-expr level (access-level access) target)
                      target)))))
    ((ast:var-field var sym _)
     (trivia:let-match1 (list (type:ty-record fields) var-tagged-ir)
         (translate-var ty-env ir-env level target break-target var)
       (trivia:let-match1 (list _ ty index)
           (loop for (field-sym field-ty) in fields
                 for index from 0
                 when (eq field-sym sym)
                   return (list field-sym field-ty index))
         (list (type:actual-ty ty)
               (tagged-expr
                (ir:expr-stm-then-expr
                 (ir:stm-expr
                  (frame:external-call
                   "CheckNilRecord"
                   (list (tagged-ir->expr var-tagged-ir))
                   target))
                 (ir:expr-mem
                  (ir:expr-bin-op
                   (tagged-ir->expr var-tagged-ir)
                   ir:bin-op-plus
                   (ir:expr-int
                    (* (+ index 1) ; + 1 to skip the record descriptor pointer
                       (frame:word-size target)))))))))))
    ((ast:var-subscript var expr _)
     (trivia:let-match (((list (type:ty-array base-ty) var-tagged-ir)
                         (translate-var ty-env ir-env level target break-target var))
                        ((list _ expr-tagged-ir)
                         (translate-expr ty-env ir-env level target break-target expr)))
       (list (type:actual-ty base-ty)
             (tagged-expr
              (ir:expr-stm-then-expr
               (ir:stm-expr
                (frame:external-call
                 "CheckArraySubscript"
                 (list
                  (tagged-ir->expr var-tagged-ir)
                  (tagged-ir->expr expr-tagged-ir))
                 target))
               (ir:expr-mem
                (ir:expr-bin-op
                 (tagged-ir->expr var-tagged-ir)
                 ir:bin-op-plus
                 (ir:expr-bin-op
                  ;; Skip the array descriptor pointer.
                  (ir:expr-bin-op
                   (tagged-ir->expr expr-tagged-ir)
                   ir:bin-op-plus
                   (ir:expr-int 1))
                  ir:bin-op-times
                  (ir:expr-int (frame:word-size target))))))))))))

(defun translate-decl (ty-env ir-env level target break-target decl)
  (serapeum:match-of ast:decl decl
    ((ast:decl-var name typ init _ escape-ref)
     (trivia:let-match1 (list init-ty init-tagged-ir)
         (translate-expr ty-env ir-env level target break-target init)
       (let ((var-access (alloc-local level (ast:escape-ref-value escape-ref) target)))
         (when (is-ty-pointer init-ty)
           (mark-access-as-pointer level var-access))
         (list ty-env
               (insert-ir-entry
                ir-env name
                (ir-entry-var
                 (if typ
                     (type:actual-ty (type:get-ty ty-env (first typ)))
                     init-ty)
                 var-access))
               (list
                (tagged-stm
                 (ir:stm-move
                  (frame:access-expr
                   (access-frame-access var-access)
                   (fp-expr level (access-level var-access) target)
                   target)
                  (tagged-ir->expr init-tagged-ir))))))))
    ((ast:decl-types decl-types)
     (let ((new-ty-env
             (reduce (lambda (acc-ty-env decl-type)
                       (type:insert-ty acc-ty-env
                                          (ast:decl-type-name decl-type)
                                          (type:ty-name (ast:decl-type-name decl-type) (type:ty-ref nil))))
                     decl-types
                     :initial-value ty-env)))
       (mapc (lambda (decl-type)
               (setf
                (type:ty-ref-value (type:ty-name-ty-ref
                                     (type:get-ty new-ty-env (ast:decl-type-name decl-type))))
                (translate-ty new-ty-env (ast:decl-type-ty decl-type))))
             decl-types)
       (list new-ty-env ir-env nil)))
    ((ast:decl-functions decl-functions)
     (let* ((new-ir-env
              (reduce (lambda (acc-ir-env decl-function)
                        (let ((name (temp:new-label (symbol:sym-name (ast:decl-function-name decl-function)))))
                          (insert-ir-entry
                           acc-ir-env
                           (ast:decl-function-name decl-function)
                           (ir-entry-fun
                            (mapcar (lambda (param-field)
                                      (type:get-ty ty-env (ast:field-type-id param-field)))
                                    (ast:decl-function-params decl-function))
                            (let ((decl-function-result (ast:decl-function-result decl-function)))
                              ;; decl-function-result form: (sym pos).
                              (if decl-function-result
                                  (type:get-ty ty-env (first decl-function-result))
                                  (type:get-unnamed-base-ty (symbol:get-sym "unit"))))
                            name
                            (new-level level name
                                       (mapcar (lambda (param-field)
                                                 (ast:escape-ref-value
                                                  (ast:field-escape-ref param-field)))
                                               (ast:decl-function-params decl-function))
                                       (frame:new-is-pointer-table)
                                       target)))))
                      decl-functions
                      :initial-value ir-env)))
       (mapc (lambda (decl-function)
               (let ((ir-entry (get-ir-entry new-ir-env (ast:decl-function-name decl-function))))
                 (trivia:let-match1 (ir-entry-fun formal-tys _ _ level) ir-entry
                   (trivia:let-match1 (list _ body-tagged-ir)
                       (translate-expr
                        ty-env
                        (loop with acc-ir-env = new-ir-env
                              for formal-ty in formal-tys
                              for param-field in (ast:decl-function-params decl-function)
                              for formal-access in (level-formals level)
                              do (setf acc-ir-env
                                       (insert-ir-entry
                                        acc-ir-env
                                        (ast:field-name param-field)
                                        (ir-entry-var
                                         formal-ty
                                         formal-access)))
                                 (when (is-ty-pointer formal-ty)
                                   (mark-access-as-pointer level formal-access))
                              finally (return acc-ir-env))
                        level
                        target
                        break-target
                        (ast:decl-function-body decl-function))
                     (let ((frame (level-inner-frame level)))
                       (alloc-fun
                        (frame:wrap-ir-entry-exit
                         frame
                         (ir:stm-move
                          (ir:expr-temp (frame:rv target))
                          (tagged-ir->expr body-tagged-ir))
                         target)
                        frame))))))
             decl-functions)
       (list ty-env new-ir-env nil)))))

(defun translate-decls (ty-env ir-env level target break-target decls)
  (reduce (lambda (acc decl)
            (trivia:let-match1 (list acc-ty-env acc-ir-env acc-tagged-irs) acc
              (trivia:let-match1 (list ty-env ir-env tagged-irs)
                  (translate-decl acc-ty-env acc-ir-env level target break-target decl)
                (list ty-env ir-env (nconc acc-tagged-irs tagged-irs)))))
          decls
          :initial-value (list ty-env ir-env nil)))

(defun translate-expr (ty-env ir-env level target break-target expr)
  (serapeum:match-of ast:expr expr
    ((ast:expr-var var)
     (translate-var ty-env ir-env level target break-target var))
    ((ast:expr-nil)
     (list
      (type:get-unnamed-base-ty (symbol:get-sym "nil"))
      ;; Should not product (tagged-expr (ir:expr-mem (ir:expr-int 0))),
      ;; otherwise, instruction selection will product an asm
      ;; instruction to load data from the zero address, cause memeory
      ;; fault.
      (tagged-expr (ir:expr-int 0))))
    ((ast:expr-int value)
     (list
      (type:get-ty type:*base-ty-env* (symbol:get-sym "int"))
      (tagged-expr (ir:expr-int value))))
    ((ast:expr-string str _)
     (list (type:get-ty type:*base-ty-env* (symbol:get-sym "string"))
           (let ((string-label (temp:new-label "string")))
             (alloc-string string-label str)
             (tagged-expr (ir:expr-label string-label)))))
    ((ast:expr-call fun args _)
     (let ((ir-entry (get-ir-entry ir-env fun)))
       (trivia:let-match1 (ir-entry-fun _ result-ty fun-label fun-level) ir-entry
         (list (type:actual-ty result-ty)
               (trivia:let-match1 (level-inner parent-level _) fun-level
                 (let ((arg-ir-exprs
                         (mapcar (lambda (arg)
                                   (trivia:let-match1 (list _ arg-tagged-ir)
                                       (translate-expr ty-env ir-env level target break-target arg)
                                     (tagged-ir->expr arg-tagged-ir)))
                                 args)))
                   (serapeum:match-of level parent-level
                     ((level-inner _ _ _)
                      (tagged-expr
                       (ir:expr-call
                        (ir:expr-label fun-label)
                        (list*
                         (fp-expr level parent-level target)
                         arg-ir-exprs))))
                     (_ (tagged-expr
                         (frame:external-call
                          (temp:label-name fun-label)
                          arg-ir-exprs
                          target))))))))))
    ((ast:expr-op left op right _)
     (trivia:let-match (((list left-ty left-tagged-ir)
                         (translate-expr ty-env ir-env level target break-target left))
                        ((list right-ty right-tagged-ir)
                         (translate-expr ty-env ir-env level target break-target right)))
       (list
        (type:get-ty type:*base-ty-env* (symbol:get-sym "int"))
        (cond ((member op (list ast:op-plus ast:op-minus ast:op-times ast:op-div))
               (tagged-expr
                (ir:expr-bin-op (tagged-ir->expr left-tagged-ir)
                                (trivia:ematch op
                                  ((ast:op-plus) ir:bin-op-plus)
                                  ((ast:op-minus) ir:bin-op-minus)
                                  ((ast:op-times) ir:bin-op-times)
                                  ((ast:op-div) ir:bin-op-div))
                                (tagged-ir->expr right-tagged-ir))))
              (t
               (trivia:match (list left-ty right-ty)
                 ((list (type:ty-string) (type:ty-string))
                  (tagged-condi
                   (lambda (true-target false-target)
                     (ir:stm-cjump
                      (frame:external-call
                       "StringCompare"
                       (list (tagged-ir->expr left-tagged-ir)
                             (tagged-ir->expr right-tagged-ir))
                       target)
                      (trivia:ematch op
                        ((ast:op-eq) ir:rel-op-eq)
                        ((ast:op-neq) ir:rel-op-neq)
                        ((ast:op-lt) ir:rel-op-lt)
                        ((ast:op-le) ir:rel-op-le)
                        ((ast:op-gt) ir:rel-op-gt)
                        ((ast:op-ge) ir:rel-op-ge))
                      (ir:expr-int 0)
                      true-target false-target))))
                 (_
                  (tagged-condi
                   (lambda (true-target false-target)
                     (ir:stm-cjump
                      (tagged-ir->expr left-tagged-ir)
                      (trivia:ematch op
                        ((ast:op-eq) ir:rel-op-eq)
                        ((ast:op-neq) ir:rel-op-neq)
                        ((ast:op-lt) ir:rel-op-lt)
                        ((ast:op-le) ir:rel-op-le)
                        ((ast:op-gt) ir:rel-op-gt)
                        ((ast:op-ge) ir:rel-op-ge))
                      (tagged-ir->expr right-tagged-ir)
                      true-target false-target))))))))))
    ((ast:expr-record type-id fields _)
     (let ((ty (type:actual-ty (type:get-ty ty-env type-id))))
       (trivia:let-match1 (type:ty-record _) ty
         (list ty
               (let ((m (temp:new-temp "record")))
                 (tagged-expr
                  (ir:expr-stm-then-expr
                   (apply #'ir:stms->stm-compound
                          (ir:stm-move
                           (ir:expr-temp m)
                           (frame:external-call
                            "AllocRecord"
                            (list (record-descriptor-expr ty))
                            target))
                          (loop for field in fields
                                for index from 0
                                collect
                                (trivia:let-match1 (list _ field-tagged-ir)
                                    (translate-expr ty-env ir-env level target break-target (second field))
                                  (ir:stm-move
                                   (ir:expr-mem
                                    (ir:expr-bin-op (ir:expr-temp m)
                                                    ir:bin-op-plus
                                                    (ir:expr-int
                                                     (* (+ index 1) ; + 1 to skip the record descriptor pointer
                                                        (frame:word-size target)))))
                                   (tagged-ir->expr field-tagged-ir)))))
                   (ir:expr-temp m))))))))
    ((ast:expr-seq exprs)
     (trivia:let-match1 (list ty tagged-irs)
         (reduce (lambda (acc expr-with-pos)
                   ;; expr-with-pos form: (expr pos).
                   (trivia:let-match1 (list _ acc-tagged-irs) acc
                     (trivia:let-match1 (list ty tagged-ir)
                         (translate-expr ty-env ir-env level target break-target (first expr-with-pos))
                       (list ty (cons tagged-ir acc-tagged-irs)))))
                 exprs
                 :initial-value (list (type:get-unnamed-base-ty (symbol:get-sym "unit")) nil))
       (list ty
             (labels ((rec (tagged-irs)
                        (trivia:ematch tagged-irs
                          (nil
                           (tagged-stm (ir:stm-expr (ir:expr-int 0))))
                          ((list tagged-ir)
                           tagged-ir)
                          ((list* tagged-ir rest-tagged-irs)
                           (tagged-expr
                            (ir:expr-stm-then-expr
                             (tagged-ir->stm tagged-ir)
                             (tagged-ir->expr (rec rest-tagged-irs))))))))
               (rec (nreverse tagged-irs))))))
    ((ast:expr-assign var expr _)
     (trivia:let-match (((list _ var-tagged-ir)
                         (translate-var ty-env ir-env level target break-target var))
                        ((list _ expr-tagged-ir)
                         (translate-expr ty-env ir-env level target break-target expr)))
       (list (type:get-unnamed-base-ty (symbol:get-sym "unit"))
             (tagged-stm
              (ir:stm-move
               (tagged-ir->expr var-tagged-ir)
               (tagged-ir->expr expr-tagged-ir))))))
    ((ast:expr-if test then else _)
     (trivia:let-match (((list _ test-tagged-ir)
                         (translate-expr ty-env ir-env level target break-target test))
                        ((list then-ty then-tagged-ir)
                         (translate-expr ty-env ir-env level target break-target then))
                        ((list else-ty else-tagged-ir)
                         (if else
                             (translate-expr ty-env ir-env level target break-target else)
                             (list nil (tagged-expr (ir:expr-int 0))))))
       (list
        (if else
            (type:upgrade-from-compatible-tys then-ty else-ty)
            (type:get-unnamed-base-ty (symbol:get-sym "unit")))
        (if else
            (let ((true-target (temp:new-label "true_target"))
                  (false-target (temp:new-label "false_target"))
                  (join-target (temp:new-label "join_target"))
                  (result-temp (temp:new-temp "result")))
              (tagged-expr
               (ir:expr-stm-then-expr
                (ir:stms->stm-compound
                 (ir:stm-cjump (tagged-ir->expr test-tagged-ir)
                               ir:rel-op-neq
                               (ir:expr-int 0)
                               true-target
                               false-target)
                 (ir:stm-label true-target)
                 (ir:stm-move (ir:expr-temp result-temp) (tagged-ir->expr then-tagged-ir))
                 (ir:stm-jump (ir:expr-label join-target) (list join-target))
                 (ir:stm-label false-target)
                 (ir:stm-move (ir:expr-temp result-temp) (tagged-ir->expr else-tagged-ir))
                 (ir:stm-label join-target))
                (ir:expr-temp result-temp))))
            (tagged-stm
             (let ((true-target (temp:new-label "true_target"))
                   (false-target (temp:new-label "false_target")))
               (ir:stms->stm-compound
                (ir:stm-cjump (tagged-ir->expr test-tagged-ir)
                              ir:rel-op-neq
                              (ir:expr-int 0)
                              true-target
                              false-target)
                (ir:stm-label true-target)
                (tagged-ir->stm then-tagged-ir)
                (ir:stm-label false-target))))))))
    ((ast:expr-while test body _)
     (let ((break-target (temp:new-label "break_target")))
       (trivia:let-match (((list _ test-tagged-ir)
                           (translate-expr ty-env ir-env level target break-target test))
                          ((list body-ty body-tagged-ir)
                           (translate-expr ty-env ir-env level target break-target body)))
         (let ((test-target (temp:new-label "test"))
               (body-target (temp:new-label "body_target")))
           (list body-ty
                 (tagged-stm
                  (ir:stms->stm-compound
                   (ir:stm-label test-target)
                   (ir:stm-cjump (tagged-ir->expr test-tagged-ir)
                                 ir:rel-op-neq
                                 (ir:expr-int 0)
                                 body-target
                                 break-target)
                   (ir:stm-label body-target)
                   (tagged-ir->stm body-tagged-ir)
                   (ir:stm-jump (ir:expr-label test-target) (list test-target))
                   (ir:stm-label break-target))))))))
    ((ast:expr-for var low high body _ escape-ref)
     (let ((break-target (temp:new-label "break_target")))
       (trivia:let-match (((list _ low-tagged-ir)
                           (translate-expr ty-env ir-env level target break-target low))
                          ((list _ high-tagged-ir)
                           (translate-expr ty-env ir-env level target break-target high)))
         (let ((ty-int (type:get-ty type:*base-ty-env* (symbol:get-sym "int"))))
           (let* ((var-access
                    (alloc-local level (ast:escape-ref-value escape-ref) target))
                  (new-ir-env
                    (insert-ir-entry
                     ir-env
                     var
                     (ir-entry-var
                      ty-int
                      var-access))))
             (trivia:let-match1 (list body-ty body-tagged-ir)
                 (translate-expr ty-env new-ir-env level target break-target body)
               (let ((high-temp (temp:new-temp "high"))
                     (pre-inc-test-temp (temp:new-temp "pre_inc_test"))
                     (body-target (temp:new-label "body_target")))
                 (flet ((var-access-expr ()
                          (frame:access-expr
                           (access-frame-access var-access)
                           (fp-expr level (access-level var-access) target)
                           target)))
                   (list body-ty
                         (tagged-stm
                          (ir:stms->stm-compound
                           (ir:stm-move
                            (var-access-expr)
                            (tagged-ir->expr low-tagged-ir))
                           (ir:stm-move
                            (ir:expr-temp high-temp)
                            (tagged-ir->expr high-tagged-ir))
                           (ir:stm-cjump
                            (var-access-expr)
                            ir:rel-op-le
                            (ir:expr-temp high-temp)
                            body-target
                            break-target)
                           (ir:stm-label body-target)
                           (tagged-ir->stm body-tagged-ir)
                           (ir:stm-move
                            (ir:expr-temp pre-inc-test-temp)
                            (tagged-ir->expr
                             (tagged-condi
                              (lambda (true-target false-target)
                                (ir:stm-cjump
                                 (var-access-expr)
                                 ir:rel-op-lt
                                 (ir:expr-temp high-temp)
                                 true-target
                                 false-target)))))
                           (ir:stm-move
                            (var-access-expr)
                            (ir:expr-bin-op
                             (var-access-expr)
                             ir:bin-op-plus
                             (ir:expr-int 1)))
                           (ir:stm-cjump
                            (ir:expr-temp pre-inc-test-temp)
                            ir:rel-op-neq
                            (ir:expr-int 0)
                            body-target
                            break-target)
                           (ir:stm-label break-target))))))))))))
    ((ast:expr-break _)
     (list (type:get-unnamed-base-ty (symbol:get-sym "unit"))
           (tagged-stm
            (ir:stm-jump (ir:expr-label break-target) (list break-target)))))
    ((ast:expr-array type-id size init _)
     (let ((ty (type:actual-ty (type:get-ty ty-env type-id))))
       (trivia:let-match1 (type:ty-array base-ty) ty
         (trivia:let-match (((list _ size-tagged-ir)
                             (translate-expr ty-env ir-env level target break-target size))
                            ((list _ init-tagged-ir)
                             (translate-expr ty-env ir-env level target break-target init)))
           (list ty
                 (tagged-expr
                  (frame:external-call
                   "AllocArray"
                   (list
                    (tagged-ir->expr size-tagged-ir)
                    (tagged-ir->expr init-tagged-ir)
                    (ir:expr-int (if (is-ty-pointer base-ty) 1 0)))
                   target)))))))
    ((ast:expr-let decls body _)
     (trivia:let-match1 (list new-ty-env new-ir-env tagged-irs)
         (translate-decls ty-env ir-env level target break-target decls)
       (trivia:let-match1 (list body-ty body-tagged-ir)
           (translate-expr new-ty-env new-ir-env level target break-target body)
         (list body-ty
               (tagged-expr
                (ir:expr-stm-then-expr
                 (apply #'ir:stms->stm-compound
                        (mapcar #'tagged-ir->stm tagged-irs))
                 (tagged-ir->expr body-tagged-ir)))))))))

;; A list of frame:frag.
(defvar *frags* nil)

(defun alloc-string (label str)
  (push (frame:frag-str label str) *frags*))

(defun alloc-fun (body frame)
  (push (frame:frag-fun body frame) *frags*))

;; A map from a record descriptor string to a label which has the record descriptor string as definition,
;; for what is record descriptor string, see tiger_AllocRecord in runtime.c,
;; the initial value of the special variable is nil,
;; it should be bound to a hash-table before translation.
(defvar *record-descriptor-table* nil)

(defun record-descriptor-expr (record-ty)
  ;; fields: A list of (sym ty)
  (trivia:let-match1 (type:ty-record fields) record-ty
    (let ((descriptor-str (make-string (length fields))))
      (loop for (nil field-ty) in fields
            for index from 0
            do (setf (elt descriptor-str index)
                     (trivia:match (type:actual-ty field-ty)
                       ((or (type:ty-array _) (type:ty-record _)) #\p)
                       (_  #\n))))
      (or (gethash descriptor-str  *record-descriptor-table*)
          (setf (gethash descriptor-str *record-descriptor-table*)
                (let ((label (temp:new-label "record_descriptor_str")))
                  (alloc-string label descriptor-str)
                  (ir:expr-label label)))))))

(defun translate-program (prog target)
  (find-and-fill-escapes prog)
  (let ((*frags* nil)
        (*record-descriptor-table* (make-hash-table :test #'equal))
        (level (new-level level-top
                          (temp:new-named-label "tiger_main")
                          nil
                          (frame:new-is-pointer-table)
                          target)))
    (trivia:let-match1 (list _ prog-tagged-ir)
        (translate-expr type:*base-ty-env*
                        (base-ir-env target)
                        level
                        target
                        nil
                        prog)
      (let ((frame (level-inner-frame level)))
        (alloc-fun
         (frame:wrap-ir-entry-exit
          frame
          (ir:stm-move
           (ir:expr-temp (frame:rv target))
           (tagged-ir->expr prog-tagged-ir))
          target)
         frame))
      (nreverse *frags*))))
