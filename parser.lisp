(cl:defpackage :cl-tiger/parser
  (:use :cl)
  (:local-nicknames
   (:symbol :cl-tiger/symbol)
   (:ast :cl-tiger/ast)))

(cl:in-package :cl-tiger/parser)

(esrap:defrule whitespace
    (+ (or #\Space #\Tab #\Newline))
  (:constant nil))

(esrap:defrule comment
    (and "/*" (* (or comment (not "*/"))) "*/")
  (:constant nil))

(esrap:defrule skippable
    (+ (or whitespace comment))
  (:constant nil))

(defmacro deftoken (name expression &body options)
  (let ((name/skippable (alexandria:symbolicate name '#:/s))
        (name/maybe-skippable (alexandria:symbolicate name '#:/?s)))
    `(progn
       (esrap:defrule ,name ,expression ,@options)
       (esrap:defrule ,name/skippable
           (and ,name skippable)
         (:function first))
       (esrap:defrule ,name/maybe-skippable
           (and ,name (esrap:? skippable))
         (:function first)))))

(esrap:defrule letter
    (esrap:character-ranges (#\a #\z) (#\A #\Z)))

(esrap:defrule digit
    (esrap:character-ranges (#\0 #\9)))

(deftoken id
    (and letter (* (or letter digit #\_)))
  (:text t))

(deftoken keyword-type "type")

(deftoken token-equal "=")

(deftoken token-comma ",")

(deftoken token-colon ":")

(deftoken token-left-brace "{")

(deftoken token-right-brace "}")

(deftoken keyword-array "array")

(deftoken keyword-of "of")

(deftoken type-id id
  (:lambda (result)
    (cl-tiger/symbol:get-sym result)))

(esrap:defrule name-ty
    type-id
  (:lambda (result esrap:&bounds start)
    (cl-tiger/ast:make-name-ty result start)))

(esrap:defrule field
    (and id/?s token-colon/?s type-id)
  (:lambda (result esrap:&bounds start)
    (cl-tiger/ast:make-field
     (cl-tiger/symbol:get-sym (nth 0 result))
     (nth 2 result)
     start)))

(deftoken fields
    (esrap:? (and field
                  (* (and (esrap:? skippable) token-comma/?s field))))
  (:lambda (result)
    (cond ((null result) nil)
          (t (cons (first result)
                   (mapcar (lambda (field-with-comma)
                             (nth 2 field-with-comma))
                           (second result)))))))

(esrap:defrule record-ty
    (and token-left-brace/?s fields/?s token-right-brace)
  (:lambda (result)
    (ast:make-record-ty (second result))))

(esrap:defrule array-ty
    (and keyword-array/?s keyword-of/?s type-id)
  (:lambda (result esrap:&bounds start)
    (ast:make-array-ty (nth 2 result) start)))

(esrap:defrule ty
    (or array-ty record-ty name-ty))

(esrap:defrule type-decl
    (and keyword-type/?s type-id/?s token-equal/?s ty)
  (:lambda (result esrap:&bounds start)
    (ast:make-type-decl (nth 1 result) (nth 3 result) start)))

(esrap:defrule type-decls
    (esrap:? (and type-decl
                  (* (and (esrap:? skippable) type-decl))))
  (:lambda (result)
    (ast:make-type-decls
     (cond ((null result) nil)
           (t (cons (first result)
                    (mapcar (lambda (type-decl-with-nil)
                              (second type-decl-with-nil))
                            (second result))))))))
