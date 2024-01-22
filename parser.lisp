(cl:defpackage :cl-tiger/parser
  (:use :cl))

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
  (:function (lambda (result)
               (cl-tiger/ast:make-type-id result))))

(deftoken type-fields
    (esrap:? (and id/?s token-colon/?s type-id
                  (* (and (esrap:? skippable) token-comma/?s id/?s token-colon/?s type-id))))
  (:function (lambda (result)
               (cond ((null result) nil)
                     (t (let ((rest-type-fields (nth 3 result)))
                          (cons (list (nth 0 result) (nth 2 result))
                                (mapcar (lambda (type-field-with-comma)
                                          (list (nth 2 type-field-with-comma)
                                                (nth 4 type-field-with-comma)))
                                        rest-type-fields))))))))

(esrap:defrule record-type
    (and token-left-brace/?s type-fields/?s token-right-brace)
  (:function (lambda (result)
               (cl-tiger/ast:make-record-type (second result)))))

(esrap:defrule array-type
    (and keyword-array/?s keyword-of/?s type-id)
  (:function (lambda (result)
               (cl-tiger/ast:make-array-type (nth 2 result)))))

(esrap:defrule type
    (or array-type record-type type-id))

(esrap:defrule type-declaration
    (and keyword-type/?s type-id/?s token-equal/?s type)
  (:function (lambda (result)
               (cl-tiger/ast:make-type-declaration (nth 1 result) (nth 3 result)))))

(esrap:defrule type-declarations
    (and type-declaration
         (* (and (esrap:? skippable) type-declaration)))
  (:function (lambda (result)
               (cl-tiger/ast:make-type-declarations
                (cons (first result)
                      (mapcar (lambda (type-declaration-with-nil)
                                (second type-declaration-with-nil))
                              (second result)))))))
