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
  (:text t)
  (:lambda (result)
    (symbol:get-sym result)))

(deftoken keyword-type "type")

(deftoken token-equal "=")

(deftoken token-comma ",")

(deftoken token-colon ":")

(deftoken token-semicolon ";")

(deftoken token-left-paren "(")

(deftoken token-right-paren ")")

(deftoken token-left-brace "{")

(deftoken token-right-brace "}")

(deftoken token-left-bracket "[")

(deftoken token-right-bracket "]")

(deftoken token-plus "+")
(deftoken token-minus "-")
(deftoken token-times "*")
(deftoken token-div "/")

(deftoken keyword-array "array")

(deftoken keyword-of "of")

(deftoken type-id id)

(esrap:defrule name-ty
    type-id
  (:lambda (result esrap:&bounds start)
    (ast:make-name-ty result start)))

(esrap:defrule field
    (and id/?s token-colon/?s type-id)
  (:lambda (result esrap:&bounds start)
    (ast:make-field (nth 0 result) (nth 2 result) start)))

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

(deftoken expr arith-expr)

(esrap:defrule simple-var id
  (:lambda (result esrap:&bounds start)
    (ast:make-simple-var result start)))

(esrap:defrule field-var
    (and var "." id)
  (:lambda (result esrap:&bounds start)
    (ast:make-field-var
     (nth 0 result)
     (nth 2 result)
     start)))

(esrap:defrule subscript-var
    (and var token-left-bracket/?s expr/?s token-right-bracket)
  (:lambda (result esrap:&bounds start)
    (ast:make-subscript-var
     (nth 0 result)
     (nth 2 result)
     start)))

(esrap:defrule var
    (or subscript-var field-var simple-var))

(esrap:defrule var-expr var
  (:lambda (result)
    (ast:make-var-expr result)))

(esrap:defrule nil-expr
    "nil"
  (:lambda (result)
    (ast:make-nil-expr)))

(esrap:defrule int-expr
    (+ digit)
  (:text t)
  (:lambda (result)
    (let ((value (parse-integer result)))
      (when (or (> value most-positive-fixnum)
                (< value most-negative-fixnum))
        (error "The integer ~A is not within the range [~A, ~A]"
               value most-negative-fixnum most-positive-fixnum))
      (ast:make-int-expr value))))

(esrap:defrule expr-with-pos expr
  (:lambda (result esrap:&bounds start)
    (list result start)))

(esrap:defrule one-or-more-expr-with-pos-list
    (and expr-with-pos
         (* (and (esrap:? skippable) token-semicolon/?s expr-with-pos)))
  (:lambda (result)
    (cons (first result)
          (mapcar (lambda (expr-with-nil)
                    (nth 2 expr-with-nil))
                  (second result)))))

(esrap:defrule seq-expr
    (and token-left-paren
         (esrap:? one-or-more-expr-with-pos-list)
         token-right-paren)
  (:lambda (result)
    (ast:make-seq-expr (second result))))

(esrap:defrule arith-expr plus-minus-or-high-prior-term)

(deftoken plus-minus-or-high-prior-term
    (or (and plus-minus-or-high-prior-term/?s token-plus/?s times-div-or-high-prior-term)
        (and plus-minus-or-high-prior-term/?s token-minus/?s times-div-or-high-prior-term)
        times-div-or-high-prior-term)
  (:lambda (result)
    (if (listp result)
        (let ((op (second result)))
          (cond ((and (stringp op) (string= op "+"))
                 (ast:make-op-expr (nth 0 result) :plus (nth 2 result)))
                ((and (stringp op) (string= op "-"))
                 (ast:make-op-expr (nth 0 result) :minus (nth 2 result)))
                (t result)))
        result)))

(deftoken times-div-or-high-prior-term
    (or (and times-div-or-high-prior-term/?s token-times/?s unary-minus-or-high-prior-term)
        (and times-div-or-high-prior-term/?s token-div/?s unary-minus-or-high-prior-term)
        unary-minus-or-high-prior-term)
  (:lambda (result)
    (if (listp result)
        (let ((op (second result)))
          (cond ((and (stringp op) (string= op "*"))
                 (ast:make-op-expr (nth 0 result) :times (nth 2 result)))
                ((and (stringp op) (string= op "/"))
                 (ast:make-op-expr (nth 0 result) :div (nth 2 result)))
                (t result)))
        result)))

(esrap:defrule unary-minus-or-high-prior-term
    (or (and token-minus/?s unary-minus-or-high-prior-term) base-term)
  (:lambda (result)
    (if (listp result)
        (let ((op (first result)))
          (cond ((and (stringp op) (string= op "-"))
                 (ast:make-op-expr (ast:make-int-expr 0) :minus (nth 1 result)))
                (t result)))
        result)))

(esrap:defrule base-term
    (or nil-expr
        int-expr
        var-expr
        seq-expr))

;; Why following rules cannot work as above rules?
;; (esrap:parse 'expr "3 + 4 + 5 - 6") will raise error
;; The two set of rules seem equivalent.

;; (deftoken plus-minus-or-high-prior-term
;;     (or plus-term minus-term times-div-or-high-prior-term))

;; (esrap:defrule plus-term
;;     (and plus-minus-or-high-prior-term/?s token-plus/?s times-div-or-high-prior-term)
;;   (:lambda (result)
;;     (ast:make-op-expr (nth 0 result) :plus (nth 2 result))))

;; (esrap:defrule minus-term
;;     (and plus-minus-or-high-prior-term/?s token-minus/?s times-div-or-high-prior-term)
;;   (:lambda (result)
;;     (ast:make-op-expr (nth 0 result) :minus (nth 2 result))))

;; (deftoken times-div-or-high-prior-term
;;     (or times-term div-term unary-minus-or-high-prior-term))

;; (esrap:defrule times-term
;;     (and times-div-or-high-prior-term/?s token-times/?s unary-minus-or-high-prior-term)
;;   (:lambda (result)
;;     (ast:make-op-expr (nth 0 result) :times (nth 2 result))))

;; (esrap:defrule div-term
;;     (and times-div-or-high-prior-term/?s token-div/?s unary-minus-or-high-prior-term)
;;   (:lambda (result)
;;     (ast:make-op-expr (nth 0 result) :div (nth 2 result))))

;; (esrap:defrule unary-minus-or-high-prior-term
;;     (or unary-minus-term base-term))

;; (esrap:defrule unary-minus-term
;;     (and token-minus/?s unary-minus-or-high-prior-term)
;;   (:lambda (result)
;;     (ast:make-op-expr (ast:make-int-expr 0) :minus (nth 1 result))))

;; (esrap:defrule base-term
;;     (or nil-expr
;;         int-expr
;;         var-expr
;;         seq-expr))
