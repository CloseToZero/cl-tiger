(cl:defpackage :cl-tiger/parser
  (:use :cl)
  (:local-nicknames
   (:symbol :cl-tiger/symbol)
   (:ast :cl-tiger/ast)))

(cl:in-package :cl-tiger/parser)

(esrap:defrule whitespace
    (+ (or #\Space #\Tab #\Newline #\Return #\Linefeed #\Page))
  (:constant nil))

(esrap:defrule comment
    (and "/*" (* (or comment (not "*/"))) "*/")
  (:constant nil))

(esrap:defrule skippable
    (+ (or whitespace comment))
  (:constant nil))

(defmacro deftoken (name expression &body options)
  (let ((name/skippable (alexandria:symbolicate name '#:/s))
        (name/maybe-skippable (alexandria:symbolicate name '#:/?s))
        (with-pos? (let ((value (assoc :with-pos options))) (second value)))
        (options (remove-if (lambda (option)
                              (eql (car option) :with-pos))
                            options)))
    `(progn
       (esrap:defrule ,name ,expression ,@options)
       (esrap:defrule ,name/skippable
           (and ,name skippable)
         (:function first))
       (esrap:defrule ,name/maybe-skippable
           (and ,name (esrap:? skippable))
         (:function first))
       ,@(when with-pos?
           (mapcar (lambda (name)
                     `(esrap:defrule ,(alexandria:symbolicate name '#:/with-pos)
                          ,name
                        ,@options
                          (:lambda (result esrap:&bounds start)
                            (list result start))))
                   (list name name/skippable name/maybe-skippable)))
       (values))))

(esrap:defrule letter
    (esrap:character-ranges (#\a #\z) (#\A #\Z)))

(esrap:defrule digit
    (esrap:character-ranges (#\0 #\9))
  (:text t))

(deftoken id
    (and letter (* (or letter digit #\_)))
  (:text t)
  (:lambda (result)
    (symbol:get-sym result)))

(deftoken keyword-type "type")

(deftoken keyword-function "function")

(deftoken keyword-var "var")

(deftoken keyword-let "let")

(deftoken keyword-in "in")

(deftoken keyword-end "end")

(deftoken keyword-break "break")

(deftoken token-comma ",")

(deftoken token-colon ":")

(deftoken token-semicolon ";")

(deftoken token-left-paren "(")

(deftoken token-right-paren ")")

(deftoken token-left-brace "{")

(deftoken token-right-brace "}")

(deftoken token-left-bracket "[")

(deftoken token-right-bracket "]")

(deftoken token-plus "+"
  (:with-pos t))
(deftoken token-minus "-"
  (:with-pos t))
(deftoken token-times "*"
  (:with-pos t))
(deftoken token-div "/"
  (:with-pos t))

(deftoken token-eq "="
  (:with-pos t))
(deftoken token-neq "<>"
  (:with-pos t))
(deftoken token-gt ">"
  (:with-pos t))
(deftoken token-lt "<"
  (:with-pos t))
(deftoken token-ge ">="
  (:with-pos t))
(deftoken token-le "<="
  (:with-pos t))

(deftoken token-assign ":=")

(deftoken token-boolean-and "&"
  (:with-pos t))

(deftoken token-boolean-or "|"
  (:with-pos t))

(deftoken keyword-array "array")

(deftoken keyword-of "of")

(deftoken keyword-if "if")
(deftoken keyword-then "then")
(deftoken keyword-else "else")

(deftoken keyword-while "while")

(deftoken keyword-for "for")

(deftoken keyword-to "to")

(deftoken keyword-do "do")

(deftoken type-id id
  (:with-pos t))

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
    (and keyword-type/?s type-id/?s token-eq/?s ty)
  (:lambda (result esrap:&bounds start)
    (ast:make-type-decl (nth 1 result) (nth 3 result) start)))

(esrap:defrule type-decls
    (and type-decl
         (* (and (esrap:? skippable) type-decl)))
  (:lambda (result)
    (ast:make-type-decls
     (cons (first result)
           (mapcar (lambda (type-decl-with-nil)
                     (second type-decl-with-nil))
                   (second result))))))

(esrap:defrule function-decl
    (and keyword-function/?s id/?s
         token-left-paren/?s
         fields/?s
         token-right-paren/?s
         (esrap:? (and token-colon/?s type-id/?s/with-pos))
         token-eq/?s expr)
  (:lambda (result esrap:&bounds start)
    (ast:make-function-decl
     (nth 1 result)
     (nth 3 result)
     (second (nth 5 result))
     (nth 7 result)
     start)))

(esrap:defrule function-decls
    (and function-decl
         (* (and (esrap:? skippable) function-decl)))
  (:lambda (result)
    (ast:make-function-decls
     (cons (first result)
           (mapcar (lambda (function-decl-with-nil)
                     (second function-decl-with-nil))
                   (second result))))))

(esrap:defrule var-decl
    (and keyword-var/?s id/?s
         (esrap:? (and token-colon/?s type-id/?s/with-pos))
         token-assign/?s expr)
  (:lambda (result esrap:&bounds start)
    (ast:make-var-decl
     (nth 1 result)
     (second (nth 2 result))
     (nth 4 result)
     start)))

(esrap:defrule decl
    (or function-decls
        type-decls
        var-decl))

(esrap:defrule one-or-more-decls
    (and decl
         (* (and (esrap:? skippable) decl)))
  (:lambda (result)
    (cons (first result)
          (mapcar (lambda (decl-with-nil)
                    (second decl-with-nil))
                  (second result)))))

(deftoken zero-or-more-decls
    (esrap:? one-or-more-decls))

(esrap:defrule let-expr
    (and keyword-let/?s zero-or-more-decls/?s keyword-in/?s
         seq-expr-without-parens/?s keyword-end)
  (:lambda (result esrap:&bounds start)
    (ast:make-let-expr
     (nth 1 result) (nth 3 result) start)))

(deftoken expr op-expr)

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

(deftoken var
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

(esrap:defrule escape-seq
    (and #\\ (or #\n
                 #\t
                 (and #\^ character)
                 (and digit digit digit)
                 #\"
                 #\\
                 (+ skippable)))
  (:lambda (result)
    (let ((code (second result)))
      (cond ((equal code '(nil))
             ;; \f__f\
             nil)
            ((and (listp code)
                  (let ((e-1 (first code)))
                    (and (stringp e-1)
                         (= (length e-1) 1)
                         (digit-char-p (elt e-1 0))))) 
             ;; \ddd
             (let ((char-value (parse-integer (apply #'format nil "~A~A~A" code))))
               (code-char char-value)))
            ((and (listp code)
                  (string= (first code) #\^))
             ;; \^c
             (code-char (logxor #x40 (char-code (char-upcase (second code))))))
            (t
             (alexandria:eswitch (code :test #'equal)
               ("n" #\Newline)
               ("t" #\Tab)
               ("\"" #\")
               ("\\" #\\))))))
  (:text t))

(esrap:defrule string-expr
    (and #\" (* (or escape-seq (not #\")))  #\")
  (:function second)
  (:text t)
  (:lambda (result esrap:&bounds start)
    (ast:make-string-expr result start)))

(esrap:defrule one-or-more-expr-by-comma
    (and expr
         (* (and (esrap:? skippable) token-comma/?s expr)))
  (:lambda (result)
    (cons (first result)
          (mapcar (lambda (expr-with-nil)
                    (nth 2 expr-with-nil))
                  (second result)))))

(deftoken zero-or-more-expr-by-comma
    (esrap:? one-or-more-expr-by-comma))

(esrap:defrule call-expr
    (and id/?s token-left-paren/?s zero-or-more-expr-by-comma/?s token-right-paren)
  (:lambda (result esrap:&bounds start)
    (ast:make-call-expr (first result) (third result) start)))

(esrap:defrule expr-with-pos expr
  (:lambda (result esrap:&bounds start)
    (list result start)))

(esrap:defrule one-or-more-exprs-by-semicolon-with-pos
    (and expr-with-pos
         (* (and (esrap:? skippable) token-semicolon/?s expr-with-pos)))
  (:lambda (result)
    (cons (first result)
          (mapcar (lambda (expr-with-nil)
                    (nth 2 expr-with-nil))
                  (second result)))))

(deftoken zero-or-more-exprs-by-semicolon-with-pos
    (esrap:? one-or-more-exprs-by-semicolon-with-pos))

(esrap:defrule seq-expr
    (and token-left-paren
         zero-or-more-exprs-by-semicolon-with-pos/?s
         token-right-paren)
  (:lambda (result)
    (ast:make-seq-expr (second result))))

(deftoken seq-expr-without-parens
    zero-or-more-exprs-by-semicolon-with-pos/?s
  (:lambda (result)
    (ast:make-seq-expr result)))

(esrap:defrule field-expr
    (and id/?s token-eq/?s expr)
  (:lambda (result esrap:&bounds start)
    (list (nth 0 result) (nth 2 result) start)))

(deftoken field-exprs
    (esrap:? (and field-expr
                  (* (and (esrap:? skippable) token-comma/?s field-expr))))
  (:lambda (result)
    (cond ((null result) nil)
          (t (cons (first result)
                   (mapcar (lambda (field-expr-with-comma)
                             (nth 2 field-expr-with-comma))
                           (second result)))))))

(esrap:defrule record-expr
    (and type-id/?s token-left-brace/?s field-exprs/?s token-right-brace)
  (:lambda (result esrap:&bounds start)
    (ast:make-record-expr (nth 0 result) (nth 2 result) start)))

(esrap:defrule array-expr
    (and type-id/?s
         token-left-bracket/?s expr/?s token-right-bracket/?s
         keyword-of/?s
         expr)
  (:lambda (result esrap:&bounds start)
    (ast:make-array-expr
     (nth 0 result)
     (nth 2 result)
     (nth 5 result)
     start)))

(esrap:defrule assign-expr
    (and var/?s token-assign/?s expr)
  (:lambda (result esrap:&bounds start)
    (ast:make-assign-expr
     (first result)
     (third result)
     start)))

(esrap:defrule if-then-expr
    (and keyword-if/?s expr/?s
         keyword-then/?s expr/?s
         (esrap:? (and keyword-else/?s expr/?s)))
  (:lambda (result esrap:&bounds start)
    (ast:make-if-expr
     (nth 1 result)
     (nth 3 result)
     (second (nth 4 result))
     start)))

(esrap:defrule while-expr
    (and keyword-while/?s expr/?s keyword-do/?s expr/?s)
  (:lambda (result esrap:&bounds start)
    (ast:make-while-expr
     (nth 1 result)
     (nth 3 result)
     start)))

(esrap:defrule for-expr
    (and keyword-for/?s
         id/?s token-assign/?s expr/?s
         keyword-to/?s expr/?s
         keyword-do/?s expr/?s)
  (:lambda (result esrap:&bounds start)
    (ast:make-for-expr
     (nth 1 result)
     (nth 3 result)
     (nth 5 result)
     (nth 7 result)
     start)))

(esrap:defrule break-expr keyword-break
  (:lambda (result esrap:&bounds start)
    (ast:make-break-expr start)))

(esrap:defrule op-expr boolean-or-or-high-prior-term)

(deftoken boolean-or-or-high-prior-term
    (or boolean-or-term boolean-and-or-high-prior-term))

(esrap:defrule boolean-or-term
    (and boolean-or-or-high-prior-term/?s token-boolean-or/?s/with-pos boolean-and-or-high-prior-term)
  (:lambda (result)
    (ast:make-if-expr
     (nth 0 result)
     (ast:make-int-expr 1)
     (nth 2 result)
     (second (second result)))))

(deftoken boolean-and-or-high-prior-term
    (or boolean-and-term comparison-or-high-prior-term))

(esrap:defrule boolean-and-term
    (and boolean-and-or-high-prior-term/?s token-boolean-and/?s/with-pos comparison-or-high-prior-term)
  (:lambda (result)
    (ast:make-if-expr
     (nth 0 result)
     (nth 2 result)
     (ast:make-int-expr 0)
     (second (second result)))))

(deftoken comparison-or-high-prior-term
    (or comparison-term plus-minus-or-high-prior-term))

(esrap:defrule comparison-term
    (or (and comparison-or-high-prior-term/?s token-eq/?s/with-pos plus-minus-or-high-prior-term)
        (and comparison-or-high-prior-term/?s token-neq/?s/with-pos plus-minus-or-high-prior-term)
        (and comparison-or-high-prior-term/?s token-gt/?s/with-pos plus-minus-or-high-prior-term)
        (and comparison-or-high-prior-term/?s token-lt/?s/with-pos plus-minus-or-high-prior-term)
        (and comparison-or-high-prior-term/?s token-ge/?s/with-pos plus-minus-or-high-prior-term)
        (and comparison-or-high-prior-term/?s token-le/?s/with-pos plus-minus-or-high-prior-term))
  (:lambda (result)
    (when (trivia:match (first result)
            ((ast:op-expr :op op)
             (member op '(:eq :neq :gt :lt :ge :le))))
      (error "Comparison operators are not associate (pos: ~A)" (second (second result))))
    (ast:make-op-expr (nth 0 result)
                      (alexandria:eswitch ((first (second result)) :test #'equal)
                        ("=" :eq)
                        ("<>" :neq)
                        (">" :gt)
                        ("<" :lt)
                        (">=" :ge)
                        ("<=" :le))
                      (nth 2 result)
                      (second (second result)))))

(deftoken plus-minus-or-high-prior-term
    (or plus-or-minus-term times-div-or-high-prior-term))

(esrap:defrule plus-or-minus-term
    (or (and plus-minus-or-high-prior-term/?s token-plus/?s/with-pos times-div-or-high-prior-term)
        (and plus-minus-or-high-prior-term/?s token-minus/?s/with-pos times-div-or-high-prior-term))
  (:lambda (result)
    (ast:make-op-expr (nth 0 result)
                      (alexandria:eswitch ((first (second result)) :test #'equal)
                        ("+" :plus)
                        ("-" :minus))
                      (nth 2 result)
                      (second (second result)))))

(deftoken times-div-or-high-prior-term
    (or times-or-div-term unary-minus-or-high-prior-term))

(esrap:defrule times-or-div-term
    (or (and times-div-or-high-prior-term/?s token-times/?s/with-pos unary-minus-or-high-prior-term)
        (and times-div-or-high-prior-term/?s token-div/?s/with-pos unary-minus-or-high-prior-term))
  (:lambda (result)
    (ast:make-op-expr (nth 0 result)
                      (alexandria:eswitch ((first (second result)) :test #'equal)
                        ("*" :times)
                        ("/" :div))
                      (nth 2 result)
                      (second (second result)))))

(esrap:defrule unary-minus-or-high-prior-term
    (or unary-minus-term base-term))

(esrap:defrule unary-minus-term
    (and token-minus/?s/with-pos unary-minus-or-high-prior-term)
  (:lambda (result)
    (ast:make-op-expr (ast:make-int-expr 0)
                      :minus
                      (second result)
                      (second (first result)))))

(esrap:defrule base-term
    (or nil-expr
        int-expr
        string-expr
        break-expr
        let-expr
        if-then-expr
        while-expr
        for-expr
        call-expr
        seq-expr
        record-expr
        array-expr
        assign-expr
        var-expr))

(esrap:defrule prog
    (and (* skippable) expr (* skippable))
  (:lambda (result)
    (second result)))
