(cl:defpackage :cl-tiger/parse
  (:use :cl)
  (:local-nicknames
   (:symbol :cl-tiger/symbol)
   (:ast :cl-tiger/ast))
  (:export
   #:parse-program))

(cl:in-package :cl-tiger/parse)

(defmacro def-whitespace-rule ()
  `(esrap:defrule whitespace
       (+ (or ,@(delete-duplicates
                 (sort (list #\Space #\Tab #\Newline #\Return #\Linefeed #\Page) #'char<))))
     (:constant nil)))

(def-whitespace-rule)

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

(defvar *keyword-table*
  (let ((table (make-hash-table))
        (keywords '("type"
                    "function"
                    "var"
                    "let"
                    "in"
                    "end"
                    "break"
                    "array"
                    "of"
                    "if"
                    "then"
                    "else"
                    "while"
                    "for"
                    "to"
                    "do")))
    (dolist (keyword keywords)
      (setf (gethash (symbol:get-sym keyword) table) t))
    table))

(defun is-non-keyword (id-sym)
  (not (gethash id-sym *keyword-table*)))

(deftoken non-keyword-id
    (is-non-keyword id))

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

(deftoken type-id non-keyword-id
  (:with-pos t))

(esrap:defrule ty-name
    type-id
  (:lambda (result esrap:&bounds start)
    (ast:ty-name result start)))

(esrap:defrule field
    (and non-keyword-id/?s token-colon/?s type-id)
  (:lambda (result esrap:&bounds start)
    (ast:field (nth 0 result) (nth 2 result) start (ast:escape-ref t))))

(deftoken fields
    (esrap:? (and field
                  (* (and (esrap:? skippable) token-comma/?s field))))
  (:lambda (result)
    (cond ((null result) nil)
          (t (cons (first result)
                   (mapcar (lambda (field-with-comma)
                             (nth 2 field-with-comma))
                           (second result)))))))

(esrap:defrule ty-record
    (and token-left-brace/?s fields/?s token-right-brace)
  (:lambda (result)
    (ast:ty-record (second result))))

(esrap:defrule ty-array
    (and keyword-array/?s keyword-of/?s type-id)
  (:lambda (result esrap:&bounds start)
    (ast:ty-array (nth 2 result) start)))

(esrap:defrule ty
    (or ty-array ty-record ty-name))

(esrap:defrule decl-type
    (and keyword-type/?s type-id/?s token-eq/?s ty)
  (:lambda (result esrap:&bounds start)
    (ast:decl-type (nth 1 result) (nth 3 result) start)))

(esrap:defrule decl-types
    (and decl-type
         (* (and (esrap:? skippable) decl-type)))
  (:lambda (result)
    (ast:decl-types
     (cons (first result)
           (mapcar (lambda (decl-type-with-nil)
                     (second decl-type-with-nil))
                   (second result))))))

(esrap:defrule decl-function
    (and keyword-function/?s non-keyword-id/?s
         token-left-paren/?s
         fields/?s
         token-right-paren/?s
         (esrap:? (and token-colon/?s type-id/?s/with-pos))
         token-eq/?s expr)
  (:lambda (result esrap:&bounds start)
    (ast:decl-function
     (nth 1 result)
     (nth 3 result)
     (second (nth 5 result))
     (nth 7 result)
     start)))

(esrap:defrule decl-functions
    (and decl-function
         (* (and (esrap:? skippable) decl-function)))
  (:lambda (result)
    (ast:decl-functions
     (cons (first result)
           (mapcar (lambda (decl-function-with-nil)
                     (second decl-function-with-nil))
                   (second result))))))

(esrap:defrule decl-var
    (and keyword-var/?s non-keyword-id/?s
         (esrap:? (and token-colon/?s type-id/?s/with-pos))
         token-assign/?s expr)
  (:lambda (result esrap:&bounds start)
    (ast:decl-var
     (nth 1 result)
     (second (nth 2 result))
     (nth 4 result)
     start
     (ast:escape-ref t))))

(esrap:defrule decl
    (or decl-functions
        decl-types
        decl-var))

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

(esrap:defrule expr-let
    (and keyword-let/?s zero-or-more-decls/?s keyword-in/?s
         expr-seq-without-parens/?s keyword-end)
  (:lambda (result esrap:&bounds start)
    (ast:expr-let
     (nth 1 result) (nth 3 result) start)))

(deftoken expr expr-op)

(esrap:defrule var-simple non-keyword-id
  (:lambda (result esrap:&bounds start)
    (ast:var-simple result start)))

(esrap:defrule var-field
    (and var "." non-keyword-id)
  (:lambda (result esrap:&bounds start)
    (ast:var-field
     (nth 0 result)
     (nth 2 result)
     start)))

(esrap:defrule var-subscript
    (and var token-left-bracket/?s expr/?s token-right-bracket)
  (:lambda (result esrap:&bounds start)
    (ast:var-subscript
     (nth 0 result)
     (nth 2 result)
     start)))

(deftoken var
    (or var-subscript var-field var-simple))

(esrap:defrule expr-var var
  (:lambda (result)
    (ast:expr-var result)))

(esrap:defrule expr-nil
    "nil"
  (:lambda (result)
    (declare (ignore result))
    ast:expr-nil))

(esrap:defrule expr-int
    (+ digit)
  (:text t)
  (:lambda (result)
    (let ((value (parse-integer result)))
      (when (or (> value most-positive-fixnum)
                (< value most-negative-fixnum))
        (error "The integer ~A is not within the range [~A, ~A]"
               value most-negative-fixnum most-positive-fixnum))
      (ast:expr-int value))))

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

(esrap:defrule expr-string
    (and #\" (* (or escape-seq (not #\")))  #\")
  (:function second)
  (:text t)
  (:lambda (result esrap:&bounds start)
    (loop for i from 0
          for ch across result
          do (let ((code (char-code ch)))
               (unless (<= 0 code 255)
                 (error "Invalid ~Ath character ~A of string ~S, the value of this character is ~A, out of range [0, 255]."
                        i ch result code))))
    (ast:expr-string result start)))

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

(esrap:defrule expr-call
    (and non-keyword-id/?s token-left-paren/?s zero-or-more-expr-by-comma/?s token-right-paren)
  (:lambda (result esrap:&bounds start)
    (ast:expr-call (first result) (third result) start)))

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

(esrap:defrule expr-seq
    (and token-left-paren
         zero-or-more-exprs-by-semicolon-with-pos/?s
         token-right-paren)
  (:lambda (result)
    (ast:expr-seq (second result))))

(deftoken expr-seq-without-parens
    zero-or-more-exprs-by-semicolon-with-pos
  (:lambda (result)
    (ast:expr-seq result)))

(esrap:defrule field-expr
    (and non-keyword-id/?s token-eq/?s expr)
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

(esrap:defrule expr-record
    (and type-id/?s token-left-brace/?s field-exprs/?s token-right-brace)
  (:lambda (result esrap:&bounds start)
    (ast:expr-record (nth 0 result) (nth 2 result) start)))

(esrap:defrule expr-array
    (and type-id/?s
         token-left-bracket/?s expr/?s token-right-bracket/?s
         keyword-of/?s
         expr)
  (:lambda (result esrap:&bounds start)
    (ast:expr-array
     (nth 0 result)
     (nth 2 result)
     (nth 5 result)
     start)))

(esrap:defrule expr-assign
    (and var/?s token-assign/?s expr)
  (:lambda (result esrap:&bounds start)
    (ast:expr-assign
     (first result)
     (third result)
     start)))

(esrap:defrule expr-if
    (and keyword-if/?s expr/?s
         keyword-then/?s expr/?s
         (esrap:? (and keyword-else/?s expr/?s)))
  (:lambda (result esrap:&bounds start)
    (ast:expr-if
     (nth 1 result)
     (nth 3 result)
     (second (nth 4 result))
     start)))

(esrap:defrule expr-while
    (and keyword-while/?s expr/?s keyword-do/?s expr/?s)
  (:lambda (result esrap:&bounds start)
    (ast:expr-while
     (nth 1 result)
     (nth 3 result)
     start)))

(esrap:defrule expr-for
    (and keyword-for/?s
         non-keyword-id/?s token-assign/?s expr/?s
         keyword-to/?s expr/?s
         keyword-do/?s expr/?s)
  (:lambda (result esrap:&bounds start)
    (ast:expr-for
     (nth 1 result)
     (nth 3 result)
     (nth 5 result)
     (nth 7 result)
     start
     (ast:escape-ref t))))

(esrap:defrule expr-break keyword-break
  (:lambda (result esrap:&bounds start)
    (declare (ignore result))
    (ast:expr-break start)))

(esrap:defrule expr-op boolean-or-or-high-prior-term)

(deftoken boolean-or-or-high-prior-term
    (or boolean-or-term boolean-and-or-high-prior-term))

(esrap:defrule boolean-or-term
    (and boolean-or-or-high-prior-term/?s token-boolean-or/?s/with-pos boolean-and-or-high-prior-term)
  (:lambda (result)
    (ast:expr-if
     (nth 0 result)
     (ast:expr-int 1)
     (nth 2 result)
     (second (second result)))))

(deftoken boolean-and-or-high-prior-term
    (or boolean-and-term comparison-or-high-prior-term))

(esrap:defrule boolean-and-term
    (and boolean-and-or-high-prior-term/?s token-boolean-and/?s/with-pos comparison-or-high-prior-term)
  (:lambda (result)
    (ast:expr-if
     (nth 0 result)
     (nth 2 result)
     (ast:expr-int 0)
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
    (when (serapeum:match-of ast:expr (first result)
            ((ast:expr-op _ op _)
             (serapeum:match-of ast:op op
               ((or ast:op-eq ast:op-neq ast:op-gt ast:op-lt ast:op-ge ast:op-le)
                t)
               (_ nil)))
            (_ nil))
      (error "Comparison operators are not associate (pos: ~A)." (second (second result))))
    (ast:expr-op (nth 0 result)
                 (alexandria:eswitch ((first (second result)) :test #'equal)
                   ("=" ast:op-eq)
                   ("<>" ast:op-neq)
                   (">" ast:op-gt)
                   ("<" ast:op-lt)
                   (">=" ast:op-ge)
                   ("<=" ast:op-le))
                 (nth 2 result)
                 (second (second result)))))

(deftoken plus-minus-or-high-prior-term
    (or plus-or-minus-term times-div-or-high-prior-term))

(esrap:defrule plus-or-minus-term
    (or (and plus-minus-or-high-prior-term/?s token-plus/?s/with-pos times-div-or-high-prior-term)
        (and plus-minus-or-high-prior-term/?s token-minus/?s/with-pos times-div-or-high-prior-term))
  (:lambda (result)
    (ast:expr-op (nth 0 result)
                 (alexandria:eswitch ((first (second result)) :test #'equal)
                   ("+" ast:op-plus)
                   ("-" ast:op-minus))
                 (nth 2 result)
                 (second (second result)))))

(deftoken times-div-or-high-prior-term
    (or times-or-div-term unary-minus-or-high-prior-term))

(esrap:defrule times-or-div-term
    (or (and times-div-or-high-prior-term/?s token-times/?s/with-pos unary-minus-or-high-prior-term)
        (and times-div-or-high-prior-term/?s token-div/?s/with-pos unary-minus-or-high-prior-term))
  (:lambda (result)
    (ast:expr-op (nth 0 result)
                 (alexandria:eswitch ((first (second result)) :test #'equal)
                   ("*" ast:op-times)
                   ("/" ast:op-div))
                 (nth 2 result)
                 (second (second result)))))

(esrap:defrule unary-minus-or-high-prior-term
    (or unary-minus-term base-term))

(esrap:defrule unary-minus-term
    (and token-minus/?s/with-pos unary-minus-or-high-prior-term)
  (:lambda (result)
    (ast:expr-op (ast:expr-int 0)
                 ast:op-minus
                 (second result)
                 (second (first result)))))

(esrap:defrule base-term
    (or expr-nil
        expr-int
        expr-string
        expr-break
        expr-let
        expr-if
        expr-while
        expr-for
        expr-call
        expr-seq
        expr-record
        expr-array
        expr-assign
        expr-var))

(esrap:defrule prog
    (and (* skippable) expr (* skippable))
  (:lambda (result)
    (second result)))

(defun parse-program (text &key (start 0) end junk-allowed)
  (esrap:parse 'prog text :start start :end end :junk-allowed junk-allowed))
