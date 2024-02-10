(cl:defpackage :cl-tiger/types
  (:use :cl)
  (:local-nicknames
   (:symbol :cl-tiger/symbol))
  (:export
   #:ty
   #:int-ty
   #:string-ty
   #:record-ty
   #:record-ty-fields
   #:array-ty
   #:array-ty-base-type
   #:nil-ty
   #:unit-ty
   #:name-ty
   #:name-ty-sym
   #:name-ty-ty-ref
   #:ty-ref
   #:ty-ref-value
   #:actual-ty
   #:type-compatible
   #:upgrade-from-compatible-types
   #:*base-type-env*
   #:get-type
   #:get-unnamed-base-type
   #:insert-type
   #:*built-in-function-bindings*))

(cl:in-package :cl-tiger/types)

(serapeum:defunion ty
  int-ty
  string-ty
  (record-ty
   ;; A list of (sym ty)
   (fields list))
  (array-ty
   (base-type ty))
  nil-ty
  unit-ty
  (name-ty
   (sym symbol:sym)
   (ty-ref ty-ref)))

(defstruct (ty-ref
            (:conc-name ty-ref-)
            (:constructor ty-ref (value)))
  (value nil :type (or ty null)))

(defun actual-ty (ty)
  (serapeum:match-of ty ty
    ((name-ty _ ty-ref)
     (if (null ty) ty (actual-ty (ty-ref-value ty-ref))))
    ((array-ty base-type) (array-ty (actual-ty base-type)))
    ;; NOTE: We don't need to recursive into record-ty since we use nominal type system.
    (_ ty)))

;; Compare two actual types.
(defun type-compatible (ty1 ty2)
  (or (eq ty1 ty2)
      (trivia:match (list ty1 ty2)
        ((list (array-ty base-type-1)
               (array-ty base-type-2))
         (type-compatible base-type-1 base-type-2))
        ((or (list (record-ty) (nil-ty))
             (list (nil-ty) (record-ty)))
         t)
        (_ nil))))

(defun upgrade-from-compatible-types (ty1 ty2)
  (trivia:match (list ty1 ty2)
    ((list (record-ty) (nil-ty))
     ty1)
    ((list (nil-ty) (record-ty))
     ty2)
    (_ ty1)))

;; A map from symbol:sym to ty
(defvar *base-type-env*
  (reduce (lambda (env binding)
            (fset:with env (first binding) (second binding)))
          (list (list (symbol:get-sym "int")
                      int-ty)
                (list (symbol:get-sym "string")
                      string-ty))
          :initial-value (fset:empty-map)))

(defvar *unnamed-base-type-env*
  (reduce (lambda (env binding)
            (fset:with env (first binding) (second binding)))
          (list (list (symbol:get-sym "nil")
                      nil-ty)
                (list (symbol:get-sym "unit")
                      unit-ty))
          :initial-value (fset:empty-map)))

(defun get-type (type-env sym)
  (fset:@ type-env sym))

(defun get-unnamed-base-type (sym)
  (fset:@ *unnamed-base-type-env* sym))

(defun insert-type (type-env sym ty)
  (fset:with type-env sym ty))

(defvar *built-in-function-bindings*
  (let ((unit-ty (get-unnamed-base-type (symbol:get-sym "unit")))
        (int-ty (get-type *base-type-env* (symbol:get-sym "int")))
        (string-ty (get-type *base-type-env* (symbol:get-sym "string"))))
    (list (list "print"
                ;; print(s: string)
                ;; print s on std output.
                (list string-ty) unit-ty)
          (list "flush"
                ;; flush()
                ;; flush the std output buffer.
                nil unit-ty)
          (list "getchar"
                ;; getchar(): string
                ;; read a character from std input, return
                ;; empty string on end of file.
                nil string-ty)
          (list "ord"
                ;; ord(s: string): int
                ;; give ASCII value of first character of s,
                ;; yield -1 if s is empty string.
                (list string-ty) int-ty)
          (list "chr"
                ;; chr(i: int): string
                ;; single-character string from ASCII value i,
                ;; halt program if i out of range.
                (list int-ty) string-ty)
          (list "size"
                ;; size(s: string): int
                ;; number of characters in s.
                (list string-ty) int-ty)
          (list "substring"
                ;; substring(s: string, first: int, n: int): string
                ;; substring of string s, starting with
                ;; character first, n characters long,
                ;; character are numbered starting at 0.
                (list string-ty int-ty int-ty) string-ty)
          (list "concat"
                ;; concat(s1: string, s2: string): string
                ;; concatenation of s1 and s2.
                (list string-ty string-ty) string-ty)
          (list "not"
                ;; not(i: int): int
                ;; return (i = 0)
                (list int-ty) int-ty)
          (list "exit"
                ;; exit(i: int)
                ;; terminate execution with code i.
                (list int-ty) unit-ty))))
