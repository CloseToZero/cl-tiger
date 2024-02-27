(cl:defpackage :cl-tiger/types
  (:use :cl)
  (:local-nicknames
   (:symbol :cl-tiger/symbol))
  (:export
   #:ty
   #:ty-int
   #:ty-string
   #:ty-record
   #:ty-record-fields
   #:ty-array
   #:ty-array-base-type
   #:ty-nil
   #:ty-unit
   #:ty-name
   #:ty-name-sym
   #:ty-name-ty-ref
   #:ty-ref
   #:ty-ref-value
   #:actual-ty
   #:type-compatible
   #:upgrade-from-compatible-types
   #:upgrade-from-compatible-short-types
   #:short-type->string
   #:*base-type-env*
   #:get-type
   #:get-unnamed-base-type
   #:insert-type
   #:*built-in-function-bindings*))

(cl:in-package :cl-tiger/types)

(serapeum:defunion ty
  ty-int
  ty-string
  (ty-record
   ;; A list of (sym ty)
   (fields list))
  (ty-array
   (base-type ty))
  ty-nil
  ty-unit
  (ty-name
   (sym symbol:sym)
   (ty-ref ty-ref)))

(defstruct (ty-ref
            (:conc-name ty-ref-)
            (:constructor ty-ref (value)))
  (value nil :type (or ty null)))

(defun actual-ty (ty)
  (serapeum:match-of ty ty
    ((ty-name _ ty-ref)
     (if (null ty) ty (actual-ty (ty-ref-value ty-ref))))
    (_ ty)))

;; Compare two actual types.
(defun type-compatible (ty-1 ty-2)
  (or (eq ty-1 ty-2)
      (trivia:match (list ty-1 ty-2)
        ((or (list (ty-record _) (ty-nil))
             (list (ty-nil) (ty-record _)))
         t)
        (_ nil))))

(defun upgrade-from-compatible-types (ty-1 ty-2)
  (trivia:match (list ty-1 ty-2)
    ((list (ty-record _) (ty-nil))
     ty-1)
    ((list (ty-nil) (ty-record _))
     ty-2)
    (_ ty-1)))

(defun upgrade-from-compatible-short-types (short-ty-1 short-ty-2)
  (trivia:match (list short-ty-1 short-ty-2)
    ((list (ty-name _) (ty-nil))
     short-ty-1)
    ((list (ty-nil) (ty-name _))
     short-ty-2)
    (_ short-ty-1)))

(defun short-type->string (short-ty)
  (serapeum:match-of ty short-ty
    (ty-int "int")
    (ty-string "string")
    (ty-record
     (let ((*print-circle* t))
       (error "Short type should not be a record type, but got: ~S." short-ty)))
    (ty-array
     (let ((*print-circle* t))
       (error "Short type should not be an array type, but got: ~S." short-ty)))
    (ty-nil "nil")
    (ty-unit "unit")
    ((ty-name sym _)
     (symbol:sym-name sym))))

;; A map from symbol:sym to ty
(defvar *base-type-env*
  (reduce (lambda (env binding)
            (fset:with env (first binding) (second binding)))
          (list (list (symbol:get-sym "int")
                      ty-int)
                (list (symbol:get-sym "string")
                      ty-string))
          :initial-value (fset:empty-map)))

(defvar *unnamed-base-type-env*
  (reduce (lambda (env binding)
            (fset:with env (first binding) (second binding)))
          (list (list (symbol:get-sym "nil")
                      ty-nil)
                (list (symbol:get-sym "unit")
                      ty-unit))
          :initial-value (fset:empty-map)))

(defun get-type (type-env sym)
  (fset:@ type-env sym))

(defun get-unnamed-base-type (sym)
  (fset:@ *unnamed-base-type-env* sym))

(defun insert-type (type-env sym ty)
  (fset:with type-env sym ty))

(defvar *built-in-function-bindings*
  (let ((ty-unit (get-unnamed-base-type (symbol:get-sym "unit")))
        (ty-int (get-type *base-type-env* (symbol:get-sym "int")))
        (ty-string (get-type *base-type-env* (symbol:get-sym "string"))))
    (list (list "print"
                ;; print(s: string)
                ;; print s on std output.
                (list ty-string) ty-unit)
          (list "printn"
                ;; print(n: int)
                ;; print n on std output.
                (list ty-int) ty-unit)
          (list "flush"
                ;; flush()
                ;; flush the std output buffer.
                nil ty-unit)
          (list "getchar"
                ;; getchar(): string
                ;; read a character from std input, return
                ;; empty string on end of file.
                nil ty-string)
          (list "ord"
                ;; ord(s: string): int
                ;; give ASCII value of first character of s,
                ;; yield -1 if s is empty string.
                (list ty-string) ty-int)
          (list "chr"
                ;; chr(i: int): string
                ;; single-character string from ASCII value i,
                ;; halt program if i out of range.
                (list ty-int) ty-string)
          (list "size"
                ;; size(s: string): int
                ;; number of characters in s.
                (list ty-string) ty-int)
          (list "substring"
                ;; substring(s: string, first: int, n: int): string
                ;; substring of string s, starting with
                ;; character first, n characters long,
                ;; character are numbered starting at 0.
                (list ty-string ty-int ty-int) ty-string)
          (list "concat"
                ;; concat(s1: string, s2: string): string
                ;; concatenation of s1 and s2.
                (list ty-string ty-string) ty-string)
          (list "not"
                ;; not(i: int): int
                ;; return (i = 0)
                (list ty-int) ty-int)
          (list "exit"
                ;; exit(i: int)
                ;; terminate execution with code i.
                (list ty-int) ty-unit))))
