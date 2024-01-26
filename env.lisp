(cl:defpackage :cl-tiger/env
  (:use :cl)
  (:local-nicknames
   (:symbol :cl-tiger/symbol)
   (:types :cl-tiger/types)
   (:cl-ds :cl-data-structures)
   (:cl-ds.hamt :cl-data-structures.dicts.hamt))
  (:export
   #:*base-type-env*
   #:get-type
   #:get-unnamed-base-type

   #:value-entry

   #:var-entry
   #:make-var-entry
   #:var-entry-ty

   #:fun-entry
   #:make-fun-entry
   #:fun-entry-formal-types
   #:fun-entry-result-type

   #:get-value))

(cl:in-package :cl-tiger/env)

;; A mapping from symbol:sym to types:ty
(defvar *base-type-env*
  (let ((env (cl-ds.hamt:make-functional-hamt-dictionary #'sxhash #'eq)))
    (reduce (lambda (env binding)
              (cl-ds:insert env (first binding) (second binding)))
            (list (list (symbol:get-sym "int")
                        (types:make-int-ty))
                  (list (symbol:get-sym "string")
                        (types:make-string-ty)))
            :initial-value env)))

(defvar *unnamed-base-type-env*
  (let ((env (cl-ds.hamt:make-functional-hamt-dictionary #'sxhash #'eq)))
    (reduce (lambda (env binding)
              (cl-ds:insert env (first binding) (second binding)))
            (list (list (symbol:get-sym "nil")
                        (types:make-nil-ty))
                  (list (symbol:get-sym "unit")
                        (types:make-unit-ty)))
            :initial-value env)))

(defun get-type (type-env sym)
  (cl-ds:at type-env sym))

(defun get-unnamed-base-type (sym)
  (cl-ds:at *unnamed-base-type-env* sym))

(defclass value-entry ()
  ())

(defclass var-entry (value-entry)
  ((ty
    :type types:ty
    :initform (error "Must supply the type of the var-entry.")
    :initarg :ty
    :accessor var-entry-ty)))

(defun make-var-entry (ty)
  (make-instance 'var-entry :ty ty))

(defclass fun-entry (value-entry)
  ((formal-types
    ;; A list of types:ty
    :type list
    :initform nil
    :initarg :formal-types
    :accessor fun-entry-formal-types)
   (result-type
    :type types:ty
    :initform (error "Must supply the result type of the fun-entry.")
    :initarg :result-type
    :accessor fun-entry-result-type)))

(defun make-fun-entry (formal-types result-type)
  (make-instance 'fun-entry :formal-types formal-types :result-type result-type))

;; A mapping from symbol:sym to value-entry
(defvar *base-value-env*
  (let ((env (cl-ds.hamt:make-functional-hamt-dictionary #'sxhash #'eq))
        (unit-ty (get-unnamed-base-type (symbol:get-sym "unit")))
        (int-ty (get-type *base-type-env* (symbol:get-sym "int")))
        (string-ty (get-type *base-type-env* (symbol:get-sym "string"))))
    (reduce (lambda (env binding)
              (cl-ds:insert env (first binding) (second binding)))
            (list (list (symbol:get-sym "print")
                        ;; print(s: string)
                        ;; print s on std output.
                        (make-fun-entry
                         (list string-ty) unit-ty))
                  (list (symbol:get-sym "flush")
                        ;; flush()
                        ;; flush the std output buffer.
                        (make-fun-entry nil unit-ty))
                  (list (symbol:get-sym "getchar")
                        ;; getchar(): string
                        ;; read a character from std input, return
                        ;; empty string on end of file.
                        (make-fun-entry nil string-ty))
                  (list (symbol:get-sym "ord")
                        ;; ord(s: string): int
                        ;; give ASCII value of first character of s,
                        ;; yield -1 if s is empty string.
                        (make-fun-entry (list string-ty) int-ty))
                  (list (symbol:get-sym "chr")
                        ;; chr(i: int): string
                        ;; single-character string from ASCII value i,
                        ;; halt program if i out of range.
                        (make-fun-entry (list int-ty) string-ty))
                  (list (symbol:get-sym "size")
                        ;; size(s: string): int
                        ;; number of characters in s.
                        (make-fun-entry (list string-ty) int-ty))
                  (list (symbol:get-sym "substring")
                        ;; substring(s: string, first: int, n: int): string
                        ;; substring of string s, starting with
                        ;; character first, n characters long,
                        ;; character are numbered starting at 0.
                        (make-fun-entry (list string-ty int-ty int-ty) string-ty))
                  (list (symbol:get-sym "concat")
                        ;; concat(s1: string, s2: string): string
                        ;; concatenation of s1 and s2.
                        (make-fun-entry (list string-ty string-ty) string-ty))
                  (list (symbol:get-sym "not")
                        ;; not(i: int): int
                        ;; return (i = 0)
                        (make-fun-entry (list int-ty) int-ty))
                  (list (symbol:get-sym "exit")
                        ;; exit(i: int)
                        ;; terminate execution with code i.
                        (make-fun-entry (list int-ty) unit-ty)))
            :initial-value env)))

(defun get-value (value-env sym)
  (cl-ds:at value-env sym))
