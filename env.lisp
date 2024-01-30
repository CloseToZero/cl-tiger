(cl:defpackage :cl-tiger/env
  (:use :cl)
  (:local-nicknames
   (:symbol :cl-tiger/symbol)
   (:types :cl-tiger/types)
   (:cl-ds :cl-data-structures)
   (:cl-ds.hamt :cl-data-structures.dicts.hamt))
  (:export
   #:*base-type-env*
   #:*base-value-env*

   #:get-type
   #:get-unnamed-base-type
   #:insert-type

   #:value-entry
   #:var-entry
   #:var-entry-ty
   #:fun-entry
   #:fun-entry-formal-types
   #:fun-entry-result-type

   #:get-value
   #:insert-value))

(cl:in-package :cl-tiger/env)

;; A map from symbol:sym to types:ty
(defvar *base-type-env*
  (let ((env (cl-ds.hamt:make-functional-hamt-dictionary #'sxhash #'eq)))
    (reduce (lambda (env binding)
              (cl-ds:insert env (first binding) (second binding)))
            (list (list (symbol:get-sym "int")
                        types:int-ty)
                  (list (symbol:get-sym "string")
                        types:string-ty))
            :initial-value env)))

(defvar *unnamed-base-type-env*
  (let ((env (cl-ds.hamt:make-functional-hamt-dictionary #'sxhash #'eq)))
    (reduce (lambda (env binding)
              (cl-ds:insert env (first binding) (second binding)))
            (list (list (symbol:get-sym "nil")
                        types:nil-ty)
                  (list (symbol:get-sym "unit")
                        types:unit-ty))
            :initial-value env)))

(defun get-type (type-env sym)
  (cl-ds:at type-env sym))

(defun get-unnamed-base-type (sym)
  (cl-ds:at *unnamed-base-type-env* sym))

(defun insert-type (type-env sym ty)
  (cl-ds:insert type-env sym ty))

(serapeum:defunion value-entry
  (var-entry
   (ty types:ty))
  (fun-entry
   ;; A list of types:ty
   (formal-types list)
   (result-type types:ty)))

;; A map from symbol:sym to value-entry
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
                        (fun-entry
                         (list string-ty) unit-ty))
                  (list (symbol:get-sym "flush")
                        ;; flush()
                        ;; flush the std output buffer.
                        (fun-entry nil unit-ty))
                  (list (symbol:get-sym "getchar")
                        ;; getchar(): string
                        ;; read a character from std input, return
                        ;; empty string on end of file.
                        (fun-entry nil string-ty))
                  (list (symbol:get-sym "ord")
                        ;; ord(s: string): int
                        ;; give ASCII value of first character of s,
                        ;; yield -1 if s is empty string.
                        (fun-entry (list string-ty) int-ty))
                  (list (symbol:get-sym "chr")
                        ;; chr(i: int): string
                        ;; single-character string from ASCII value i,
                        ;; halt program if i out of range.
                        (fun-entry (list int-ty) string-ty))
                  (list (symbol:get-sym "size")
                        ;; size(s: string): int
                        ;; number of characters in s.
                        (fun-entry (list string-ty) int-ty))
                  (list (symbol:get-sym "substring")
                        ;; substring(s: string, first: int, n: int): string
                        ;; substring of string s, starting with
                        ;; character first, n characters long,
                        ;; character are numbered starting at 0.
                        (fun-entry (list string-ty int-ty int-ty) string-ty))
                  (list (symbol:get-sym "concat")
                        ;; concat(s1: string, s2: string): string
                        ;; concatenation of s1 and s2.
                        (fun-entry (list string-ty string-ty) string-ty))
                  (list (symbol:get-sym "not")
                        ;; not(i: int): int
                        ;; return (i = 0)
                        (fun-entry (list int-ty) int-ty))
                  (list (symbol:get-sym "exit")
                        ;; exit(i: int)
                        ;; terminate execution with code i.
                        (fun-entry (list int-ty) unit-ty)))
            :initial-value env)))

(defun get-value (value-env sym)
  (cl-ds:at value-env sym))

(defun insert-value (value-env sym value-entry)
  (cl-ds:insert value-env sym value-entry))
