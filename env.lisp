(cl:defpackage :cl-tiger/env
  (:use :cl)
  (:local-nicknames
   (:symbol :cl-tiger/symbol)
   (:temp :cl-tiger/temp)
   (:target :cl-tiger/target)
   (:types :cl-tiger/types)
   (:trans :cl-tiger/translate)
   (:cl-ds :cl-data-structures)
   (:cl-ds.hamt :cl-data-structures.dicts.hamt))
  (:export
   #:*base-type-env*
   #:base-value-env

   #:get-type
   #:get-unnamed-base-type
   #:insert-type

   #:value-entry
   #:var-entry
   #:var-entry-ty
   #:var-entry-access
   #:fun-entry
   #:fun-entry-formal-types
   #:fun-entry-result-type
   #:fun-entry-label
   #:fun-entry-level

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
   (ty types:ty)
   (access trans:access))
  (fun-entry
   ;; A list of types:ty
   (formal-types list)
   (result-type types:ty)
   (label temp:label)
   (level trans:level)))

;; Returns a map from symbol:sym to value-entry
(defun base-value-env (target)
  (let ((env (cl-ds.hamt:make-functional-hamt-dictionary #'sxhash #'eq))
        (unit-ty (get-unnamed-base-type (symbol:get-sym "unit")))
        (int-ty (get-type *base-type-env* (symbol:get-sym "int")))
        (string-ty (get-type *base-type-env* (symbol:get-sym "string"))))
    (reduce (lambda (env binding)
              (trivia:let-match1 (list name formal-types result-type) binding
                (let ((level (trans:new-level trans:top-level
                                              (temp:new-named-label name)
                                              (mapcar (lambda (_) nil) formal-types)
                                              target)))
                  (cl-ds:insert env
                                (symbol:get-sym name)
                                (fun-entry formal-types
                                           result-type
                                           (temp:new-named-label name)
                                           level)))))
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
                        (list int-ty) unit-ty))
            :initial-value env)))

(defun get-value (value-env sym)
  (cl-ds:at value-env sym))

(defun insert-value (value-env sym value-entry)
  (cl-ds:insert value-env sym value-entry))
