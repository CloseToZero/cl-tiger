(cl:defpackage :cl-tiger/frame
  (:use :cl)
  (:local-nicknames
   (:target :cl-tiger/target)
   (:temp :cl-tiger/temp)
   (:ir :cl-tiger/ir))
  (:export
   #:access
   #:frame
   #:frame-name
   #:frame-formals
   #:new-frame
   #:alloc-local
   #:word-size
   #:fp
   #:rv
   #:arg-regs
   #:caller-saves
   #:callee-saves
   #:regs
   #:access-expr
   #:external-call
   #:external-call-label-name
   #:label-name
   #:wrap-ir-entry-exit
   #:wrap-entry-exit
   #:preserve-live-out
   #:frag-str->definition
   #:frag
   #:frag-str
   #:frag-str-label
   #:frag-str-str
   #:frag-fun
   #:frag-fun-body
   #:frag-fun-frame

   ;; internal functions:
   #:frame-formals%
   #:new-frame%
   #:alloc-local%
   #:word-size%
   #:fp%
   #:rv%
   #:arg-regs%
   #:caller-saves%
   #:callee-saves%
   #:regs%
   #:access-expr%
   #:external-call%
   #:external-call-label-name%
   #:label-name%
   #:wrap-ir-entry-exit%
   #:wrap-entry-exit%
   #:preserve-live-out%
   #:frag-str->definition%))

(cl:in-package :cl-tiger/frame)

(defclass access ()
  ())

(defclass frame ()
  ((name
    :type temp:label
    :initarg :name
    :reader frame-name)
   (formals
    ;; A list of access (not a list of boolean).
    :type list
    :initform nil
    :initarg :formals
    :reader frame-formals)
   (target
    :type target:target
    :initarg :target
    :reader frame-target)))

(defun (setf frame-formals%) (formals frame)
  (with-slots ((frame-formals formals)) frame
    (setf frame-formals formals)))

;; formals: A list of boolean to specify the number of arguments and
;; whether the corresponding argument can only reside in memory (which
;; we call it: escape).
(defun new-frame (name formals target)
  (new-frame% name formals target (target:target-arch target) (target:target-os target)))

(defgeneric new-frame% (name formals target target-arch target-os))

;; Returns an access.
(defun alloc-local (frame escape target)
  (alloc-local% frame escape target (target:target-arch target) (target:target-os target)))

(defgeneric alloc-local% (frame escape target target-arch target-os))

(defun word-size (target)
  (word-size% target (target:target-arch target) (target:target-os target)))

(defgeneric word-size% (target target-arch target-os))

;; Returns an temp:temp
(defun fp (target)
  (fp% target (target:target-arch target) (target:target-os target)))

(defgeneric fp% (target target-arch target-os))

;; Returns an temp:temp
(defun rv (target)
  (rv% target (target:target-arch target) (target:target-os target)))

(defgeneric rv% (target target-arch target-os))

(defun arg-regs (target)
  (arg-regs% target (target:target-arch target) (target:target-os target)))

(defgeneric arg-regs% (target target-arch target-os))

(defun caller-saves (target)
  (caller-saves% target (target:target-arch target) (target:target-os target)))

(defgeneric caller-saves% (target target-arch target-os))

(defun callee-saves (target)
  (callee-saves% target (target:target-arch target) (target:target-os target)))

(defgeneric callee-saves% (target target-arch target-os))

(defun regs (target)
  (regs% target (target:target-arch target) (target:target-os target)))

(defgeneric regs% (target target-arch target-os))

(defun access-expr (access fp-expr target)
  (access-expr% access fp-expr target (target:target-arch target) (target:target-os target)))

(defgeneric access-expr% (access fp-expr target target-arch target-os))

(defun external-call (name args target)
  (external-call% name args target (target:target-arch target) (target:target-os target)))

(defgeneric external-call% (name args target target-arch target-os))

(defun label-name (label target)
  (label-name% label target (target:target-arch target) (target:target-os target)))

(defgeneric label-name% (label target target-arch target-os))

(defun external-call-label-name (name target)
  (external-call-label-name% name target (target:target-arch target) (target:target-os target)))

(defgeneric external-call-label-name% (name target target-arch target-os))

;; Returns an new body-stm
(defun wrap-ir-entry-exit (frame body-stm target)
  (wrap-ir-entry-exit% frame body-stm target (target:target-arch target) (target:target-os target)))

(defgeneric wrap-ir-entry-exit% (frame body-stm target target-arch target-os))

(serapeum:defunion frag
  (frag-str
   (label temp:label)
   (str string))
  (frag-fun
   (body ir:stm)
   (frame frame)))

;; Returns a list of instr:instr
(defun preserve-live-out (frame body-instrs target)
  (preserve-live-out% frame body-instrs target (target:target-arch target) (target:target-os target)))

(defgeneric preserve-live-out% (frame body-instrs target target-arch target-os))

;; Returns a list of the form ((prolog list-of-string) (body-instrs list-of-instr:instr) (epilog list-of-string))
(defun wrap-entry-exit (frame body-instrs target)
  (wrap-entry-exit% frame body-instrs target (target:target-arch target) (target:target-os target)))

(defgeneric wrap-entry-exit% (frame body-instrs target target-arch target-os))

;; Returns the string's defintion (an ASM instruction)
(defun frag-str->definition (frag-str string-literal-as-comment target)
  (frag-str->definition% frag-str string-literal-as-comment
                         target (target:target-arch target) (target:target-os target)))

(defgeneric frag-str->definition% (frag-str string-literal-as-comment target target-arch target-os))
