(cl:defpackage :cl-tiger/x86-64-frame
  (:use :cl)
  (:local-nicknames
   (:utils :cl-tiger/utils)
   (:target :cl-tiger/target)
   (:temp :cl-tiger/temp)
   (:ir :cl-tiger/ir)
   (:frame :cl-tiger/frame)
   (:asm :cl-tiger/asm)))

(cl:in-package :cl-tiger/x86-64-frame)

(defvar *word-size* 8)
(defvar *fp* (temp:named-temp "rbp"))
(defvar *rv* (temp:named-temp "rax"))
(progn
  (defvar *arg-regs*
    (mapcar #'temp:named-temp
            (list "rcx"
                  "rdx"
                  "r8"
                  "r9")))
  (defvar *num-of-arg-regs*
    (length *arg-regs*)))
(defvar *caller-saves*
  (mapcar #'temp:named-temp
          (list "rax"
                "rcx"
                "rdx"
                "r8"
                "r9"
                "r10"
                "r11")))
(defvar *callee-saves*
  (mapcar #'temp:named-temp
          (list "rbx"
                "rdi"
                "rsi"
                "r12"
                "r13"
                "r14"
                "r15")))
(defvar *regs*
  (mapcar #'temp:named-temp
          (list "rax"
                "rbx"
                "rcx"
                "rdx"
                "rsi"
                "rdi"
                "rsp"
                "rbp"
                "r8"
                "r9"
                "r10"
                "r11"
                "r12"
                "r13"
                "r14"
                "r15")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass frame (frame:frame)
    ((next-offset
      :type fixnum
      :initform (- *word-size*)
      :accessor frame-next-offset)
     (size
      :type fixnum
      :initform 0
      :accessor frame-size)))

  (defclass access-in-frame (frame:access)
    ((offset
      :type fixnum
      :initarg :offset
      :reader access-in-frame-offset)))

  (defclass access-in-reg (frame:access)
    ((reg
      :type temp:temp
      :initarg :reg
      :reader access-in-reg-reg))))

(defun access-in-frame (offset)
  (make-instance 'access-in-frame :offset offset))

(defun access-in-reg (reg)
  (make-instance 'access-in-reg :reg reg))

(defun alloc-formal (frame escape target)
  (declare (ignore target))
  (if escape
      (let ((offset (frame-next-offset frame)))
        (decf (frame-next-offset frame) *word-size*)
        (incf (frame-size frame) *word-size*)
        (access-in-frame offset))
      (access-in-reg (temp:new-temp))))

(defun alloc-local (frame escape target)
  (declare (ignore target))
  (if escape
      (let ((offset (frame-next-offset frame)))
        (decf (frame-next-offset frame) *word-size*)
        (incf (frame-size frame) *word-size*)
        (access-in-frame offset))
      (access-in-reg (temp:new-temp))))

(defmethod frame:new-frame% (name formals target
                             (target-arch target:arch-x86-64) (target-os target:os-windows))
  (let ((frame (make-instance 'frame :name name :target target)))
    (setf (frame:frame-formals% frame)
          (mapcar (lambda (escape)
                    (alloc-formal frame escape target))
                  formals))
    frame))

(defmethod frame:alloc-local% (frame escape target
                               (target-arch target:arch-x86-64) (target-os target:os-windows))
  (alloc-local frame escape target))

(defmethod frame:word-size% (target
                             (target-arch target:arch-x86-64) (target-os target:os-windows))
  *word-size*)

(defmethod frame:fp% (target
                      (target-arch target:arch-x86-64) (target-os target:os-windows))
  *fp*)

(defmethod frame:rv% (target
                      (target-arch target:arch-x86-64) (target-os target:os-windows))
  *rv*)

(defmethod frame:arg-regs% (target
                            (target-arch target:arch-x86-64) (target-os target:os-windows))
  *arg-regs*)

(defmethod frame:caller-saves% (target
                                (target-arch target:arch-x86-64) (target-os target:os-windows))
  *caller-saves*)

(defmethod frame:callee-saves% (target
                                (target-arch target:arch-x86-64) (target-os target:os-windows))
  *callee-saves*)

(defmethod frame:regs% (target
                        (target-arch target:arch-x86-64) (target-os target:os-windows))
  *regs*)

(defmethod frame:access-expr% (access fp-expr target
                               (target-arch target:arch-x86-64) (target-os target:os-windows))
  (trivia:ematch access
    ((access-in-frame offset)
     (ir:mem-expr
      (ir:bin-op-expr
       fp-expr
       ir:plus-bin-op
       (ir:int-expr offset))))
    ((access-in-reg reg)
     (ir:temp-expr reg))))

(defmethod frame:external-call% (name args target
                                 (target-arch target:arch-x86-64) (target-os target:os-windows))
  (ir:call-expr
   (ir:label-expr
    (temp:new-named-label
     (frame:external-call-label-name name target)))
   args))

(defmethod frame:external-call-label-name% (name target
                                            (target-arch target:arch-x86-64) (target-os target:os-windows))
  (concatenate 'string "tiger_" name))

(defmethod frame:wrap-ir-entry-exit% (frame body-stm target
                                      (target-arch target:arch-x86-64) (target-os target:os-windows))
  (let ((saved-reg-temp-accesses
          (mapcar (lambda (reg)
                    (declare (ignore reg))
                    (frame:alloc-local frame nil target))
                  *callee-saves*)))
    (ir:compound-stm
     (apply
      #'ir:stms->compound-stm
      (loop for reg in *callee-saves*
            for reg-temp-access in saved-reg-temp-accesses
            collect (ir:move-stm
                     (frame:access-expr reg-temp-access (ir:temp-expr *fp*) target)
                     (ir:temp-expr reg))))
     (ir:compound-stm
      (apply
       #'ir:stms->compound-stm
       (loop for i from 0
             for formal-access in (frame:frame-formals frame)
             collect (cond ((< i (length *arg-regs*))
                            (ir:move-stm
                             (frame:access-expr formal-access (ir:temp-expr *fp*) target)
                             (ir:temp-expr (nth i *arg-regs*))))
                           (t
                            (ir:move-stm
                             (frame:access-expr formal-access (ir:temp-expr *fp*) target)
                             (frame:access-expr
                              (access-in-frame
                               (+ (* i *word-size*)
                                  ;; 2 * *word-size* for saved static link and return address
                                  (* 2 *word-size*)))
                              (ir:temp-expr *fp*)
                              target))))))
      (ir:compound-stm
       body-stm
       (apply
        #'ir:stms->compound-stm
        (loop for reg in *callee-saves*
              for reg-temp-access in saved-reg-temp-accesses
              collect (ir:move-stm
                       (ir:temp-expr reg)
                       (frame:access-expr reg-temp-access (ir:temp-expr *fp*) target)))))))))

(defmethod frame:preserve-live-out% (frame body-instrs target
                                    (target-arch target:arch-x86-64) (target-os target:os-windows))
  (append
   body-instrs
   (list
    (asm:op-instr
     ";; A fake instruction used to preserve live-out temporaries."
     nil
     (append (list *rv* (temp:named-temp "rsp")) *callee-saves*)
     (asm:is-jump nil)))))

(defmethod frame:wrap-entry-exit% (frame body-instrs target
                                   (target-arch target:arch-x86-64) (target-os target:os-windows))
  (let* ((max-num-of-call-args (loop for instr in body-instrs
                                     maximize (trivia:if-match (asm:call-instr _ _ _ num-of-args) instr
                                                num-of-args
                                                0)))
         (total-frame-size (+ (frame-size frame)
                              ;; Additional 32 bytes for home space
                              ;; required by x64 calling convention,
                              ;; see https://devblogs.microsoft.com/oldnewthing/20160623-00/?p=93735 for details.
                              (* *num-of-arg-regs* *word-size*)
                              ;; Preallocate space for arguments passing,
                              ;; see x86-64-instr-select::select-instr-args for details.
                              (if (<= max-num-of-call-args *num-of-arg-regs*)
                                  0
                                  (* (- max-num-of-call-args *num-of-arg-regs*)
                                     *word-size*)))
                           ;; The above calculation can be simplify to
                           ;; (+ frame-size (* (max max-num-of-call-args *num-of-arg-regs*) word-size*)),
                           ;; but I prefer clarity and readability.
                           ))
    (list
     (list
      (format nil "~A:" (temp:label-name (frame:frame-name frame)))
      "push rbp"
      "mov rbp, rsp"
      (format nil "sub rsp, ~A" total-frame-size))
     (mapcar
      (lambda (instr)
        (trivia:if-match (asm:stack-arg-instr _ _ _ reloc-fun) instr
          (funcall reloc-fun instr total-frame-size)
          instr))
      body-instrs)
     (list
      (format nil "add rsp, ~A" total-frame-size)
      "pop rbp"
      "ret"))))

(defmethod frame:frag-str->definition% (frag-str string-literal-as-comment target
                                        (target-arch target:arch-x86-64) (target-os target:os-windows))
  (trivia:let-match1 (frame:frag-str label str) frag-str
    (with-output-to-string (out)
      (format out "~A db " (temp:label-name label))
      (let ((bytes (trivial-utf-8:string-to-utf-8-bytes str :null-terminate t)))
        (loop with first? = t
              for byte across bytes
              do (if first?
                     (setf first? nil)
                     (format out ", "))
                 (format out "~A" byte)))
      (when string-literal-as-comment
        (format out " ; ~S" (utils:str-without-newlines str))))))
