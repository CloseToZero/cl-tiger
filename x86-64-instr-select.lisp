(cl:defpackage :cl-tiger/x86-64-instr-select
  (:use :cl)
  (:local-nicknames
   (:ir :cl-tiger/ir)
   (:target :cl-tiger/target)
   (:temp :cl-tiger/temp)
   (:frame :cl-tiger/frame)
   (:instr :cl-tiger/instr)
   (:instr-select :cl-tiger/instr-select)))

(cl:in-package :cl-tiger/x86-64-instr-select)

;; NOTE: This module is highly coupled with x86-64-frame,
;; use lots of implicit knowledge of x86-64-frame,
;; like how to pass arguments properly.

(serapeum:defunion mem
  (mem-abs-addr
   (addr fixnum))
  (mem-reg
   (reg temp:temp))
  (mem-base-dreg
   (base temp:temp)
   (disp temp:temp))
  (mem-base-disp
   (base temp:temp)
   (disp fixnum))
  (mem-base-index-scale-disp
   (base temp:temp)
   (index temp:temp)
   (scale fixnum)
   (disp fixnum)))

(serapeum:defunion arg
  (mem-arg
   (value mem))
  (temp-arg
   (value temp:temp))
  (label-arg
   (value temp:label))
  (call-reg-arg
   (value temp:temp))
  (int-arg
   (value fixnum)))

(serapeum:defunion op
  op-mov
  op-cmp
  op-add
  op-sub
  op-imul
  op-and
  op-or
  op-xor
  op-shl
  op-shr
  op-sar

  op-jmp
  op-idiv
  op-call
  op-je
  op-jne
  op-jl
  op-jle
  op-jg
  op-jge
  op-jb
  op-jbe
  op-ja
  op-jae)

(serapeum:defunion asm
  (asm-bin-op
   (op op)
   (dst arg)
   (src arg))
  (asm-unary-op
   (op op)
   (arg arg))
  (asm-label
   (name temp:label))
  (asm-get-label-addr
   (dst arg)
   (label temp:label)))

(defvar *instrs* nil)
(defvar *d0* (temp:named-temp "'d0"))
(defvar *s0* (temp:named-temp "'s0"))
(defvar *s1* (temp:named-temp "'s1"))
(defvar *s2* (temp:named-temp "'s2"))
(defvar *j0* (temp:new-named-label "'j0"))
(defvar *j1* (temp:new-named-label "'j1"))
(defvar *cl* (temp:named-temp "cl"))

(defvar *d0-arg* (temp-arg *d0*))
(defvar *s0-arg* (temp-arg *s0*))
(defvar *s1-arg* (temp-arg *s1*))
(defvar *j0-arg* (label-arg *j0*))
(defvar *j1-arg* (label-arg *j1*))
(defvar *cl-arg* (temp-arg *cl*))

(defun emit (instr)
  (setf *instrs* (cons instr *instrs*))
  nil)

(defun rel-op->unary-op (rel-op)
  (serapeum:match-of ir:rel-op rel-op
    (ir:eq-rel-op op-je)
    (ir:neq-rel-op op-jne)
    (ir:lt-rel-op op-jl)
    (ir:le-rel-op op-jle)
    (ir:gt-rel-op op-jg)
    (ir:ge-rel-op op-jge)
    (ir:ult-rel-op op-jb)
    (ir:ule-rel-op op-jbe)
    (ir:ugt-rel-op op-ja)
    (ir:uge-rel-op op-jae)))

(defmethod instr-select:select-instrs% (stm frame target
                                        (target-arch target:arch-x86-64) target-os)
  (let ((*instrs* nil))
    (select-instr-stm stm frame target)
    (nreverse *instrs*)))

(defun select-instr-stm (stm frame target)
  (trivia:ematch stm
    ((or
      (trivia:guard
       (ir:move-stm
        (ir:temp-expr d)
        (ir:mem-expr
         (ir:bin-op-expr
          (ir:temp-expr s0)
          (ir:plus-bin-op)
          (ir:bin-op-expr
           (ir:temp-expr s1)
           (ir:times-bin-op)
           (ir:int-expr c)))))
       (member c '(2 4 8)))
      (trivia:guard
       (ir:move-stm
        (ir:temp-expr d)
        (ir:mem-expr
         (ir:bin-op-expr
          (ir:bin-op-expr
           (ir:temp-expr s1)
           (ir:times-bin-op)
           (ir:int-expr c))
          (ir:plus-bin-op)
          (ir:temp-expr s0))))
       (member c '(2 4 8))))
     (emit
      (instr:op-instr
       (asm->string
        (asm-bin-op
         op-mov
         *d0-arg*
         (mem-arg
          (mem-base-index-scale-disp
           *s0* *s1* c 0)))
        target)
       (list d)
       (list s0 s1)
       instr:not-jump)))
    ((or
      (ir:move-stm
       (ir:temp-expr d)
       (ir:mem-expr
        (ir:bin-op-expr
         (ir:temp-expr s0)
         (trivia:<> (or (ir:plus-bin-op) (ir:minus-bin-op)) op op)
         (ir:int-expr c))))
      (ir:move-stm
       (ir:temp-expr d)
       (ir:mem-expr
        (ir:bin-op-expr
         (ir:int-expr c)
         (trivia:<> (or (ir:plus-bin-op) (ir:minus-bin-op)) op op)
         (ir:temp-expr s0)))))
     (emit
      (instr:op-instr
       (asm->string
        (asm-bin-op
         op-mov
         *d0-arg*
         (mem-arg
          (mem-base-disp
           *s0*
           (trivia:ematch op
             ((ir:plus-bin-op) c)
             ((ir:minus-bin-op) (- c))))))
        target)
       (list d)
       (list s0)
       instr:not-jump)))
    ((ir:move-stm
      (ir:temp-expr d)
      (ir:mem-expr
       (ir:bin-op-expr
        (ir:temp-expr s0)
        (ir:plus-bin-op)
        (ir:temp-expr s1))))
     (emit
      (instr:op-instr
       (asm->string
        (asm-bin-op
         op-mov
         *d0-arg*
         (mem-arg
          (mem-base-dreg *s0* *s1*)))
        target)
       (list d)
       (list s0 s1)
       instr:not-jump)))
    ((ir:move-stm
      (ir:temp-expr d)
      (ir:int-expr c))
     (emit
      (instr:op-instr
       (asm->string
        (asm-bin-op op-mov *d0-arg* (int-arg c))
        target)
       (list d)
       nil
       instr:not-jump)))
    ((ir:move-stm
      (ir:temp-expr d)
      (ir:temp-expr s))
     (emit
      (instr:move-instr
       (asm->string
        (asm-bin-op op-mov *d0-arg* *s0-arg*)
        target)
       d s)))
    ((ir:move-stm
      (ir:temp-expr d)
      right)
     (emit
      (instr:move-instr
       (asm->string
        (asm-bin-op op-mov *d0-arg* *s0-arg*)
        target)
       d
       (select-instr-expr right frame target))))
    ((or
      (trivia:guard
       (ir:move-stm
        (ir:mem-expr
         (ir:bin-op-expr
          (ir:temp-expr s1)
          (ir:plus-bin-op)
          (ir:bin-op-expr
           (ir:temp-expr s2)
           (ir:times-bin-op)
           (ir:int-expr c))))
        (ir:temp-expr s0))
       (member c '(2 4 8)))
      (trivia:guard
       (ir:move-stm
        (ir:mem-expr
         (ir:bin-op-expr
          (ir:bin-op-expr
           (ir:temp-expr s2)
           (ir:times-bin-op)
           (ir:int-expr c))
          (ir:plus-bin-op)
          (ir:temp-expr s1)))
        (ir:temp-expr s0))
       (member c '(2 4 8))))
     (emit
      (instr:op-instr
       (asm->string
        (asm-bin-op
         op-mov
         (mem-arg
          (mem-base-index-scale-disp *s1* *s2* c 0))
         *s0-arg*)
        target)
       nil
       (list s0 s1 s2)
       instr:not-jump)))
    ((or
      (trivia:guard
       (ir:move-stm
        (ir:mem-expr
         (ir:bin-op-expr
          (ir:temp-expr s0)
          (ir:plus-bin-op)
          (ir:bin-op-expr
           (ir:temp-expr s1)
           (ir:times-bin-op)
           (ir:int-expr c0))))
        (ir:int-expr c1))
       (member c0 '(2 4 8)))
      (trivia:guard
       (ir:move-stm
        (ir:mem-expr
         (ir:bin-op-expr
          (ir:bin-op-expr
           (ir:temp-expr s1)
           (ir:times-bin-op)
           (ir:int-expr c0))
          (ir:plus-bin-op)
          (ir:temp-expr s0)))
        (ir:int-expr c1))
       (member c0 '(2 4 8))))
     (emit
      (instr:op-instr
       (asm->string
        (asm-bin-op
         op-mov
         (mem-arg
          (mem-base-index-scale-disp *s0* *s1* c0 0))
         (int-arg c1))
        target)
       nil
       (list s0 s1)
       instr:not-jump)))
    ((or
      (ir:move-stm
       (ir:mem-expr
        (ir:bin-op-expr
         (ir:temp-expr s1)
         (trivia:<> (or (ir:plus-bin-op) (ir:minus-bin-op)) op op)
         (ir:int-expr c)))
       (ir:temp-expr s0))
      (ir:move-stm
       (ir:mem-expr
        (ir:bin-op-expr
         (ir:int-expr c)
         (trivia:<> (or (ir:plus-bin-op) (ir:minus-bin-op)) op op)
         (ir:temp-expr s1)))
       (ir:temp-expr s0)))
     (emit
      (instr:op-instr
       (asm->string
        (asm-bin-op
         op-mov
         (mem-arg
          (mem-base-disp
           *s1*
           (trivia:ematch op
             ((ir:plus-bin-op) c)
             ((ir:minus-bin-op) (- c)))))
         *s0-arg*)
        target)
       nil
       (list s0 s1)
       instr:not-jump)))
    ((ir:move-stm
      (ir:mem-expr
       (ir:bin-op-expr
        (ir:temp-expr s0)
        (ir:plus-bin-op)
        (ir:temp-expr s1)))
      (ir:int-expr c))
     (emit
      (instr:op-instr
       (asm->string
        (asm-bin-op
         op-mov
         (mem-arg
          (mem-base-dreg *s0* *s1*))
         (int-arg c))
        target)
       nil
       (list s0 s1)
       instr:not-jump)))
    ((ir:move-stm
      (ir:mem-expr
       (ir:bin-op-expr
        (ir:temp-expr s1)
        (ir:plus-bin-op)
        (ir:temp-expr s2)))
      (ir:temp-expr s0))
     (emit
      (instr:op-instr
       (asm->string
        (asm-bin-op
         op-mov
         (mem-arg
          (mem-base-dreg *s1* *s2*))
         *s0-arg*)
        target)
       nil
       (list s0 s1 s2)
       instr:not-jump)))
    ((ir:move-stm
      (ir:mem-expr
       (ir:temp-expr s))
      (ir:int-expr c))
     (emit
      (instr:op-instr
       (asm->string
        (asm-bin-op
         op-mov
         (mem-arg (mem-reg *s0*))
         (int-arg c))
        target)
       nil
       (list s)
       instr:not-jump)))
    ((ir:move-stm
      (ir:mem-expr
       (ir:temp-expr s1))
      (ir:temp-expr s0))
     (emit
      (instr:op-instr
       (asm->string
        (asm-bin-op
         op-mov
         (mem-arg (mem-reg *s1*))
         *s0-arg*)
        target)
       nil
       (list s0 s1)
       instr:not-jump)))
    ((ir:move-stm
      (ir:mem-expr mem)
      right)
     (emit
      (instr:op-instr
       (asm->string
        (asm-bin-op
         op-mov
         (mem-arg (mem-reg *s0*))
         *s1-arg*)
        target)
       nil
       (list (select-instr-expr mem frame target)
             (select-instr-expr right frame target))
       instr:not-jump)))
    ((ir:expr-stm expr)
     (select-instr-expr expr frame target)
     nil)
    ((ir:jump-stm
      (ir:label-expr label)
      _)
     (emit
      (instr:op-instr
       (asm->string
        (asm-unary-op op-jmp *j0-arg*)
        target)
       nil
       nil
       (instr:is-jump (list label)))))
    ((ir:jump-stm expr possible-labels)
     (emit
      (instr:op-instr
       (asm->string
        (asm-unary-op op-jmp *s0-arg*)
        target)
       nil
       (list (select-instr-expr expr frame target))
       (instr:is-jump possible-labels))))
    ((ir:cjump-stm left op right true-target false-target)
     (emit
      (instr:op-instr
       (format nil "~A~%~A~%~A"
               (asm->string
                (asm-bin-op op-cmp *s0-arg* *s1-arg*)
                target)
               (asm->string
                (asm-unary-op
                 (rel-op->unary-op op)
                 *j0-arg*)
                target)
               (asm->string
                (asm-unary-op op-jmp *j1-arg*)
                target))
       nil
       (list (select-instr-expr left frame target)
             (select-instr-expr right frame target))
       (instr:is-jump (list true-target false-target)))))
    ((ir:compound-stm first second)
     (select-instr-stm first frame target)
     (select-instr-stm second frame target))
    ((ir:label-stm name)
     (emit
      (instr:label-instr
       (asm->string (asm-label name) target)
       name)))))

(defun select-instr-expr (expr frame target)
  (trivia:ematch expr
    ((ir:int-expr value)
     (let ((r (temp:new-temp)))
       (emit
        (instr:op-instr
         (asm->string
          (asm-bin-op op-mov *d0-arg* (int-arg value))
          target)
         (list r) nil instr:not-jump))
       r))
    ((ir:label-expr label)
     (let ((r (temp:new-temp)))
       (emit
        (instr:op-instr
         (asm->string
          (asm-get-label-addr *d0-arg* label)
          target)
         (list r)
         nil
         instr:not-jump))
       r))
    ((ir:temp-expr temp)
     temp)
    ((ir:bin-op-expr left (ir:div-bin-op) right)
     (let ((r (temp:new-temp)))
       (emit
        ;; Clear rdx.
        (instr:op-instr
         (asm->string
          (asm-bin-op op-mov *d0-arg* (int-arg 0))
          target)
         (list (temp:named-temp "rdx"))
         nil
         instr:not-jump))
       (emit
        ;; We select instrs for right later, shorten the live range.
        (instr:move-instr
         (asm->string
          (asm-bin-op op-mov *d0-arg* *s0-arg*)
          target)
         (temp:named-temp "rax")
         (select-instr-expr left frame target)))
       (emit
        (instr:op-instr
         (asm->string
          (asm-unary-op op-idiv *s0-arg*)
          target)
         (list (temp:named-temp "rax")
               (temp:named-temp "rdx"))
         (list (select-instr-expr right frame target)
               (temp:named-temp "rax")
               (temp:named-temp "rdx"))
         instr:not-jump))
       (emit
        (instr:move-instr
         (asm->string
          (asm-bin-op op-mov *d0-arg* *s0-arg*)
          target)
         r (temp:named-temp "rax")))
       r))
    ((ir:bin-op-expr left
                     (trivia:<>
                      (or (ir:plus-bin-op)
                          (ir:minus-bin-op)
                          (ir:times-bin-op)
                          (ir:and-bin-op)
                          (ir:or-bin-op)
                          (ir:xor-bin-op))
                      op
                      op)
                     right)
     (let ((r (temp:new-temp)))
       (emit
        (instr:move-instr
         (asm->string
          (asm-bin-op op-mov *d0-arg* *s0-arg*)
          target)
         r
         (select-instr-expr left frame target)))
       (emit
        (instr:op-instr
         (asm->string
          (asm-bin-op
           (trivia:ematch op
             ((ir:plus-bin-op) op-add)
             ((ir:minus-bin-op) op-sub)
             ((ir:times-bin-op) op-imul)
             ((ir:and-bin-op) op-and)
             ((ir:or-bin-op) op-or)
             ((ir:xor-bin-op) op-xor))
           *d0-arg*
           *s0-arg*)
          target)
         (list r)
         (list (select-instr-expr right frame target) r)
         instr:not-jump))
       r))
    ((ir:bin-op-expr left
                     (trivia:<>
                      (or (ir:lshift-bin-op)
                          (ir:rshift-bin-op)
                          (ir:arshift-bin-op))
                      op
                      op)
                     (ir:int-expr c))
     (unless (<= 0 c 63)
       (error "Perform operations ~S with ~A bits which is more than 63" op c))
     (let ((r (temp:new-temp)))
       (emit
        (instr:move-instr
         (asm->string
          (asm-bin-op op-mov *d0-arg* *s0-arg*)
          target)
         r
         (select-instr-expr left frame target)))
       (emit
        (instr:op-instr
         (asm->string
          (asm-bin-op
           (trivia:ematch op
             ((ir:lshift-bin-op) op-shl)
             ((ir:rshift-bin-op) op-shr)
             ((ir:arshift-bin-op) op-sar))
           *d0-arg*
           (int-arg c))
          target)
         (list r)
         (list r)
         instr:not-jump))
       r))
    ((ir:bin-op-expr left
                     (trivia:<>
                      (or (ir:lshift-bin-op)
                          (ir:rshift-bin-op)
                          (ir:arshift-bin-op))
                      op
                      op)
                     right)
     (let ((r (temp:new-temp)))
       (emit
        (instr:move-instr
         (asm->string
          (asm-bin-op op-mov *d0-arg* *s0-arg*)
          target)
         r
         (select-instr-expr left frame target)))
       (emit
        (instr:move-instr
         (asm->string
          (asm-bin-op op-mov *d0-arg* *s0-arg*)
          target)
         (temp:named-temp "rcx")
         (select-instr-expr right frame target)))
       (emit
        (instr:op-instr
         (asm->string
          (asm-bin-op
           (trivia:ematch op
             ((ir:lshift-bin-op) op-shl)
             ((ir:rshift-bin-op) op-shr)
             ((ir:arshift-bin-op) op-sar))
           *d0-arg*
           *cl-arg*)
          target)
         (list r)
         (list (temp:named-temp "rcx") r)
         instr:not-jump))
       r))
    ((or
      (trivia:guard
       (ir:mem-expr
        (ir:bin-op-expr
         (ir:temp-expr s0)
         (ir:plus-bin-op)
         (ir:bin-op-expr
          (ir:temp-expr s1)
          (ir:times-bin-op)
          (ir:int-expr c))))
       (member c '(2 4 8)))
      (trivia:guard
       (ir:mem-expr
        (ir:bin-op-expr
         (ir:bin-op-expr
          (ir:temp-expr s1)
          (ir:times-bin-op)
          (ir:int-expr c))
         (ir:plus-bin-op)
         (ir:temp-expr s0)))
       (member c '(2 4 8))))
     (let ((r (temp:new-temp)))
       (emit
        (instr:op-instr
         (asm->string
          (asm-bin-op
           op-mov
           *d0-arg*
           (mem-arg
            (mem-base-index-scale-disp *s0* *s1* c 0)))
          target)
         (list r)
         (list s0 s1)
         instr:not-jump))
       r))
    ((or
      (ir:mem-expr
       (ir:bin-op-expr
        (ir:temp-expr s0)
        (trivia:<> (or (ir:plus-bin-op) (ir:minus-bin-op)) op op)
        (ir:int-expr c)))
      (ir:mem-expr
       (ir:bin-op-expr
        (ir:int-expr c)
        (trivia:<> (or (ir:plus-bin-op) (ir:minus-bin-op)) op op)
        (ir:temp-expr s0))))
     (let ((r (temp:new-temp)))
       (emit
        (instr:op-instr
         (asm->string
          (asm-bin-op
           op-mov
           *d0-arg*
           (mem-arg
            (mem-base-disp
             *s0*
             (trivia:ematch op
               ((ir:plus-bin-op) c)
               ((ir:minus-bin-op) (- c))))))
          target)
         (list r)
         (list s0)
         instr:not-jump))
       r))
    ((ir:mem-expr
      (ir:bin-op-expr
       (ir:temp-expr s0)
       (ir:plus-bin-op)
       (ir:temp-expr s1)))
     (let ((r (temp:new-temp)))
       (emit
        (instr:op-instr
         (asm->string
          (asm-bin-op
           op-mov
           *d0-arg*
           (mem-arg
            (mem-base-dreg *s0* *s1*)))
          target)
         (list r)
         (list s0 s1)
         instr:not-jump))
       r))
    ((ir:mem-expr
      expr)
     (let ((r (temp:new-temp)))
       (emit
        (instr:op-instr
         (asm->string
          (asm-bin-op
           op-mov
           *d0-arg*
           (mem-arg (mem-reg *s0*)))
          target)
         (list r)
         (list (select-instr-expr expr frame target))
         instr:not-jump))
       r))
    ((ir:call-expr fun args)
     (let ((f (temp:new-temp "fun")))
       (emit
        (instr:move-instr
         (asm->string
          (asm-bin-op op-mov *d0-arg* *s0-arg*)
          target)
         f (select-instr-expr fun frame target)))
       (let ((arg-temps (select-instr-args args frame target))
             (r (temp:new-temp)))
         (emit
          (instr:call-instr
           (asm->string
            (asm-unary-op op-call (call-reg-arg *s0*))
            target)
           (frame:caller-saves target)
           (cons f arg-temps)
           (length args)))
         (emit
          (instr:move-instr
           (asm->string
            (asm-bin-op op-mov *d0-arg* *s0-arg*)
            target)
           r
           (frame:rv target)))
         ;; Pass back the rax directly is a bad idea,
         ;; in many places, we assume the value of the temp
         ;; return by select-instr-expr won't be overwritten,
         ;; Pass back the rax directly violate this assumption.
         r)))
    ((ir:stm-then-expr stm expr)
     (select-instr-stm stm frame target)
     (select-instr-expr expr frame target))))

(defun select-instr-args (args frame target)
  ;; The first four arguments are passed in rcx, rdx, r8, r9,
  ;; remaining arguments get pushed on the stack in right to left,
  ;; see https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention for details.
  ;; We will scan the whole function to find the maximum number of arguments M,
  ;; and x86-64-frame will reserve (M - 4) * word-size bytes stack space for remaining arguments,
  ;; So in here, we should use "mov [rbp + offset], arg-temp" to "push" a argument,
  ;; rather than use "push arg-temp".
  (let* ((arg-regs (frame:arg-regs target))
         (arg-regs-len (length arg-regs))
         (word-size (frame:word-size target))
         (args-len (length args))
         (arg-temps (loop for arg in args
                          collect (let ((r (temp:new-temp)))
                                    (emit
                                     (instr:move-instr
                                      (asm->string
                                       (asm-bin-op op-mov *d0-arg* *s0-arg*)
                                       target)
                                      r (select-instr-expr arg frame target)))
                                    r))))
    (loop with result = nil
          for i from (- args-len 1) downto 0
          ;; iterate in reverse arg order, so we can push the arg in reversed order.
          for arg in (reverse args)
          do (cond ((< i arg-regs-len)
                    (emit
                     (instr:move-instr
                      (asm->string
                       (asm-bin-op op-mov *d0-arg* *s0-arg*)
                       target)
                      (let ((reg (nth i arg-regs)))
                        (push reg result)
                        reg)
                      (nth i arg-temps))))
                   (t
                    (emit
                     (instr:stack-arg-instr
                      ;; We don't know the final frame size yet,
                      ;; cannot calculate the correct offset, so we
                      ;; use a arbitrary number (0) as offset and
                      ;; correct the instr later by reloc-fun.
                      (asm->string
                       (asm-bin-op
                        op-mov
                        (mem-arg
                         (mem-base-disp *s0* 0))
                        *s1-arg*)
                       target)
                      nil
                      (list
                       (frame:fp target)
                       (let ((r (nth i arg-temps)))
                         (push r result)
                         r))
                      (let (
                            ;; Avoid use the modified i.
                            (i i))
                        (lambda (instr frame-size)
                          (trivia:let-match1 (instr:stack-arg-instr _ dsts srcs _) instr
                            (instr:stack-arg-instr
                             (asm->string
                              (asm-bin-op
                               op-mov
                               (mem-arg
                                (mem-base-disp
                                 *s0*
                                 (+ (- frame-size)
                                    (* (serapeum:match-of target:os (target:target-os target)
                                         (target:os-windows
                                          i)
                                         ((or target:os-linux target:os-mac)
                                          ;; System V AMD64 ABI doesn't need home space
                                          (- i arg-regs-len)))
                                       word-size))))
                               *s1-arg*)
                              target)
                             dsts srcs nil))))))))
          finally (return result))))

(defun asm->string (asm target)
  (asm->string% asm target (target:target-arch target) (target:target-os target)))

(defgeneric asm->string% (asm target target-arch target-os))

(defmethod asm->string% (asm target (target-arch target:arch-x86-64) (target-os target:os-windows))
  (serapeum:match-of asm asm
    ((asm-bin-op op dst src)
     (format nil "~A ~A, ~A"
             (masm-op->string op)
             (masm-arg->string dst target)
             (masm-arg->string src target)))
    ((asm-unary-op op arg)
     (format nil "~A ~A"
             (masm-op->string op)
             (masm-arg->string arg target)))
    ((asm-label name)
     (format nil "~A: "
             (frame:label-name name target)))
    ((asm-get-label-addr dst label)
     (format nil "lea ~A, ~A"
             (masm-arg->string dst target)
             (frame:label-name label target)))))

(defun masm-mem->string (mem)
  (serapeum:match-of mem mem
    ((mem-abs-addr addr)
     (format nil "qword ptr [~A]" addr))
    ((mem-reg reg)
     (format nil "qword ptr [~A]" (temp:temp-name reg)))
    ((mem-base-dreg base disp)
     (format nil "qword ptr [~A + ~A]" (temp:temp-name base) (temp:temp-name disp)))
    ((mem-base-disp base disp)
     (format nil "qword ptr [~A + ~A]" (temp:temp-name base) disp))
    ((mem-base-index-scale-disp base index scale disp)
     (format nil "qword ptr [~A + ~A * ~A + ~A]"
             (temp:temp-name base) (temp:temp-name index) scale disp))))

(defun masm-arg->string (arg target)
  (serapeum:match-of arg arg
    ((mem-arg value)
     (masm-mem->string value))
    ((temp-arg value)
     (format nil "~A" (temp:temp-name value)))
    ((label-arg value)
     (format nil "~A" (frame:label-name value target)))
    ((call-reg-arg value)
     (format nil "~A" (temp:temp-name value)))
    ((int-arg value)
     (format nil "~A" value))))

(defun masm-op->string (op)
  (serapeum:match-of op op
    (op-mov "mov")
    (op-cmp "cmp")
    (op-add "add")
    (op-sub "sub")
    (op-imul "imul")
    (op-and "and")
    (op-or "or")
    (op-xor "xor")
    (op-shl "shl")
    (op-shr "shr")
    (op-sar "sar")

    (op-jmp "jmp")
    (op-idiv "idiv")
    (op-call "call")
    (op-je "je")
    (op-jne "jne")
    (op-jl "jl")
    (op-jle "jle")
    (op-jg "jg")
    (op-jge "jge")
    (op-jb "jb")
    (op-jbe "jbe")
    (op-ja "ja")
    (op-jae "jae")))

(defmethod asm->string% (asm target (target-arch target:arch-x86-64) target-os)
  (serapeum:match-of asm asm
    ((asm-bin-op op dst src)
     (format nil "~A ~A, ~A"
             (gas-op->string op)
             (gas-arg->string src target)
             (gas-arg->string dst target)))
    ((asm-unary-op op arg)
     (format nil "~A ~A"
             (gas-op->string op)
             (gas-arg->string arg target)))
    ((asm-label name)
     (format nil "~A: "
             (frame:label-name name target)))
    ((asm-get-label-addr dst label)
     (format nil "lea ~A(%rip), ~A"
             (frame:label-name label target)
             (gas-arg->string dst target)))))

(defun gas-mem->string (mem)
  (serapeum:match-of mem mem
    ((mem-abs-addr addr)
     (format nil "(~A)" addr))
    ((mem-reg reg)
     (format nil "(%~A)" (temp:temp-name reg)))
    ((mem-base-dreg base disp)
     (format nil "~A(%~A)" (temp:temp-name disp) (temp:temp-name base)))
    ((mem-base-disp base disp)
     (format nil "~A(%~A)" disp (temp:temp-name base)))
    ((mem-base-index-scale-disp base index scale disp)
     (format nil "~A(%~A, %~A, ~A)"
             disp (temp:temp-name base) (temp:temp-name index) scale))))

(defun gas-arg->string (arg target)
  (serapeum:match-of arg arg
    ((mem-arg value)
     (gas-mem->string value))
    ((temp-arg value)
     (format nil "%~A" (temp:temp-name value)))
    ((label-arg value)
     (format nil "~A" (frame:label-name value target)))
    ((call-reg-arg value)
     (format nil "*%~A" (temp:temp-name value)))
    ((int-arg value)
     (format nil "$~A" value))))

(defun gas-op->string (op)
  (serapeum:match-of op op
    (op-mov "movq")
    (op-cmp "cmpq")
    (op-add "addq")
    (op-sub "subq")
    (op-imul "imulq")
    (op-and "andq")
    (op-or "orq")
    (op-xor "xorq")
    (op-shl "shlq")
    (op-shr "shrq")
    (op-sar "sarq")

    (op-jmp "jmp")
    (op-idiv "idivq")
    (op-call "callq")
    (op-je "je")
    (op-jne "jne")
    (op-jl "jl")
    (op-jle "jle")
    (op-jg "jg")
    (op-jge "jge")
    (op-jb "jb")
    (op-jbe "jbe")
    (op-ja "ja")
    (op-jae "jae")))
