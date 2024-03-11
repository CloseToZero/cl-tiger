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
  (arg-mem
   (value mem))
  (arg-temp
   (value temp:temp))
  (arg-label
   (value temp:label))
  (arg-call-reg
   (value temp:temp))
  (arg-int
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

(defvar *d0-arg* (arg-temp *d0*))
(defvar *s0-arg* (arg-temp *s0*))
(defvar *s1-arg* (arg-temp *s1*))
(defvar *j0-arg* (arg-label *j0*))
(defvar *j1-arg* (arg-label *j1*))
(defvar *cl-arg* (arg-temp *cl*))

(defun emit (instr)
  (setf *instrs* (cons instr *instrs*))
  nil)

(defun rel-op->unary-op (rel-op)
  (serapeum:match-of ir:rel-op rel-op
    (ir:rel-op-eq op-je)
    (ir:rel-op-neq op-jne)
    (ir:rel-op-lt op-jl)
    (ir:rel-op-le op-jle)
    (ir:rel-op-gt op-jg)
    (ir:rel-op-ge op-jge)
    (ir:rel-op-ult op-jb)
    (ir:rel-op-ule op-jbe)
    (ir:rel-op-ugt op-ja)
    (ir:rel-op-uge op-jae)))

(defmethod instr-select:select-instrs% (stm frame target
                                        (target-arch target:arch-x86-64) target-os)
  (let ((*instrs* nil))
    (select-instr-stm stm frame target)
    (nreverse *instrs*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun combine-patterns (patterns)
    (if (rest patterns)
        `(or ,@patterns)
        (first patterns)))

  (defun commute-patterns-expr (expr-pattern)
    (trivia:match expr-pattern
      ((or (list 'ir:expr-int _)
           (list 'ir:expr-label _)
           (list 'ir:expr-temp _))
       (list expr-pattern))
      ((list 'ir:expr-bin-op left-pattern op right-pattern)
       (let ((new-left-pattern (combine-commute-patterns-expr left-pattern))
             (new-right-pattern (combine-commute-patterns-expr right-pattern)))
         (list (list 'ir:expr-bin-op new-left-pattern op new-right-pattern)
               (list 'ir:expr-bin-op new-right-pattern op new-left-pattern))))
      ((list 'ir:expr-mem value-pattern)
       (list (list 'ir:expr-mem (combine-commute-patterns-expr value-pattern))))
      ((list 'ir:expr-call fun-pattern arg-patterns)
       (list (list 'ir:expr-call
                   (combine-commute-patterns-expr fun-pattern)
                   (mapcan (lambda (arg-pattern)
                             (combine-commute-patterns-expr arg-pattern))
                           arg-patterns))))
      ((list 'ir:expr-stm-then-expr stm-pattern expr-pattern)
       (list (list 'ir:expr-stm-then-expr
                   (combine-commute-patterns-stm stm-pattern)
                   (combine-commute-patterns-expr expr-pattern))))
      ((list 'trivia:guard expr-pattern test-form)
       ;; We simply assume the pattern is expr-pattern.
       (list (list 'trivia:guard
                   (combine-commute-patterns-expr expr-pattern)
                   test-form)))
      ;; Don't dive into other patterns right now.
      (_ (list expr-pattern))))

  (defun combine-commute-patterns-expr (expr-pattern)
    (combine-patterns (commute-patterns-expr expr-pattern)))

  (defun combine-commute-patterns-stm (pattern-stm)
    (trivia:match pattern-stm
      ((list 'ir:stm-move left-pattern right-pattern)
       (list 'ir:stm-move
             (combine-commute-patterns-expr left-pattern)
             (combine-commute-patterns-expr right-pattern)))
      ((list 'ir:stm-expr value-pattern)
       (list 'ir:stm-expr (combine-commute-patterns-expr value-pattern)))
      ((list 'ir:stm-jump target-pattern possible-labels)
       (list 'ir:stm-jump (combine-commute-patterns-expr target-pattern)
             possible-labels))
      ((list 'ir:stm-cjump left-pattern op right-pattern true-target false-target)
       (list 'ir:stm-cjump
             (combine-commute-patterns-expr left-pattern)
             op
             (combine-commute-patterns-expr right-pattern)
             true-target false-target))
      ((list 'ir:stm-compound first-pattern second-pattern)
       (list 'ir:stm-compound
             (combine-commute-patterns-stm first-pattern)
             (combine-commute-patterns-stm second-pattern)))
      ((list 'ir:stm-label _)
       pattern-stm)
      ((list 'trivia:guard stm-pattern test-form)
       ;; We simply assume the pattern is stm-pattern.
       (list 'trivia:guard
             (combine-commute-patterns-stm stm-pattern)
             test-form))
      ;; Don't dive into other patterns right now.
      (_ pattern-stm)))

  (trivia:defpattern commute-patterns-expr! (expr-pattern)
    (combine-commute-patterns-expr expr-pattern))

  (trivia:defpattern commute-patterns-stm! (stm-pattern)
    (combine-commute-patterns-stm stm-pattern)))

(defun select-instr-stm (stm frame target)
  (trivia:ematch stm
    ((commute-patterns-stm!
      (trivia:guard
       (ir:stm-move
        (ir:expr-temp d)
        (ir:expr-mem
         (ir:expr-bin-op
          (ir:expr-bin-op
           (ir:expr-temp s0)
           (ir:bin-op-plus)
           (ir:expr-bin-op
            (ir:expr-temp s1)
            (ir:bin-op-times)
            (ir:expr-int c)))
          (ir:bin-op-plus)
          (ir:expr-int o))))
       (member c '(2 4 8))))
     (emit
      (instr:instr-op
       (asm->string
        (asm-bin-op
         op-mov
         *d0-arg*
         (arg-mem
          (mem-base-index-scale-disp *s0* *s1* c o)))
        target)
       (list d)
       (list s0 s1)
       instr:not-jump)))
    ((commute-patterns-stm!
      (trivia:guard
       (ir:stm-move
        (ir:expr-temp d)
        (ir:expr-mem
         (ir:expr-bin-op
          (ir:expr-temp s0)
          (ir:bin-op-plus)
          (ir:expr-bin-op
           (ir:expr-temp s1)
           (ir:bin-op-times)
           (ir:expr-int c)))))
       (member c '(2 4 8))))
     (emit
      (instr:instr-op
       (asm->string
        (asm-bin-op
         op-mov
         *d0-arg*
         (arg-mem
          (mem-base-index-scale-disp *s0* *s1* c 0)))
        target)
       (list d)
       (list s0 s1)
       instr:not-jump)))
    ((commute-patterns-stm!
      (ir:stm-move
       (ir:expr-temp d)
       (ir:expr-mem
        (ir:expr-bin-op
         (ir:expr-temp s0)
         (trivia:<> (or (ir:bin-op-plus) (ir:bin-op-minus)) op op)
         (ir:expr-int c)))))
     (emit
      (instr:instr-op
       (asm->string
        (asm-bin-op
         op-mov
         *d0-arg*
         (arg-mem
          (mem-base-disp
           *s0*
           (trivia:ematch op
             ((ir:bin-op-plus) c)
             ((ir:bin-op-minus) (- c))))))
        target)
       (list d)
       (list s0)
       instr:not-jump)))
    ((ir:stm-move
      (ir:expr-temp d)
      (ir:expr-mem
       (ir:expr-bin-op
        (ir:expr-temp s0)
        (ir:bin-op-plus)
        (ir:expr-temp s1))))
     (emit
      (instr:instr-op
       (asm->string
        (asm-bin-op
         op-mov
         *d0-arg*
         (arg-mem
          (mem-base-dreg *s0* *s1*)))
        target)
       (list d)
       (list s0 s1)
       instr:not-jump)))
    ((ir:stm-move
      (ir:expr-temp d)
      (ir:expr-int c))
     (emit
      (instr:instr-op
       (asm->string
        (asm-bin-op op-mov *d0-arg* (arg-int c))
        target)
       (list d)
       nil
       instr:not-jump)))
    ((ir:stm-move
      (ir:expr-temp d)
      (ir:expr-temp s))
     (emit
      (instr:instr-move
       (asm->string
        (asm-bin-op op-mov *d0-arg* *s0-arg*)
        target)
       d s)))
    ((ir:stm-move
      (ir:expr-temp d)
      right)
     (emit
      (instr:instr-move
       (asm->string
        (asm-bin-op op-mov *d0-arg* *s0-arg*)
        target)
       d
       (select-instr-expr right frame target))))
    ((commute-patterns-stm!
      (trivia:guard
       (ir:stm-move
        (ir:expr-mem
         (ir:expr-bin-op
          (ir:expr-bin-op
           (ir:expr-temp s1)
           (ir:bin-op-plus)
           (ir:expr-bin-op
            (ir:expr-temp s2)
            (ir:bin-op-times)
            (ir:expr-int c)))
          (ir:bin-op-plus)
          (ir:expr-int o)))
        (ir:expr-temp s0))
       (member c '(2 4 8))))
     (emit
      (instr:instr-op
       (asm->string
        (asm-bin-op
         op-mov
         (arg-mem
          (mem-base-index-scale-disp *s1* *s2* c o))
         *s0-arg*)
        target)
       nil
       (list s0 s1 s2)
       instr:not-jump)))
    ((commute-patterns-stm!
      (trivia:guard
       (ir:stm-move
        (ir:expr-mem
         (ir:expr-bin-op
          (ir:expr-temp s1)
          (ir:bin-op-plus)
          (ir:expr-bin-op
           (ir:expr-temp s2)
           (ir:bin-op-times)
           (ir:expr-int c))))
        (ir:expr-temp s0))
       (member c '(2 4 8))))
     (emit
      (instr:instr-op
       (asm->string
        (asm-bin-op
         op-mov
         (arg-mem
          (mem-base-index-scale-disp *s1* *s2* c 0))
         *s0-arg*)
        target)
       nil
       (list s0 s1 s2)
       instr:not-jump)))
    ((commute-patterns-stm!
      (trivia:guard
       (ir:stm-move
        (ir:expr-mem
         (ir:expr-bin-op
          (ir:expr-bin-op
           (ir:expr-temp s0)
           (ir:bin-op-plus)
           (ir:expr-bin-op
            (ir:expr-temp s1)
            (ir:bin-op-times)
            (ir:expr-int c0)))
          (ir:bin-op-plus)
          (ir:expr-int o)))
        (ir:expr-int c1))
       (member c0 '(2 4 8))))
     (emit
      (instr:instr-op
       (asm->string
        (asm-bin-op
         op-mov
         (arg-mem
          (mem-base-index-scale-disp *s0* *s1* c0 o))
         (arg-int c1))
        target)
       nil
       (list s0 s1)
       instr:not-jump)))
    ((commute-patterns-stm!
      (trivia:guard
       (ir:stm-move
        (ir:expr-mem
         (ir:expr-bin-op
          (ir:expr-temp s0)
          (ir:bin-op-plus)
          (ir:expr-bin-op
           (ir:expr-temp s1)
           (ir:bin-op-times)
           (ir:expr-int c0))))
        (ir:expr-int c1))
       (member c0 '(2 4 8))))
     (emit
      (instr:instr-op
       (asm->string
        (asm-bin-op
         op-mov
         (arg-mem
          (mem-base-index-scale-disp *s0* *s1* c0 0))
         (arg-int c1))
        target)
       nil
       (list s0 s1)
       instr:not-jump)))
    ((commute-patterns-stm!
      (ir:stm-move
       (ir:expr-mem
        (ir:expr-bin-op
         (ir:expr-temp s1)
         (trivia:<> (or (ir:bin-op-plus) (ir:bin-op-minus)) op op)
         (ir:expr-int c)))
       (ir:expr-temp s0)))
     (emit
      (instr:instr-op
       (asm->string
        (asm-bin-op
         op-mov
         (arg-mem
          (mem-base-disp
           *s1*
           (trivia:ematch op
             ((ir:bin-op-plus) c)
             ((ir:bin-op-minus) (- c)))))
         *s0-arg*)
        target)
       nil
       (list s0 s1)
       instr:not-jump)))
    ((ir:stm-move
      (ir:expr-mem
       (ir:expr-bin-op
        (ir:expr-temp s0)
        (ir:bin-op-plus)
        (ir:expr-temp s1)))
      (ir:expr-int c))
     (emit
      (instr:instr-op
       (asm->string
        (asm-bin-op
         op-mov
         (arg-mem
          (mem-base-dreg *s0* *s1*))
         (arg-int c))
        target)
       nil
       (list s0 s1)
       instr:not-jump)))
    ((ir:stm-move
      (ir:expr-mem
       (ir:expr-bin-op
        (ir:expr-temp s1)
        (ir:bin-op-plus)
        (ir:expr-temp s2)))
      (ir:expr-temp s0))
     (emit
      (instr:instr-op
       (asm->string
        (asm-bin-op
         op-mov
         (arg-mem
          (mem-base-dreg *s1* *s2*))
         *s0-arg*)
        target)
       nil
       (list s0 s1 s2)
       instr:not-jump)))
    ((ir:stm-move
      (ir:expr-mem
       (ir:expr-temp s))
      (ir:expr-int c))
     (emit
      (instr:instr-op
       (asm->string
        (asm-bin-op
         op-mov
         (arg-mem (mem-reg *s0*))
         (arg-int c))
        target)
       nil
       (list s)
       instr:not-jump)))
    ((ir:stm-move
      (ir:expr-mem
       (ir:expr-temp s1))
      (ir:expr-temp s0))
     (emit
      (instr:instr-op
       (asm->string
        (asm-bin-op
         op-mov
         (arg-mem (mem-reg *s1*))
         *s0-arg*)
        target)
       nil
       (list s0 s1)
       instr:not-jump)))
    ((ir:stm-move
      (ir:expr-mem mem)
      right)
     (emit
      (instr:instr-op
       (asm->string
        (asm-bin-op
         op-mov
         (arg-mem (mem-reg *s0*))
         *s1-arg*)
        target)
       nil
       (list (select-instr-expr mem frame target)
             (select-instr-expr right frame target))
       instr:not-jump)))
    ((ir:stm-expr expr)
     (select-instr-expr expr frame target)
     nil)
    ((ir:stm-jump
      (ir:expr-label label)
      _)
     (emit
      (instr:instr-op
       (asm->string
        (asm-unary-op op-jmp *j0-arg*)
        target)
       nil
       nil
       (instr:is-jump (list label)))))
    ((ir:stm-jump expr possible-labels)
     (emit
      (instr:instr-op
       (asm->string
        (asm-unary-op op-jmp *s0-arg*)
        target)
       nil
       (list (select-instr-expr expr frame target))
       (instr:is-jump possible-labels))))
    ((ir:stm-cjump left op right true-target false-target)
     (emit
      (instr:instr-op
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
    ((ir:stm-compound first second)
     (select-instr-stm first frame target)
     (select-instr-stm second frame target))
    ((ir:stm-label name)
     (emit
      (instr:instr-label
       (asm->string (asm-label name) target)
       name)))))

(defun select-instr-expr (expr frame target)
  (trivia:ematch expr
    ((ir:expr-int value)
     (let ((r (temp:new-temp)))
       (emit
        (instr:instr-op
         (asm->string
          (asm-bin-op op-mov *d0-arg* (arg-int value))
          target)
         (list r) nil instr:not-jump))
       r))
    ((ir:expr-label label)
     (let ((r (temp:new-temp)))
       (emit
        (instr:instr-op
         (asm->string
          (asm-get-label-addr *d0-arg* label)
          target)
         (list r)
         nil
         instr:not-jump))
       r))
    ((ir:expr-temp temp)
     temp)
    ((ir:expr-bin-op left (ir:bin-op-div) right)
     (let ((r (temp:new-temp)))
       (emit
        ;; Clear rdx.
        (instr:instr-op
         (asm->string
          (asm-bin-op op-mov *d0-arg* (arg-int 0))
          target)
         (list (temp:named-temp "rdx"))
         nil
         instr:not-jump))
       (emit
        ;; We select instrs for right later, shorten the live range.
        (instr:instr-move
         (asm->string
          (asm-bin-op op-mov *d0-arg* *s0-arg*)
          target)
         (temp:named-temp "rax")
         (select-instr-expr left frame target)))
       (emit
        (instr:instr-op
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
        (instr:instr-move
         (asm->string
          (asm-bin-op op-mov *d0-arg* *s0-arg*)
          target)
         r (temp:named-temp "rax")))
       r))
    ((ir:expr-bin-op left
                     (trivia:<>
                      (or (ir:bin-op-plus)
                          (ir:bin-op-minus)
                          (ir:bin-op-times)
                          (ir:bin-op-and)
                          (ir:bin-op-or)
                          (ir:bin-op-xor))
                      op
                      op)
                     right)
     (let ((r (temp:new-temp)))
       (emit
        (instr:instr-move
         (asm->string
          (asm-bin-op op-mov *d0-arg* *s0-arg*)
          target)
         r
         (select-instr-expr left frame target)))
       (emit
        (instr:instr-op
         (asm->string
          (asm-bin-op
           (trivia:ematch op
             ((ir:bin-op-plus) op-add)
             ((ir:bin-op-minus) op-sub)
             ((ir:bin-op-times) op-imul)
             ((ir:bin-op-and) op-and)
             ((ir:bin-op-or) op-or)
             ((ir:bin-op-xor) op-xor))
           *d0-arg*
           *s0-arg*)
          target)
         (list r)
         (list (select-instr-expr right frame target) r)
         instr:not-jump))
       r))
    ((ir:expr-bin-op left
                     (trivia:<>
                      (or (ir:bin-op-lshift)
                          (ir:bin-op-rshift)
                          (ir:bin-op-arshift))
                      op
                      op)
                     (ir:expr-int c))
     (unless (<= 0 c 63)
       (error "Perform operations ~S with ~A bits which is more than 63" op c))
     (let ((r (temp:new-temp)))
       (emit
        (instr:instr-move
         (asm->string
          (asm-bin-op op-mov *d0-arg* *s0-arg*)
          target)
         r
         (select-instr-expr left frame target)))
       (emit
        (instr:instr-op
         (asm->string
          (asm-bin-op
           (trivia:ematch op
             ((ir:bin-op-lshift) op-shl)
             ((ir:bin-op-rshift) op-shr)
             ((ir:bin-op-arshift) op-sar))
           *d0-arg*
           (arg-int c))
          target)
         (list r)
         (list r)
         instr:not-jump))
       r))
    ((ir:expr-bin-op left
                     (trivia:<>
                      (or (ir:bin-op-lshift)
                          (ir:bin-op-rshift)
                          (ir:bin-op-arshift))
                      op
                      op)
                     right)
     (let ((r (temp:new-temp)))
       (emit
        (instr:instr-move
         (asm->string
          (asm-bin-op op-mov *d0-arg* *s0-arg*)
          target)
         r
         (select-instr-expr left frame target)))
       (emit
        (instr:instr-move
         (asm->string
          (asm-bin-op op-mov *d0-arg* *s0-arg*)
          target)
         (temp:named-temp "rcx")
         (select-instr-expr right frame target)))
       (emit
        (instr:instr-op
         (asm->string
          (asm-bin-op
           (trivia:ematch op
             ((ir:bin-op-lshift) op-shl)
             ((ir:bin-op-rshift) op-shr)
             ((ir:bin-op-arshift) op-sar))
           *d0-arg*
           *cl-arg*)
          target)
         (list r)
         (list (temp:named-temp "rcx") r)
         instr:not-jump))
       r))
    ((commute-patterns-expr!
      (trivia:guard
       (ir:expr-mem
        (ir:expr-bin-op
         (ir:expr-bin-op
          (ir:expr-temp s0)
          (ir:bin-op-plus)
          (ir:expr-bin-op
           (ir:expr-temp s1)
           (ir:bin-op-times)
           (ir:expr-int c)))
         (ir:bin-op-plus)
         (ir:expr-int o)))
       (member c '(2 4 8))))
     (let ((r (temp:new-temp)))
       (emit
        (instr:instr-op
         (asm->string
          (asm-bin-op
           op-mov
           *d0-arg*
           (arg-mem
            (mem-base-index-scale-disp *s0* *s1* c o)))
          target)
         (list r)
         (list s0 s1)
         instr:not-jump))
       r))
    ((commute-patterns-expr!
      (trivia:guard
       (ir:expr-mem
        (ir:expr-bin-op
         (ir:expr-temp s0)
         (ir:bin-op-plus)
         (ir:expr-bin-op
          (ir:expr-temp s1)
          (ir:bin-op-times)
          (ir:expr-int c))))
       (member c '(2 4 8))))
     (let ((r (temp:new-temp)))
       (emit
        (instr:instr-op
         (asm->string
          (asm-bin-op
           op-mov
           *d0-arg*
           (arg-mem
            (mem-base-index-scale-disp *s0* *s1* c 0)))
          target)
         (list r)
         (list s0 s1)
         instr:not-jump))
       r))
    ((commute-patterns-expr!
      (ir:expr-mem
       (ir:expr-bin-op
        (ir:expr-temp s0)
        (trivia:<> (or (ir:bin-op-plus) (ir:bin-op-minus)) op op)
        (ir:expr-int c))))
     (let ((r (temp:new-temp)))
       (emit
        (instr:instr-op
         (asm->string
          (asm-bin-op
           op-mov
           *d0-arg*
           (arg-mem
            (mem-base-disp
             *s0*
             (trivia:ematch op
               ((ir:bin-op-plus) c)
               ((ir:bin-op-minus) (- c))))))
          target)
         (list r)
         (list s0)
         instr:not-jump))
       r))
    ((ir:expr-mem
      (ir:expr-bin-op
       (ir:expr-temp s0)
       (ir:bin-op-plus)
       (ir:expr-temp s1)))
     (let ((r (temp:new-temp)))
       (emit
        (instr:instr-op
         (asm->string
          (asm-bin-op
           op-mov
           *d0-arg*
           (arg-mem
            (mem-base-dreg *s0* *s1*)))
          target)
         (list r)
         (list s0 s1)
         instr:not-jump))
       r))
    ((ir:expr-mem
      expr)
     (let ((r (temp:new-temp)))
       (emit
        (instr:instr-op
         (asm->string
          (asm-bin-op
           op-mov
           *d0-arg*
           (arg-mem (mem-reg *s0*)))
          target)
         (list r)
         (list (select-instr-expr expr frame target))
         instr:not-jump))
       r))
    ((ir:expr-call fun args)
     (let ((frame-layout-t (temp:new-temp "frame_layout_t"))
           (frame-layout-data-label (temp:new-label "frame_layout")))
       ;; TODO fill in real frame layout, need liveness information after the call,
       ;; provide a callback here to acheve that.
       (instr-select:alloc-data
        frame-layout-data-label
        (make-array
         1 :element-type '(unsigned-byte 8) :initial-element 0))
       (emit
        (instr:instr-op
         (asm->string
          (asm-get-label-addr *d0-arg* frame-layout-data-label)
          target)
         (list frame-layout-t)
         nil
         instr:not-jump))
       (emit
        (instr:instr-op
         (asm->string
          (asm-bin-op
           op-mov
           (arg-mem
            (mem-base-disp
             *s0*
             (- (frame:word-size target))))
           *s1-arg*)
          target)
         nil
         (list (frame:fp target) frame-layout-t)
         instr:not-jump)))
     (let ((f (temp:new-temp "fun")))
       (emit
        (instr:instr-move
         (asm->string
          (asm-bin-op op-mov *d0-arg* *s0-arg*)
          target)
         f (select-instr-expr fun frame target)))
       (let ((arg-temps (select-instr-args args frame target))
             (r (temp:new-temp)))
         (emit
          (instr:instr-call
           (asm->string
            (asm-unary-op op-call (arg-call-reg *s0*))
            target)
           (frame:caller-saves target)
           (cons f arg-temps)
           (length args)))
         (emit
          (instr:instr-move
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
    ((ir:expr-stm-then-expr stm expr)
     (select-instr-stm stm frame target)
     (select-instr-expr expr frame target))))

(defun select-instr-args (args frame target)
  ;; Note:
  ;; For Microsoft x64 calling convention,
  ;; the first four arguments are passed in rcx, rdx, r8, r9,
  ;; remaining arguments get pushed on the stack in right to left,
  ;; see https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention for details.
  ;; We will scan the whole function to find the maximum number of arguments M,
  ;; and x86-64-frame will reserve (M - 4) * word-size bytes stack space for remaining arguments,
  ;; So in here, we should use "mov [rbp + offset], arg-temp" to "push" a argument,
  ;; rather than use "push arg-temp".
  ;; The case for System V AMD64 ABI is similar,
  ;; see https://en.wikipedia.org/wiki/X86_calling_conventions#System_V_AMD64_ABI.
  (let* ((arg-regs (frame:arg-regs target))
         (arg-regs-len (length arg-regs))
         (word-size (frame:word-size target))
         (args-len (length args))
         (arg-temps (loop for arg in args
                          collect (let ((r (temp:new-temp)))
                                    (emit
                                     (instr:instr-move
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
                     (instr:instr-move
                      (asm->string
                       (asm-bin-op op-mov *d0-arg* *s0-arg*)
                       target)
                      (let ((reg (nth i arg-regs)))
                        (push reg result)
                        reg)
                      (nth i arg-temps))))
                   (t
                    (emit
                     (instr:instr-stack-arg
                      ;; We don't know the final frame size yet,
                      ;; cannot calculate the correct offset, so we
                      ;; use a arbitrary number (0) as offset and
                      ;; correct the instr later by reloc-fun.
                      (asm->string
                       (asm-bin-op
                        op-mov
                        (arg-mem
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
                          (trivia:let-match1 (instr:instr-stack-arg _ dsts srcs _) instr
                            (instr:instr-stack-arg
                             (asm->string
                              (asm-bin-op
                               op-mov
                               (arg-mem
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
    ((arg-mem value)
     (masm-mem->string value))
    ((arg-temp value)
     (format nil "~A" (temp:temp-name value)))
    ((arg-label value)
     (format nil "~A" (frame:label-name value target)))
    ((arg-call-reg value)
     (format nil "~A" (temp:temp-name value)))
    ((arg-int value)
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
    ((arg-mem value)
     (gas-mem->string value))
    ((arg-temp value)
     (format nil "%~A" (temp:temp-name value)))
    ((arg-label value)
     (format nil "~A" (frame:label-name value target)))
    ((arg-call-reg value)
     (format nil "*%~A" (temp:temp-name value)))
    ((arg-int value)
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
