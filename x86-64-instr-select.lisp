(cl:defpackage :cl-tiger/x86-64-instr-select
  (:use :cl)
  (:local-nicknames
   (:ir :cl-tiger/ir)
   (:target :cl-tiger/target)
   (:temp :cl-tiger/temp)
   (:frame :cl-tiger/frame)
   (:asm :cl-tiger/asm)
   (:instr-select :cl-tiger/instr-select)))

(cl:in-package :cl-tiger/x86-64-instr-select)

;; NOTE: This module is highly coupled with x86-64-frame,
;; use lots of implicit knowledge of x86-64-frame,
;; like how to pass arguments properly.

(defvar *instrs* nil)

(defun emit (instr)
  (setf *instrs* (cons instr *instrs*))
  nil)

(defun rel-op->jump-asm (rel-op)
  (serapeum:match-of ir:rel-op rel-op
    (ir:eq-rel-op "je")
    (ir:neq-rel-op "jne")
    (ir:lt-rel-op "jl")
    (ir:le-rel-op "jle")
    (ir:gt-rel-op "jg")
    (ir:ge-rel-op "jge")
    (ir:ult-rel-op "jb")
    (ir:ule-rel-op "jbe")
    (ir:ugt-rel-op "ja")
    (ir:uge-rel-op "jae")))

(defmethod instr-select:select-instrs% (stm frame target
                                        (target-arch target:arch-x86-64) (target-os target:os-windows))
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
      (asm:op-instr
       (format nil "mov 'd0, qword ptr ['s0 + 's1 * ~A]" c)
       (list d)
       (list s0 s1)
       asm:not-jump)))
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
      (asm:op-instr
       (format nil "mov 'd0, qword ptr ['s0 ~A ~A]"
               (trivia:ematch op
                 ((ir:plus-bin-op) "+")
                 ((ir:minus-bin-op) "-"))
               c)
       (list d)
       (list s0)
       asm:not-jump)))
    ((ir:move-stm
      (ir:temp-expr d)
      (ir:mem-expr
       (ir:bin-op-expr
        (ir:temp-expr s0)
        (ir:plus-bin-op)
        (ir:temp-expr s1))))
     (emit
      (asm:op-instr
       "mov 'd0, qword ptr ['s0 + 's1]"
       (list d)
       (list s0 s1)
       asm:not-jump)))
    ((ir:move-stm
      (ir:temp-expr d)
      (ir:int-expr c))
     (emit
      (asm:op-instr
       (format nil "mov 'd0, ~A" c)
       (list d)
       nil
       asm:not-jump)))
    ((ir:move-stm
      (ir:temp-expr d)
      (ir:temp-expr s))
     (emit
      (asm:move-instr
       "mov 'd0, 's0"
       d s)))
    ((ir:move-stm
      (ir:temp-expr d)
      right)
     (emit
      (asm:move-instr
       "mov 'd0, 's0"
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
      (asm:op-instr
       (format nil "mov qword ptr ['s1 + 's2 * ~A], 's0" c)
       nil
       (list s0 s1 s2)
       asm:not-jump)))
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
      (asm:op-instr
       (format nil "mov qword ptr ['s0 + 's1 * ~A], ~A" c0 c1)
       nil
       (list s0 s1)
       asm:not-jump)))
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
      (asm:op-instr
       (format nil "mov qword ptr ['s1 ~A ~A], 's0"
               (trivia:ematch op
                 ((ir:plus-bin-op) "+")
                 ((ir:minus-bin-op) "-"))
               c)
       nil
       (list s0 s1)
       asm:not-jump)))
    ((ir:move-stm
      (ir:mem-expr
       (ir:bin-op-expr
        (ir:temp-expr s0)
        (ir:plus-bin-op)
        (ir:temp-expr s1)))
      (ir:int-expr c))
     (emit
      (asm:op-instr
       (format nil "mov qword ptr ['s0 + 's1], ~A" c)
       nil
       (list s0 s1)
       asm:not-jump)))
    ((ir:move-stm
      (ir:mem-expr
       (ir:bin-op-expr
        (ir:temp-expr s1)
        (ir:plus-bin-op)
        (ir:temp-expr s2)))
      (ir:temp-expr s0))
     (emit
      (asm:op-instr
       "mov qword ptr ['s1 + 's2], 's0"
       nil
       (list s0 s1 s2)
       asm:not-jump)))
    ((ir:move-stm
      (ir:mem-expr
       (ir:temp-expr s))
      (ir:int-expr c))
     (emit
      (asm:op-instr
       (format nil "mov qword ptr ['s0], ~A" c)
       nil
       (list s)
       asm:not-jump)))
    ((ir:move-stm
      (ir:mem-expr
       (ir:temp-expr s1))
      (ir:temp-expr s0))
     (emit
      (asm:op-instr
       "mov qword ptr ['s1], 's0"
       nil
       (list s0 s1)
       asm:not-jump)))
    ((ir:move-stm
      (ir:mem-expr mem)
      right)
     (emit
      (asm:op-instr
       "mov qword ptr ['s0], 's1"
       nil
       (list (select-instr-expr mem frame target)
             (select-instr-expr right frame target))
       asm:not-jump)))
    ((ir:expr-stm expr)
     (select-instr-expr expr frame target)
     nil)
    ((ir:jump-stm
      (ir:label-expr target)
      _)
     (emit
      (asm:op-instr
       "jmp 'j0"
       nil
       nil
       (asm:is-jump (list target)))))
    ((ir:jump-stm expr possible-labels)
     (emit
      (asm:op-instr
       "jmp 's0"
       nil
       (list (select-instr-expr expr frame target))
       (asm:is-jump possible-labels))))
    ((ir:cjump-stm left op right true-target false-target)
     (emit
      (asm:op-instr
       (format nil "cmp 's0, 's1~%~A 'j0~%jmp 'j1"
               (rel-op->jump-asm op))
       nil
       (list (select-instr-expr left frame target)
             (select-instr-expr right frame target))
       (asm:is-jump (list true-target false-target)))))
    ((ir:compound-stm first second)
     (select-instr-stm first frame target)
     (select-instr-stm second frame target))
    ((ir:label-stm name)
     (emit
      (asm:label-instr
       (format nil "~A: "
               (temp:label-name name))
       name)))))

(defun select-instr-expr (expr frame target)
  (trivia:ematch expr
    ((ir:int-expr value)
     (let ((r (temp:new-temp)))
       (emit
        (asm:op-instr
         (format nil "mov 'd0, ~A" value)
         (list r) nil asm:not-jump))
       r))
    ((ir:label-expr label)
     (let ((r (temp:new-temp)))
       (emit
        (asm:op-instr
         (format nil "lea 'd0, [~A]"
                 (temp:label-name label))
         (list r)
         nil
         asm:not-jump))
       r))
    ((ir:temp-expr temp)
     temp)
    ((ir:bin-op-expr left (ir:div-bin-op) right)
     (let ((r (temp:new-temp)))
       (emit
        ;; Clear rdx.
        (asm:op-instr
         "mov 'd0, 0"
         (list (temp:named-temp "rdx"))
         nil
         asm:not-jump))
       (emit
        ;; We select instrs for right later, shorten the live range.
        (asm:move-instr
         "mov 'd0, 's0"
         (temp:named-temp "rax")
         (select-instr-expr left frame target)))
       (emit
        (asm:op-instr
         "idiv 's0"
         (list (temp:named-temp "rax")
               (temp:named-temp "rdx"))
         (list (select-instr-expr right frame target)
               (temp:named-temp "rax")
               (temp:named-temp "rdx"))
         asm:not-jump))
       (emit
        (asm:move-instr
         "mov 'd0, 's0"
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
        (asm:move-instr
         "mov 'd0, 's0"
         r
         (select-instr-expr left frame target)))
       (emit
        (asm:op-instr
         (format nil "~A 'd0, 's0"
                 (trivia:ematch op
                   ((ir:plus-bin-op) "add")
                   ((ir:minus-bin-op) "sub")
                   ((ir:times-bin-op) "imul")
                   ((ir:and-bin-op) "and")
                   ((ir:or-bin-op) "or")
                   ((ir:xor-bin-op) "xor")))
         (list r)
         (list (select-instr-expr right frame target) r)
         asm:not-jump))
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
        (asm:move-instr
         "mov 'd0, 's0"
         r
         (select-instr-expr left frame target)))
       (emit
        (asm:op-instr
         (format nil "~A 'd0, ~A"
                 (trivia:ematch op
                   ((ir:lshift-bin-op) "shl")
                   ((ir:rshift-bin-op) "shr")
                   ((ir:arshift-bin-op) "sar"))
                 c)
         (list r)
         (list r)
         asm:not-jump))
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
        (asm:move-instr
         "mov 'd0, 's0"
         r
         (select-instr-expr left frame target)))
       (emit
        (asm:move-instr
         "mov 'd0, 's0"
         (temp:named-temp "rcx")
         (select-instr-expr right frame target)))
       (emit
        (asm:op-instr
         (format nil "~A 'd0, cl"
                 (trivia:ematch op
                   ((ir:lshift-bin-op) "shl")
                   ((ir:rshift-bin-op) "shr")
                   ((ir:arshift-bin-op) "sar")))
         (list r)
         (list (temp:named-temp "rcx") r)
         asm:not-jump))
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
        (asm:op-instr
         (format nil "mov 'd0, qword ptr ['s0 + 's1 * ~A]" c)
         (list r)
         (list s0 s1)
         asm:not-jump))
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
        (asm:op-instr
         (format nil "mov 'd0, qword ptr ['s0 ~A ~A]"
                 (trivia:ematch op
                   ((ir:plus-bin-op) "+")
                   ((ir:minus-bin-op) "-"))
                 c)
         (list r)
         (list s0)
         asm:not-jump))
       r))
    ((ir:mem-expr
      (ir:bin-op-expr
       (ir:temp-expr s0)
       (ir:plus-bin-op)
       (ir:temp-expr s1)))
     (let ((r (temp:new-temp)))
       (emit
        (asm:op-instr
         "mov 'd0, qword ptr ['s0 + 's1]"
         (list r)
         (list s0 s1)
         asm:not-jump))
       r))
    ((ir:mem-expr
      expr)
     (let ((r (temp:new-temp)))
       (emit
        (asm:op-instr
         "mov 'd0, qword ptr ['s0]"
         (list r)
         (list (select-instr-expr expr frame target))
         asm:not-jump))
       r))
    ((ir:call-expr fun args)
     (let ((f (temp:new-temp "fun")))
       (emit
        (asm:move-instr
         "mov 'd0, 's0"
         f (select-instr-expr fun frame target)))
       (let ((arg-temps (select-instr-args args frame target))
             (r (temp:new-temp)))
         (emit
          (asm:call-instr
           "call 's0"
           (frame:caller-saves target)
           (cons f arg-temps)
           (length args)))
         (emit
          (asm:move-instr
           "mov 'd0, 's0"
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
                                     (asm:move-instr
                                      "mov 'd0, 's0"
                                      r (select-instr-expr arg frame target)))
                                    r))))
    (loop with result = nil
          for i from (- args-len 1) downto 0
          ;; iterate in reverse arg order, so we can push the arg in reversed order.
          for arg in (reverse args)
          do (cond ((< i arg-regs-len)
                    (emit
                     (asm:move-instr
                      "mov 'd0, 's0"
                      (let ((reg (nth i arg-regs)))
                        (push reg result)
                        reg)
                      (nth i arg-temps))))
                   (t
                    (emit
                     (asm:stack-arg-instr
                      ;; We don't know the final frame size yet,
                      ;; cannot calculate the correct offset, so we
                      ;; use a arbitrary number (0) as offset and
                      ;; correct the instr later by reloc-fun.
                      "mov qword ptr ['s0 + 0], 's1"
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
                          (trivia:let-match1 (asm:stack-arg-instr _ dsts srcs _) instr
                            (asm:stack-arg-instr
                             (format nil "mov qword ptr ['s0 + ~A], 's1"
                                     (+ (- frame-size) (* i word-size)))
                             dsts srcs nil))))))))
          finally (return result))))
