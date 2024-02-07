(cl:defpackage :cl-tiger/x86-64-instr-select
  (:use :cl)
  (:local-nicknames
   (:ir :cl-tiger/ir)
   (:target :cl-tiger/target)
   (:asm :cl-tiger/asm)
   (:instr-select :cl-tiger/instr-select)))

(cl:in-package :cl-tiger/x86-64-instr-select)

(defvar *instrs* nil)

(defun emit (instr)
  (cons instr *instrs*)
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

(defmethod select-instrs% (stm frame target (target-arch target:arch-x86-64) (target-os target:os-windows))
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
       nil)))
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
       nil)))
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
       nil)))
    ((ir:move-stm
      (ir:temp-expr d)
      (ir:int-expr c))
     (emit
      (asm:op-instr
       (format nil "mov 'd0, ~A" c)
       (list d)
       nil
       nil)))
    ((ir:move-stm
      (ir:temp-expr d)
      (ir:temp-expr s))
     (emit
      (asm:op-instr
       "mov 'd0, 's0"
       (list d)
       (list s)
       nil)))
    ((ir:move-stm
      (ir:temp-expr d)
      right)
     (emit
      (asm:op-instr
       "mov 'd0, 's0"
       (list d)
       (list (select-instr-expr right frame target))
       nil)))
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
       nil)))
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
       nil)))
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
       nil)))
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
       nil)))
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
       nil)))
    ((ir:move-stm
      (ir:mem-expr
       (ir:temp-expr s))
      (ir:int-expr c))
     (emit
      (asm:op-instr
       (format nil "mov qword ptr ['s], ~A" c)
       nil
       (list s)
       nil)))
    ((ir:move-stm
      (ir:mem-expr
       (ir:temp-expr s1))
      (ir:temp-expr s0))
     (emit
      (asm:op-instr
       "mov qword ptr ['s1], 's0"
       nil
       (list s0 s1)
       nil)))
    ((ir:move-stm
      (ir:mem-expr mem)
      right)
     (emit
      (asm:op-instr
       "mov qword ptr ['s1], 's0"
       nil
       (list (select-instr-expr mem frame target)
             (select-instr-expr right frame target))
       nil)))
    ((ir:expr-stm expr)
     (select-instr-expr expr frame target)
     nil)
    ((ir:jump-stm
      (ir:label-expr target)
      _)
     (emit
      (asm:op-instr
       "jump 'j0"
       nil
       nil
       (list target))))
    ((ir:jump-stm
      expr
      _)
     (emit
      (asm:op-instr
       "jump 's0"
       nil
       (list (select-instr-expr expr frame target))
       nil)))
    ((ir:cjump-stm left op right true-target false-target)
     (emit
      (asm:op-instr
       (format nil "cmp 's0, 's1~%~A 'j0~%jump 'j1"
               (rel-op->jump-asm op))
       nil
       (list (select-instr-expr left frame target)
             (select-instr-expr right frame target))
       (list true-target false-target))))
    ((ir:compound-stm first second)
     (select-instr-stm first frame target)
     (select-instr-stm second frame target))
    ((ir:label-stm name)
     (emit
      (asm:label-instr
       "'j0: " name)))))

(defun select-instr-expr (expr frame target)
  (declare (ignore expr frame target))
  )
