(cl:defpackage :cl-tiger/normalize
  (:use :cl)
  (:local-nicknames
   (:temp :cl-tiger/temp)
   (:ir :cl-tiger/ir))
  (:export
   #:normalize
   #:split-into-basic-blocks
   #:trace-schedule))

(cl:in-package :cl-tiger/normalize)

(defun commute? (stm expr)
  (trivia:match (list stm expr)
    ((list (ir:expr-stm (ir:int-expr _)) _) t)
    ((list _ (ir:label-expr _)) t)
    ((list _ (ir:int-expr _)) t)
    (_ nil)))

(defun concat-stms (first-stm second-stm)
  (trivia:match (list first-stm second-stm)
    ((list (ir:expr-stm (ir:int-expr _)) second-stm)
     second-stm)
    ((list first-stm (ir:expr-stm (ir:int-expr _)))
     first-stm)
    (_ (ir:compound-stm first-stm second-stm))))

(defun normalize-expr (expr)
  (serapeum:match-of ir:expr expr
    ((or (ir:int-expr _)
         (ir:label-expr _)
         (ir:temp-expr _))
     (normalize-exprs-as-stms-and-expr
      nil
      (lambda (exprs)
        (declare (ignore exprs))
        expr)))
    ((ir:bin-op-expr left op right)
     (normalize-exprs-as-stms-and-expr
      (list left right)
      (lambda (exprs)
        (trivia:let-match1 (list left right) exprs
          (ir:bin-op-expr left op right)))))
    ((ir:mem-expr value)
     (normalize-exprs-as-stms-and-expr
      (list value)
      (lambda (exprs)
        (trivia:let-match1 (list value) exprs
          (ir:mem-expr value)))))
    ((ir:call-expr fun args)
     (normalize-exprs-as-stms-and-expr
      (cons fun args)
      (lambda (exprs)
        (trivia:let-match1 (list* fun args) exprs
          (ir:call-expr fun args)))))
    ((ir:stm-then-expr stm expr)
     (trivia:let-match ((stm-stms (normalize-stm stm))
                        ((list expr-stms expr-expr) (normalize-expr expr)))
       (list (concat-stms stm-stms expr-stms) expr-expr)))))

(defun normalize-stm (stm)
  (trivia:ematch stm
    ((ir:move-stm (ir:temp-expr value) (ir:call-expr fun args))
     (normalize-exprs-as-stms
      (cons fun args)
      (lambda (exprs)
        (trivia:let-match1 (list* fun args) exprs
          (ir:move-stm
           (ir:temp-expr value)
           (ir:call-expr fun args))))))
    ;; Note that for (move temp ...) and (move (mem ...) ...),
    ;; we should not pass down the temp or (mem ...) to
    ;; normalize-exprs-as-stms since these l-values may have no value right now,
    ;; pass them down may product wrong result (reference to uninitialized temporary or memory).
    ;; As for (move (stm-then-expr ...) ...), we fallback it to other cases
    ;; (transform to use a "smaller" expr as the left-expr of a ir:move-stm).
    ((ir:move-stm (ir:temp-expr value) right)
     (normalize-exprs-as-stms
      (list right)
      (lambda (exprs)
        (trivia:let-match1 (list right) exprs
          (ir:move-stm
           (ir:temp-expr value)
           right)))))
    ((ir:move-stm (ir:mem-expr value) right)
     (normalize-exprs-as-stms
      (list value right)
      (lambda (exprs)
        (trivia:let-match1 (list value right) exprs
          (ir:move-stm
           (ir:mem-expr value)
           right)))))
    ((ir:move-stm (ir:stm-then-expr stm expr) right)
     (normalize-stm
      (ir:compound-stm
       stm
       (ir:move-stm
        expr
        right))))
    ((ir:expr-stm (ir:call-expr fun args))
     (normalize-exprs-as-stms
      (cons fun args)
      (lambda (exprs)
        (trivia:let-match1 (list* fun args) exprs
          (ir:expr-stm
           (ir:call-expr fun args))))))
    ((ir:expr-stm value)
     (normalize-exprs-as-stms
      (list value)
      (lambda (exprs)
        (trivia:let-match1 (list value) exprs
          (ir:expr-stm value)))))
    ((ir:jump-stm target possible-labels)
     (normalize-exprs-as-stms
      (list target)
      (lambda (exprs)
        (trivia:let-match1 (list target) exprs
          (ir:jump-stm target possible-labels)))))
    ((ir:cjump-stm left op right true-target false-target)
     (normalize-exprs-as-stms
      (list left right)
      (lambda (exprs)
        (trivia:let-match1 (list left right) exprs
          (ir:cjump-stm left op right true-target false-target)))))
    ((ir:compound-stm first second)
     (concat-stms
      (normalize-stm first)
      (normalize-stm second)))
    ((ir:label-stm name)
     (normalize-exprs-as-stms
      nil
      (lambda (exprs)
        (declare (ignore exprs))
        (ir:label-stm name))))))

(defun normalize-exprs (exprs)
  (trivia:ematch exprs
    ((list* expr exprs)
     (serapeum:match-of ir:expr expr
       ((ir:call-expr _ _)
        (let ((call-result-temp (temp:new-temp "call_result")))
          (normalize-exprs
           (cons
            (ir:stm-then-expr
             (ir:move-stm
              (ir:temp-expr call-result-temp)
              expr)
             (ir:temp-expr call-result-temp))
            exprs))))
       (_
        (trivia:let-match (((list first-stms first-expr) (normalize-expr expr))
                           ((list rest-stms rest-exprs) (normalize-exprs exprs)))
          (cond ((commute? rest-stms first-expr)
                 (list (concat-stms first-stms rest-stms) (cons first-expr rest-exprs)))
                (t
                 (let ((temp (temp:new-temp)))
                   (list
                    (concat-stms
                     (concat-stms
                      first-stms
                      (ir:move-stm
                       (ir:temp-expr temp)
                       first-expr))
                     rest-stms)
                    (cons (ir:temp-expr temp) rest-exprs)))))))))
    (nil (list (ir:expr-stm (ir:int-expr 0)) nil))))

(defun normalize-exprs-as-stms (exprs rebuild-fun)
  (trivia:let-match1 (list stms exprs2) (normalize-exprs exprs)
    (concat-stms stms (funcall rebuild-fun exprs2))))

(defun normalize-exprs-as-stms-and-expr (exprs rebuild-fun)
  (trivia:let-match1 (list stms exprs2) (normalize-exprs exprs)
    (list stms (funcall rebuild-fun exprs2))))

(defun linearize-stm (stm)
  (labels ((rec (first-stm acc-stms)
             (serapeum:match-of ir:stm first-stm
               ((ir:compound-stm first-stm second-stm)
                (rec first-stm (rec second-stm acc-stms)))
               (_
                (cons first-stm acc-stms)))))
    (rec stm nil)))

(defun normalize (stm)
  (linearize-stm (normalize-stm stm)))

(defun split-into-basic-blocks (stms)
  (let ((exit-label (temp:new-label "exit")))
    (labels ((collect-block-stms (stms cur-block acc-blocks)
               (trivia:match stms
                 ((list* stm rest-stms)
                  (trivia:match stm
                    ((or (ir:jump-stm _ _)
                         (ir:cjump-stm _ _ _ _ _))
                     (collect-blocks
                      rest-stms
                      (cons (nreverse (cons stm cur-block))
                            acc-blocks)))
                    ((ir:label-stm name)
                     (collect-block-stms
                      (cons (ir:jump-stm (ir:label-expr name) (list name)) stms)
                      cur-block
                      acc-blocks))
                    (_
                     (collect-block-stms rest-stms (cons stm cur-block) acc-blocks))))
                 (nil
                  (collect-block-stms
                   (list (ir:jump-stm (ir:label-expr exit-label) (list exit-label)))
                   cur-block
                   acc-blocks))))
             (collect-blocks (stms acc-blocks)
               (trivia:match stms
                 ((list* stm rest-stms)
                  (trivia:match stm
                    ((ir:label-stm _)
                     (collect-block-stms rest-stms (list stm) acc-blocks))
                    (_
                     (collect-blocks (cons (ir:label-stm (temp:new-label)) stms)
                                     acc-blocks))))
                 (nil
                  (nreverse acc-blocks)))))
      (list (collect-blocks stms nil) exit-label))))

(defun trace-schedule (blocks exit-label)
  (labels ((schedule-blocks (block-table blocks)
             (trivia:match blocks
               ((list* block rest-blocks)
                (trivia:let-match1 (list* (ir:label-stm name) _) block
                  ;; Note that (fset:@ block-table name) returns a nil value
                  ;; means the block is already scheduled by schedule-block-by-trace,
                  ;; we should skip the block in that case.
                  (if (fset:@ block-table name)
                      (schedule-block-by-trace block-table block rest-blocks)
                      (schedule-blocks block-table rest-blocks))))
               (nil
                nil)))
           (schedule-block-by-trace (block-table cur-block blocks)
             (trivia:let-match1 (list* (ir:label-stm name) _) cur-block
               (let ((block-table (fset:with block-table name nil)))
                 (trivia:ematch (split-last cur-block)
                   ((list front-stms (ir:jump-stm (ir:label-expr name) _))
                    (let ((opt-target-block (fset:@ block-table name)))
                      (cond ((null opt-target-block)
                             ;; The target block have been scheduled,
                             ;; so the current trace is interrupted/finished,
                             ;; we just prepend the statements of cur-block to
                             ;; the rest scheduled result.
                             (nconc cur-block (schedule-blocks block-table blocks)))
                            (t
                             ;; The target block haven't been scheduled,
                             ;; so we can schedule the target block after the cur-block,
                             ;; so the ir:jump-stm become useless and can be eliminated,
                             ;; that's why we only prepend the front-stms rather than
                             ;; prepend the whole cur-block.
                             (nconc front-stms (schedule-block-by-trace block-table opt-target-block blocks))))))
                   ((list front-stms (ir:cjump-stm left op right true-target false-target))
                    ;; 1. Try to schedule the false-target after the cur-block.
                    ;; 2. Try to schedule the true-target after the cur-block and reverse the condition.
                    ;; 3. Otherwise, create a new false-target and schedule it after the cur-block.
                    (let ((opt-true-block (fset:@ block-table true-target))
                          (opt-false-block (fset:@ block-table false-target)))
                      (trivia:match (list opt-true-block opt-false-block)
                        ((list _ (not nil))
                         (nconc cur-block (schedule-block-by-trace block-table opt-false-block blocks)))
                        ((list (not nil) _)
                         (nconc front-stms
                                (list (ir:cjump-stm left (ir:not-rel-op op) right false-target true-target))
                                (schedule-block-by-trace block-table opt-true-block blocks)))
                        (_
                         (let ((new-false-target (temp:new-label "false_target")))
                           (nconc front-stms
                                  (list (ir:cjump-stm left op right true-target new-false-target)
                                        (ir:label-stm new-false-target)
                                        (ir:jump-stm (ir:label-expr false-target) (list false-target)))
                                  (schedule-blocks block-table blocks)))))))
                   ((list _ (ir:jump-stm _))
                    ;; Unknown destination, terminate the current trace.
                    (nconc cur-block (schedule-blocks block-table blocks)))))))
           (split-last (list)
             (trivia:ematch list
               ((list x)
                (list nil x))
               ((list* x xs)
                (trivia:let-match1 (list f l) (split-last xs)
                  (list (cons x f) l))))))
    (nconc
     (schedule-blocks
      (reduce (lambda (acc-block-table block)
                (trivia:let-match1 (list* (ir:label-stm name) _) block
                  (fset:with acc-block-table name block)))
              blocks
              :initial-value
              (fset:empty-map))
      blocks)
     (list (ir:label-stm exit-label)))))
