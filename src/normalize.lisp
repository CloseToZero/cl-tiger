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
    ((list (ir:stm-expr (ir:expr-int _)) _) t)
    ((list _ (ir:expr-label _)) t)
    ((list _ (ir:expr-int _)) t)
    (_ nil)))

(defun concat-stms (first-stm second-stm)
  (trivia:match (list first-stm second-stm)
    ((list (ir:stm-expr (ir:expr-int _)) second-stm)
     second-stm)
    ((list first-stm (ir:stm-expr (ir:expr-int _)))
     first-stm)
    (_ (ir:stm-compound first-stm second-stm))))

(defun normalize-expr (expr)
  (serapeum:match-of ir:expr expr
    ((or (ir:expr-int _)
         (ir:expr-label _)
         (ir:expr-temp _))
     (normalize-exprs-as-stms-and-expr
      nil
      (lambda (exprs)
        (declare (ignore exprs))
        expr)))
    ((ir:expr-bin-op left op right)
     (normalize-exprs-as-stms-and-expr
      (list left right)
      (lambda (exprs)
        (trivia:let-match1 (list left right) exprs
          (ir:expr-bin-op left op right)))))
    ((ir:expr-mem value)
     (normalize-exprs-as-stms-and-expr
      (list value)
      (lambda (exprs)
        (trivia:let-match1 (list value) exprs
          (ir:expr-mem value)))))
    ((ir:expr-call fun args)
     (normalize-exprs-as-stms-and-expr
      (cons fun args)
      (lambda (exprs)
        (trivia:let-match1 (list* fun args) exprs
          (ir:expr-call fun args)))))
    ((ir:expr-stm-then-expr stm expr)
     (trivia:let-match ((stm-stms (normalize-stm stm))
                        ((list expr-stms expr-expr) (normalize-expr expr)))
       (list (concat-stms stm-stms expr-stms) expr-expr)))))

(defun normalize-stm (stm)
  (trivia:ematch stm
    ((ir:stm-move (ir:expr-temp value) (ir:expr-call fun args))
     (normalize-exprs-as-stms
      (cons fun args)
      (lambda (exprs)
        (trivia:let-match1 (list* fun args) exprs
          (ir:stm-move
           (ir:expr-temp value)
           (ir:expr-call fun args))))))
    ;; Note that for (move temp ...) and (move (mem ...) ...),
    ;; we should not pass down the temp or (mem ...) to
    ;; normalize-exprs-as-stms since these l-values may have no value right now,
    ;; pass them down may product wrong result (reference to uninitialized temporary or memory).
    ;; As for (move (expr-stm-then-expr ...) ...), we fallback it to other cases
    ;; (transform to use a "smaller" expr as the left-expr of a ir:stm-move).
    ((ir:stm-move (ir:expr-temp value) right)
     (normalize-exprs-as-stms
      (list right)
      (lambda (exprs)
        (trivia:let-match1 (list right) exprs
          (ir:stm-move
           (ir:expr-temp value)
           right)))))
    ((ir:stm-move (ir:expr-mem value) right)
     (normalize-exprs-as-stms
      (list value right)
      (lambda (exprs)
        (trivia:let-match1 (list value right) exprs
          (ir:stm-move
           (ir:expr-mem value)
           right)))))
    ((ir:stm-move (ir:expr-stm-then-expr stm expr) right)
     (normalize-stm
      (ir:stm-compound
       stm
       (ir:stm-move
        expr
        right))))
    ((ir:stm-expr (ir:expr-call fun args))
     (normalize-exprs-as-stms
      (cons fun args)
      (lambda (exprs)
        (trivia:let-match1 (list* fun args) exprs
          (ir:stm-expr
           (ir:expr-call fun args))))))
    ((ir:stm-expr value)
     (normalize-exprs-as-stms
      (list value)
      (lambda (exprs)
        (trivia:let-match1 (list value) exprs
          (ir:stm-expr value)))))
    ((ir:stm-jump target possible-labels)
     (normalize-exprs-as-stms
      (list target)
      (lambda (exprs)
        (trivia:let-match1 (list target) exprs
          (ir:stm-jump target possible-labels)))))
    ((ir:stm-cjump left op right true-target false-target)
     (normalize-exprs-as-stms
      (list left right)
      (lambda (exprs)
        (trivia:let-match1 (list left right) exprs
          (ir:stm-cjump left op right true-target false-target)))))
    ((ir:stm-compound first second)
     (concat-stms
      (normalize-stm first)
      (normalize-stm second)))
    ((ir:stm-label name)
     (normalize-exprs-as-stms
      nil
      (lambda (exprs)
        (declare (ignore exprs))
        (ir:stm-label name))))))

(defun normalize-exprs (exprs)
  (trivia:ematch exprs
    ((list* expr exprs)
     (serapeum:match-of ir:expr expr
       ((ir:expr-call _ _)
        (let ((call-result-temp (temp:new-temp "call_result")))
          (normalize-exprs
           (cons
            (ir:expr-stm-then-expr
             (ir:stm-move
              (ir:expr-temp call-result-temp)
              expr)
             (ir:expr-temp call-result-temp))
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
                      (ir:stm-move
                       (ir:expr-temp temp)
                       first-expr))
                     rest-stms)
                    (cons (ir:expr-temp temp) rest-exprs)))))))))
    (nil (list (ir:stm-expr (ir:expr-int 0)) nil))))

(defun normalize-exprs-as-stms (exprs rebuild-fun)
  (trivia:let-match1 (list stms exprs2) (normalize-exprs exprs)
    (concat-stms stms (funcall rebuild-fun exprs2))))

(defun normalize-exprs-as-stms-and-expr (exprs rebuild-fun)
  (trivia:let-match1 (list stms exprs2) (normalize-exprs exprs)
    (list stms (funcall rebuild-fun exprs2))))

(defun linearize-stm (stm)
  (labels ((rec (first-stm acc-stms)
             (serapeum:match-of ir:stm first-stm
               ((ir:stm-compound first-stm second-stm)
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
                    ((or (ir:stm-jump _ _)
                         (ir:stm-cjump _ _ _ _ _))
                     (collect-blocks
                      rest-stms
                      (cons (nreverse (cons stm cur-block))
                            acc-blocks)))
                    ((ir:stm-label name)
                     (collect-block-stms
                      (cons (ir:stm-jump (ir:expr-label name) (list name)) stms)
                      cur-block
                      acc-blocks))
                    (_
                     (collect-block-stms rest-stms (cons stm cur-block) acc-blocks))))
                 (nil
                  (collect-block-stms
                   (list (ir:stm-jump (ir:expr-label exit-label) (list exit-label)))
                   cur-block
                   acc-blocks))))
             (collect-blocks (stms acc-blocks)
               (trivia:match stms
                 ((list* stm rest-stms)
                  (trivia:match stm
                    ((ir:stm-label _)
                     (collect-block-stms rest-stms (list stm) acc-blocks))
                    (_
                     (collect-blocks (cons (ir:stm-label (temp:new-label)) stms)
                                     acc-blocks))))
                 (nil
                  (nreverse acc-blocks)))))
      (list (collect-blocks stms nil) exit-label))))

(defun trace-schedule (blocks exit-label)
  (labels ((schedule-blocks (block-table blocks)
             (trivia:match blocks
               ((list* block rest-blocks)
                (trivia:let-match1 (list* (ir:stm-label name) _) block
                  ;; Note that (fset:@ block-table name) returns a nil value
                  ;; means the block is already scheduled by schedule-block-by-trace,
                  ;; we should skip the block in that case.
                  (if (fset:@ block-table name)
                      (schedule-block-by-trace block-table block rest-blocks)
                      (schedule-blocks block-table rest-blocks))))
               (nil
                nil)))
           (schedule-block-by-trace (block-table cur-block blocks)
             (trivia:let-match1 (list* (ir:stm-label name) _) cur-block
               (let ((block-table (fset:with block-table name nil)))
                 (trivia:ematch (split-last cur-block)
                   ((list front-stms (ir:stm-jump (ir:expr-label name) _))
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
                             ;; so the ir:stm-jump become useless and can be eliminated,
                             ;; that's why we only prepend the front-stms rather than
                             ;; prepend the whole cur-block.
                             (nconc front-stms (schedule-block-by-trace block-table opt-target-block blocks))))))
                   ((list front-stms (ir:stm-cjump left op right true-target false-target))
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
                                (list (ir:stm-cjump left (ir:not-rel-op op) right false-target true-target))
                                (schedule-block-by-trace block-table opt-true-block blocks)))
                        (_
                         (let ((new-false-target (temp:new-label "false_target")))
                           (nconc front-stms
                                  (list (ir:stm-cjump left op right true-target new-false-target)
                                        (ir:stm-label new-false-target)
                                        (ir:stm-jump (ir:expr-label false-target) (list false-target)))
                                  (schedule-blocks block-table blocks)))))))
                   ((list _ (ir:stm-jump _))
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
                (trivia:let-match1 (list* (ir:stm-label name) _) block
                  (fset:with acc-block-table name block)))
              blocks
              :initial-value
              (fset:empty-map))
      blocks)
     (list (ir:stm-label exit-label)))))
