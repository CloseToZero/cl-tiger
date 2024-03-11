(cl:defpackage :cl-tiger/reg-alloc
  (:use :cl)
  (:local-nicknames
   (:symbol :cl-tiger/symbol)
   (:util :cl-tiger/util)
   (:temp :cl-tiger/temp)
   (:frame :cl-tiger/frame)
   (:ir :cl-tiger/ir)
   (:instr :cl-tiger/instr)
   (:graph :cl-tiger/graph)
   (:flow-graph :cl-tiger/flow-graph)
   (:liveness :cl-tiger/liveness)
   (:instr-select :cl-tiger/instr-select))
  (:export
   #:reg-alloc))

(cl:in-package :cl-tiger/reg-alloc)

(defclass temp-statistics ()
  ((num-of-defs
    :type integer
    :initform 0)
   (num-of-uses
    :type integer
    :initform 0)))

(defclass move ()
  ((dst
    :type temp:temp
    :initarg :dst)
   (src
    :type temp:temp
    :initarg :src)))

(defmethod print-object ((move move) stream)
  (print-unreadable-object (move stream :type t :identity t)
    (format stream "dst: ~S, src: ~S"
            (slot-value move 'dst)
            (slot-value move 'src))))

(defmethod fset:compare ((x move) (y move))
  (let ((first-result (fset:compare (slot-value x 'dst) (slot-value y 'dst))))
    (cond ((eql first-result :equal)
           (fset:compare (slot-value x 'src) (slot-value y 'src)))
          (t
           first-result))))

(defun reg-alloc (instrs frame target)
  (reg-alloc% instrs (fset:empty-set) frame target))

(defun reg-alloc% (instrs pre-generated-temps frame target)
  (let* ((regs (frame:regs target))

         (flow-graph (flow-graph:instrs->flow-graph instrs))
         (liveness (liveness:liveness flow-graph))
         (live-out-table (second liveness))

         ;; A map from a temp to its adjacent nodes (a `fset:set') in the interference graph.
         (temp->adjs-table (make-hash-table))
         ;; A map from a temp to its adjacent nodes (a `fset:set') in the interference graph,
         ;; unlike `temp->adjs-table', in this table, the adjacent nodes of a node
         ;; is already filtered (exculde those reduced nodes).
         (temp->filtered-adjs-table (make-hash-table))

         ;; A map from a temp to its statistics (a `temp-statistics'),
         ;; used to calculate spill cost.
         (temp->statistics-table (make-hash-table))

         ;; A map from a temp to move instructions related to it,
         ;; each move instruction is represented by `move'.
         (temp->moves-table (make-hash-table))

         ;; A map from a temp to its alias (which it has been coalesced into).
         ;; Note: we won't remove the entries of coalesced nodes within
         ;; `temp->alias-table' for several reasons:
         ;; 1. If we remove the entries of coalesced nodes, then whenever we coalesce a node,
         ;;    we need to delete entries that alias to the coalesced node, which need full table
         ;;    scanning if we don't maintain additional data structures.
         ;; 2. When we assign colors/registers later, we also need to assign colors/registers
         ;;    for coalesced nodes, keeping the entries of coalesced nodes makes it easy to do so.
         (temp->alias-table (make-hash-table))

         ;; A map from a temp to a boolean indicates whether the temp is precolored.
         (precolored-table (loop with table = (make-hash-table)
                                 for reg in regs
                                 do (setf (gethash reg table) t)
                                 finally (return table)))

         ;; A queue of temps for reduction,
         ;; Note: we don't put any move-related nodes that we still consider coalescing
         ;; into this queue.
         (reduce-queue (queues:make-queue :simple-queue))
         ;; A priority queue of temps for spilling ordered by spill cost,
         ;; Note: we don't put any move-related nodes that we still consider coalescing
         ;; into this queue.
         (spill-queue (queues:make-queue
                       :priority-queue
                       :compare
                       (lambda (temp-1 temp-2)
                         (flet ((temp->spill-cost (temp)
                                  (if (or (gethash temp precolored-table)
                                          (fset:contains? pre-generated-temps temp))
                                      ;; We also avoid spill those temps generated for spilled temps,
                                      ;; since generated temps tend to have less defs and uses.
                                      most-positive-fixnum
                                      (with-slots (num-of-defs num-of-uses) (gethash temp temp->statistics-table)
                                        (/ (+ num-of-defs num-of-uses)
                                           (max (fset:size (gethash temp temp->adjs-table))
                                                0.5))))))
                           (< (temp->spill-cost temp-1)
                              (temp->spill-cost temp-2))))))
         ;; We use the following two tables to mark a item in spill-queue as obsoleted,
         ;; in order to acheve the effect of "remove" a item from spill-queue.
         (spill-queue-exists-table (make-hash-table))
         (spill-queue-obsoleted-table (make-hash-table))

         ;; A queue of moves to be test for coalescing.
         (coalesce-move-queue (queues:make-queue :simple-queue))
         ;; A set of moves that failed to be coalesced,
         ;; but still haven't gave up hope to be coalesced,
         ;; need to reconsider coalescing these moves later
         ;; (when degrees of related nodes changed).
         ;; Note: when we give up hope to coalesce a node (not a move),
         ;; we should remove its related moves from `move-wait-coalesce-again-set',
         ;; and put the node into `reduce-queue' or `spill-queue'
         ;; (because the node is no longer considered move-related).
         (move-wait-coalesce-again-set (fset:set))

         (empty-set (fset:empty-set))

         (num-of-regs (length regs))

         (reduced-stack nil)

         (spilled-temps nil)
         (allocation (make-hash-table))
         (data-frags-list nil))
    (labels ((build ()
               (let ((fgraph (flow-graph:flow-graph-graph flow-graph))
                     (defs-table (flow-graph:flow-graph-defs-table flow-graph))
                     (uses-table (flow-graph:flow-graph-uses-table flow-graph))
                     (is-move-set (flow-graph:flow-graph-is-move-set flow-graph))

                     (move->move-table (fset:empty-map)))
                 (fset:do-set (fnode (graph:graph-nodes fgraph))
                   (unless (flow-graph:is-fake-node flow-graph fnode)
                     (let* ((live-out-set (fset:@ live-out-table fnode))
                            (is-move (fset:contains? is-move-set fnode))
                            (exclude-temp (and is-move (util:set-singleton (fset:@ uses-table fnode)))))
                       (when is-move
                         (let* ((dst (util:set-singleton (fset:@ defs-table fnode)))
                                (src exclude-temp))
                           ;; Avoid coalesce moves of those temps generated for spilled temps,
                           ;; even we will give high spill costs for these generated temps
                           ;; (see the :compare function of spill-queue in the above), but that's no enough,
                           ;; we may still end up with one generated temp as the only spilled temp,
                           ;; and cause algorithm looping.
                           (when (or (fset:contains? pre-generated-temps dst)
                                     (fset:contains? pre-generated-temps src))
                             (let ((move (let ((move (make-instance 'move :dst dst :src src)))
                                           (or (fset:@ move->move-table move)
                                               (progn
                                                 (queues:qpush coalesce-move-queue move)
                                                 (fset:includef move->move-table move move)
                                                 move)))))
                               (fset:includef (gethash dst temp->moves-table empty-set) move)
                               (fset:includef (gethash src temp->moves-table empty-set) move)))))
                       (flet ((temp->statistics (temp)
                                (or (gethash temp temp->statistics-table)
                                    (setf (gethash temp temp->statistics-table)
                                          (make-instance 'temp-statistics))))
                              (ensure-adjs (temp)
                                (unless (gethash temp temp->adjs-table)
                                  (setf (gethash temp temp->adjs-table) empty-set)
                                  (setf (gethash temp temp->filtered-adjs-table) empty-set))))
                         (fset:do-set (def (fset:@ defs-table fnode))
                           (let ((statistics (temp->statistics def)))
                             (incf (slot-value statistics 'num-of-defs)))
                           (fset:do-set (live-out-temp live-out-set)
                             (unless (or (eq live-out-temp def)
                                         (eq live-out-temp exclude-temp))
                               (add-edge def live-out-temp)))
                           (ensure-adjs def))
                         (fset:do-set (use (fset:@ uses-table fnode))
                           (let ((statistics (temp->statistics use)))
                             (incf (slot-value statistics 'num-of-uses)))
                           (ensure-adjs use))))))))

             (add-edge (temp-1 temp-2)
               (fset:includef (gethash temp-1 temp->adjs-table empty-set) temp-2)
               (fset:includef (gethash temp-2 temp->adjs-table empty-set) temp-1)
               (fset:includef (gethash temp-1 temp->filtered-adjs-table empty-set) temp-2)
               (fset:includef (gethash temp-2 temp->filtered-adjs-table empty-set) temp-1))

             (remove-edge (temp-1 temp-2)
               (fset:excludef (gethash temp-1 temp->adjs-table) temp-2)
               (fset:excludef (gethash temp-2 temp->adjs-table) temp-1)
               ;; The temp-1/temp-2 may been coalesced, so it may not within temp->filtered-adjs-table,
               ;; we should test this rather than add back a empty set.
               (when (gethash temp-1 temp->filtered-adjs-table)
                 (fset:excludef (gethash temp-1 temp->filtered-adjs-table) temp-2))
               (when (gethash temp-2 temp->filtered-adjs-table)
                 (fset:excludef (gethash temp-2 temp->filtered-adjs-table)  temp-1)))

             (init-queues ()
               (maphash (lambda (temp value)
                          (declare (ignore value))
                          (try-reduce-or-spill temp))
                        temp->adjs-table))

             (push-spill-queue (item)
               (cond ((and (gethash item spill-queue-exists-table)
                           (gethash item spill-queue-obsoleted-table))
                      ;; Note that in this case, we don't move the item in front of the queue,
                      ;; this is fine for register allocation, but not fine for many other purposes.
                      (remhash item spill-queue-obsoleted-table))
                     ((not (gethash item spill-queue-exists-table))
                      (queues:qpush spill-queue item)
                      (setf (gethash item spill-queue-exists-table) t))))

             (pop-spill-queue (&optional empty)
               (loop (cond ((zerop (queues:qsize spill-queue))
                            (return empty))
                           (t
                            (let ((top (queues:qpop spill-queue empty)))
                              (unless (gethash top spill-queue-obsoleted-table)
                                (remhash top spill-queue-obsoleted-table)
                                ;; Since we don't allow duplicates in spill-queue,
                                ;; there is just one obsoleted item for this top,
                                ;; so it's safe to also clear the top from spill-queue-exists-table.
                                (remhash top spill-queue-exists-table)
                                (return top)))))))

             (spill-queue-empty? ()
               (loop (when (zerop (queues:qsize spill-queue))
                       (return t))
                     (let ((top (queues:qtop spill-queue)))
                       (cond ((gethash top spill-queue-obsoleted-table)
                              (queues:qpop spill-queue)
                              (remhash top spill-queue-obsoleted-table)
                              (remhash top spill-queue-exists-table))
                             (t
                              (return nil))))))

             (mark-spill-queue-item-obsoleted (item &optional (error-if-not-exists t))
               (when (and error-if-not-exists
                          (not (gethash item spill-queue-exists-table)))
                 (error "Item ~S is not in spill-queue." item))
               (setf (gethash item spill-queue-obsoleted-table) t))

             (try-reduce-or-spill (temp)
               (unless (or (move-related? temp) (gethash temp precolored-table))
                 (if (< (degrees temp) num-of-regs)
                     (progn
                       (queues:qpush reduce-queue temp))
                     (push-spill-queue temp))))

             (move-related? (temp)
               (fset:nonempty? (gethash temp temp->moves-table empty-set)))

             (degrees (temp)
               (fset:size (gethash temp temp->filtered-adjs-table)))

             (do-reduce ()
               (loop for i from 1
                     until (zerop (queues:qsize reduce-queue))
                     for temp = (queues:qpop reduce-queue)
                     do (fset:do-set (adj (gethash temp temp->filtered-adjs-table))
                          (fset:excludef (gethash adj temp->filtered-adjs-table) temp))
                        (remhash temp temp->filtered-adjs-table)
                        (push temp reduced-stack)))

             (do-coalesce ()
               (loop for i from 1
                     until (zerop (queues:qsize coalesce-move-queue))
                     for move = (queues:qpop coalesce-move-queue)
                     ;; The nodes of move may already been coalesced,
                     ;; so we need to call `actual-temp'.
                     for temp-1 = (actual-temp (slot-value move 'dst))
                     for temp-2 = (actual-temp (slot-value move 'src))
                     do (when (gethash temp-2 precolored-table)
                          ;; Ensure the temp-1 is precolored if we have one,
                          ;; so that we only merge non-precolored node into precolored,
                          ;; rather than merge precolored node into non-precolored,
                          ;; thus things like precolored-table don't need to be updated.
                          (rotatef temp-1 temp-2))
                        (cond ((eq temp-1 temp-2)
                               (fset:excludef (gethash temp-1 temp->moves-table) move)
                               ;; The temp-1 may no longer be move-related,
                               ;; move it to reduce-queue or spill-queue.
                               (try-reduce-or-spill temp-1))
                              ((connected? temp-1 temp-2)
                               ;; constrained move.
                               ;; impossible to be coalesced.
                               (freeze-move move temp-1 temp-2))
                              ((can-be-coalesced? temp-1 temp-2)
                               (coalesce-move move temp-1 temp-2))
                              (t
                               (fset:includef move-wait-coalesce-again-set move)))))

             (actual-temp (temp)
               (loop with pre-pre-temp = nil
                     with pre-temp = temp
                     for i from 1
                     for next-temp = (gethash pre-temp temp->alias-table)
                     while next-temp
                     do (setf pre-pre-temp pre-temp)
                        (setf pre-temp next-temp)
                     finally
                        (progn
                          ;; compress alias path.
                          (when (and pre-pre-temp (>= i 3))
                            (setf (gethash pre-pre-temp temp->alias-table)
                                  pre-temp))
                          (return pre-temp))))

             (can-be-coalesced? (temp-1 temp-2)
               (and (not (and (gethash temp-1 precolored-table)
                              (gethash temp-2 precolored-table)))
                    (or
                     ;; Briggs testing
                     (< (let ((high-degree-count 0))
                          (flet ((do-count (temp coalesced-temp)
                                   (fset:do-set (adj (gethash temp temp->filtered-adjs-table))
                                     (when (and (not (eq adj coalesced-temp))
                                                (>= (- (degrees adj)
                                                       (if (and (connected? adj temp-1)
                                                                (connected? adj temp-2))
                                                           1
                                                           0))
                                                    num-of-regs))
                                       (incf high-degree-count)))))
                            (do-count temp-1 temp-2)
                            (do-count temp-2 temp-1))
                          high-degree-count)
                        num-of-regs)
                     ;; George testing
                     (block result
                       (flet ((do-test (temp coalesced-temp)
                                (fset:do-set (adj (gethash temp temp->filtered-adjs-table))
                                  (when (not (or (eq adj coalesced-temp)
                                                 (< (degrees adj) num-of-regs)))
                                    (return-from result nil)))))
                         (do-test temp-1 temp-2)
                         (do-test temp-2 temp-1))
                       t))))

             (connected? (temp-1 temp-2)
               (fset:contains? (gethash temp-1 temp->filtered-adjs-table) temp-2))

             (coalesce-move (old-move into-temp from-temp)
               ;; Note that the dst and src of old-move may both not be into-temp
               ;; because of aliasing.
               (setf (gethash from-temp temp->alias-table) into-temp)
               (setf (gethash into-temp temp->moves-table)
                     (fset:union (gethash into-temp temp->moves-table)
                                 (gethash from-temp temp->moves-table)))
               (fset:excludef (gethash into-temp temp->moves-table) old-move)
               (fset:do-set (adj (gethash from-temp temp->filtered-adjs-table))
                 (let ((old-adj-degrees (degrees adj)))
                   (unless (eq adj into-temp)
                     (add-edge adj into-temp))
                   (remove-edge adj from-temp)
                   ;; If adj just transfer from high-degree to low-degree,
                   ;; we should retry the coalescing of related moves,
                   ;; or move it into the reduce queue.
                   (when (and (not (eq adj into-temp))
                              (>= old-adj-degrees num-of-regs)
                              (< (degrees adj) num-of-regs))
                     (cond ((move-related? adj)
                            ;; Briggs testing care about the degree of a node itself,
                            ;; so the related moves of adj should be retried.
                            (retry-coalesce-moves-of-temp adj)
                            ;; George testing care about the degree of adjacent nodes,
                            ;; so the related moves of adjacent nodes of adj should be
                            ;; retried (as adj is one of their adjacent nodes).
                            (fset:do-set (adj-adj (gethash adj temp->filtered-adjs-table))
                              (when (move-related? adj-adj)
                                (retry-coalesce-moves-of-temp adj-adj))))
                           (t
                            (try-move-from-spill-to-reduce adj))))))
               (remhash from-temp temp->filtered-adjs-table)
               (remhash from-temp temp->moves-table)
               ;; into-temp may no longer move-related,
               ;; we should add it to reduce-queue or spill-queue if so.
               (try-reduce-or-spill into-temp))

             (freeze-move (old-move temp-1 temp-2)
               (fset:excludef (gethash temp-1 temp->moves-table) old-move)
               (fset:excludef (gethash temp-2 temp->moves-table) old-move)
               (try-reduce-or-spill temp-1)
               (try-reduce-or-spill temp-2))

             (retry-coalesce-moves-of-temp (temp)
               (fset:do-set (move (gethash temp temp->moves-table))
                 (when (fset:contains? move-wait-coalesce-again-set move)
                   (fset:excludef move-wait-coalesce-again-set move)
                   (queues:qpush coalesce-move-queue move))))

             (try-move-from-spill-to-reduce (temp)
               (unless (gethash temp precolored-table)
                 (mark-spill-queue-item-obsoleted temp)
                 (queues:qpush reduce-queue temp)))

             (do-freeze ()
               (let ((reduce-temp nil)
                     (spill-temp nil))
                 (fset:do-set (move move-wait-coalesce-again-set)
                   (let ((dst (actual-temp (slot-value move 'dst)))
                         (src (actual-temp (slot-value move 'src))))
                     (flet ((do-check (temp)
                              (cond ((and (< (degrees temp) num-of-regs)
                                          (not (gethash temp precolored-table)))
                                     (setf reduce-temp temp)
                                     (return))
                                    (t
                                     (unless spill-temp
                                       (setf spill-temp temp))))))
                       (do-check dst)
                       (do-check src))))
                 (cond (reduce-temp
                        (freeze-moves-of-temp reduce-temp))
                       (spill-temp
                        (freeze-moves-of-temp spill-temp)))))

             (freeze-moves-of-temp (temp)
               (fset:do-set (move (gethash temp temp->moves-table))
                 (when (fset:contains? move-wait-coalesce-again-set move)
                   (fset:excludef move-wait-coalesce-again-set move)
                   (let ((temp-2 (let ((actual-dst (actual-temp (slot-value move 'dst))))
                                   (if (eq temp actual-dst)
                                       (actual-temp (slot-value move 'src))
                                       actual-dst))))
                     (fset:excludef (gethash temp-2 temp->moves-table) move)
                     ;; the temp-2 may no longer move-related,
                     ;; add it to reduce-queue or spill-queue if so.
                     (try-reduce-or-spill temp-2))))
               (remhash temp temp->moves-table)
               (try-reduce-or-spill temp))

             (do-spill ()
               ;; Just spill one temp.
               (unless (spill-queue-empty?)
                 (let ((temp (pop-spill-queue)))
                   (queues:qpush reduce-queue temp))))

             (assign-regs ()
               (loop for reg in regs
                     do (setf (gethash reg allocation) reg))
               (loop until (null reduced-stack)
                     for temp = (pop reduced-stack)
                     do (let ((reg (find-if
                                    (lambda (reg)
                                      (not
                                       (fset:find-if
                                        (lambda (adj)
                                          (eq (gethash (actual-temp adj) allocation) reg))
                                        (gethash temp temp->adjs-table))))
                                    regs)))
                          (if reg
                              (setf (gethash temp allocation) reg)
                              (push temp spilled-temps))))
               (maphash (lambda (temp alias)
                          (setf (gethash temp allocation)
                                (gethash (actual-temp alias) allocation)))
                        temp->alias-table))

             (rewrite-instrs-for-spilled-temps ()
               (let ((spilled-temp->access-table (make-hash-table))
                     (generated-temps (fset:empty-set)))
                 (dolist (spilled-temp spilled-temps)
                   (setf (gethash spilled-temp spilled-temp->access-table)
                         (frame:alloc-local frame t target)))
                 (flet ((rewrite-instr (dsts srcs rebuild-instr-fun)
                          (let ((src-fetch-stms nil)
                                (new-srcs nil)
                                (dst-restore-stms nil)
                                (new-dsts nil))
                            (dolist (src srcs)
                              (alexandria:if-let (access (gethash src spilled-temp->access-table))
                                (let ((temp (temp:new-temp "spilled_fetch")))
                                  (push temp new-srcs)
                                  (fset:includef generated-temps temp)
                                  (push
                                   (ir:stm-move
                                    (ir:expr-temp temp)
                                    (frame:access-expr
                                     access
                                     (ir:expr-temp (frame:fp target))
                                     target))
                                   src-fetch-stms))
                                (push src new-srcs)))
                            (dolist (dst dsts)
                              (alexandria:if-let (access (gethash dst spilled-temp->access-table))
                                (let ((temp (temp:new-temp "spilled_restore")))
                                  (push temp new-dsts)
                                  (fset:includef generated-temps temp)
                                  (push
                                   (ir:stm-move
                                    (frame:access-expr
                                     access
                                     (ir:expr-temp (frame:fp target))
                                     target)
                                    (ir:expr-temp temp))
                                   dst-restore-stms))
                                (push dst new-dsts)))
                            (nconc
                             (when src-fetch-stms
                               (trivia:let-match1 (list stm-instrs stm-data-frags)
                                   (instr-select:select-instrs
                                    (apply #'ir:stms->stm-compound (nreverse src-fetch-stms))
                                    frame target)
                                 (push stm-data-frags data-frags-list)
                                 stm-instrs))
                             (list
                              (funcall rebuild-instr-fun (nreverse new-dsts) (nreverse new-srcs)))
                             (when dst-restore-stms
                               (trivia:let-match1 (list stm-instrs stm-data-frags)
                                   (instr-select:select-instrs
                                    (apply #'ir:stms->stm-compound (nreverse dst-restore-stms))
                                    frame target)
                                 (push stm-data-frags data-frags-list)
                                 stm-instrs))))))
                   (setf instrs
                         (mapcan (lambda (instr)
                                   (serapeum:match-of instr:instr instr
                                     ((instr:instr-op template dsts srcs jumps)
                                      (rewrite-instr
                                       dsts srcs
                                       (lambda (new-dsts new-srcs)
                                         (instr:instr-op template new-dsts new-srcs jumps))))
                                     ((instr:instr-stack-arg template dsts srcs reloc-fun)
                                      (rewrite-instr
                                       dsts srcs
                                       (lambda (new-dsts new-srcs)
                                         (instr:instr-stack-arg template new-dsts new-srcs reloc-fun))))
                                     ((instr:instr-call template dsts srcs num-of-regs)
                                      (rewrite-instr
                                       dsts srcs
                                       (lambda (new-dsts new-srcs)
                                         (instr:instr-call template new-dsts new-srcs num-of-regs))))
                                     ((instr:instr-move template dst src)
                                      (rewrite-instr
                                       (list dst) (list src)
                                       (lambda (new-dsts new-srcs)
                                         (instr:instr-move
                                          template
                                          (first new-dsts)
                                          (first new-srcs)))))
                                     ((instr:instr-label _ _)
                                      (list instr))))
                                 instrs)))
                 generated-temps)))
      (build)
      (init-queues)
      (loop until (and (zerop (queues:qsize reduce-queue))
                       (spill-queue-empty?)
                       (zerop (queues:qsize coalesce-move-queue))
                       (fset:empty? move-wait-coalesce-again-set))
            do (tagbody
                start
                  (do-reduce)
                  (do-coalesce)
                  (when (plusp (queues:qsize reduce-queue))
                    (go start))
                  (do-freeze)
                  (when (plusp (queues:qsize reduce-queue))
                    (go start))
                  (do-spill)))
      (assign-regs)
      (cond ((not (null spilled-temps))
             (let ((generated-temps (rewrite-instrs-for-spilled-temps)))
               (reg-alloc%
                instrs
                (fset:union generated-temps pre-generated-temps)
                frame
                target)))
            (t
             (flet ((get-reg (temp)
                      (let ((reg (gethash temp allocation)))
                        (unless reg
                          (error "Cannot get reg of ~S." temp))
                        reg)))
               (list
                (remove-if
                 (lambda (instr)
                   ;; remove self moves.
                   (serapeum:match-of instr:instr instr
                     ((instr:instr-move _ dst src)
                      (eq dst src))
                     (_
                      nil)))
                 (mapcar
                  (lambda (instr)
                    (serapeum:match-of instr:instr instr
                      ((instr:instr-op template dsts srcs jumps)
                       (instr:instr-op template
                                       (mapcar #'get-reg dsts)
                                       (mapcar #'get-reg srcs)
                                       jumps))
                      ((instr:instr-stack-arg template dsts srcs reloc-fun)
                       (instr:instr-stack-arg template
                                              (mapcar #'get-reg dsts)
                                              (mapcar #'get-reg srcs)
                                              reloc-fun))
                      ((instr:instr-call template dsts srcs num-of-regs)
                       (instr:instr-call template
                                         (mapcar #'get-reg dsts)
                                         (mapcar #'get-reg srcs)
                                         num-of-regs))
                      ((instr:instr-move template dst src)
                       (instr:instr-move template
                                         (get-reg dst)
                                         (get-reg src)))
                      ((instr:instr-label _ _)
                       instr)))
                  instrs))
                (apply #'nconc (nreverse data-frags-list)))))))))

(defun temp->adjs-table->graphviz (temp->adjs-table &optional (stream t))
  (format stream "digraph G {~%")
  (maphash (lambda (temp adjs)
             (cond ((fset:empty? adjs)
                    (format stream "  \"(~A, ~A)\"~%"
                            (util:str-without-newlines (temp:temp-name temp))
                            (temp:temp-num temp)))
                   (t
                    (fset:do-set (adj adjs)
                      (format stream "  \"(~A, ~A)\" -> \"(~A, ~A)\"~%"
                              (util:str-without-newlines (temp:temp-name temp)) (temp:temp-num temp)
                              (util:str-without-newlines (temp:temp-name adj)) (temp:temp-num adj))))))
           temp->adjs-table)
  (format stream "}~%"))

(defun temp->moves-table->graphviz (temp->moves-table &optional (stream t))
  (format stream "digraph G {~%")
  (maphash (lambda (temp moves)
             (cond ((fset:empty? moves)
                    (format stream "  \"(~A, ~A)\"~%"
                            (util:str-without-newlines (temp:temp-name temp))
                            (temp:temp-num temp)))
                   (t
                    (fset:do-set (move moves)
                      (apply #'format stream "  \"(~A, ~A)\" -> \"(~A, ~A)\"~%"
                             (with-slots (dst src) move
                               (list
                                (util:str-without-newlines (temp:temp-name src)) (temp:temp-num src)
                                (util:str-without-newlines (temp:temp-name dst)) (temp:temp-num dst))))))))
           temp->moves-table)
  (format stream "}~%"))
