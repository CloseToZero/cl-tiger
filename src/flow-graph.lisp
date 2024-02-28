(cl:defpackage :cl-tiger/flow-graph
  (:use :cl)
  (:local-nicknames
   (:util :cl-tiger/util)
   (:temp :cl-tiger/temp)
   (:instr :cl-tiger/instr)
   (:graph :cl-tiger/graph))
  (:export
   #:flow-graph
   #:flow-graph-graph
   #:flow-graph-fake-node
   #:flow-graph-defs-table
   #:flow-graph-uses-table
   #:flow-graph-is-move-set
   #:is-fake-node
   #:instrs->flow-graph))

(cl:in-package :cl-tiger/flow-graph)

(defclass flow-graph ()
  ((graph
    :type graph:graph
    :initform (graph:graph)
    :reader flow-graph-graph)
   (fake-node
    :type graph:node
    :reader flow-graph-fake-node)
   (defs-table
    ;; A map from a node to a set of temp:temp
    :type fset:map
    :reader flow-graph-defs-table)
   (uses-table
    ;; A map from a node to a set of temp:temp
    :type fset:map
    :reader flow-graph-uses-table)
   (is-move-set
    ;; A set of move nodes
    :type fset:set
    :reader flow-graph-is-move-set)))

(defun is-fake-node (flow-graph node)
  (eq (flow-graph-fake-node flow-graph) node))

(defun instrs->flow-graph (instrs)
  (let* ((graph (graph:graph))
         (fake-node (graph:new-node graph "--fake--"))
         (defs-table (fset:empty-map))
         (uses-table (fset:empty-map))
         (is-move-set (fset:empty-set))
         (label->node-table (fset:empty-map))
         (instr->node-table (fset:empty-map)))
    ;; Create all nodes first.
    (loop for instr in instrs
          do (let* ((name (instr:format-instr instr))
                    (node (graph:new-node graph name)))
               (fset:includef instr->node-table instr node)
               (trivia:when-match (instr:instr-label _ name) instr
                 (fset:includef label->node-table name node))))
    (labels ((instr->node (instr)
               (let ((node (fset:@ instr->node-table instr)))
                 (unless node
                   (error "Undefined node of the instr: ~A" (instr:format-instr instr)))
                 node))
             (label->node (label)
               (let ((node (fset:@ label->node-table label)))
                 (unless node
                   (error "Undefined node of the label: ~A" (temp:label-name label)))
                 node))
             (add-fall-through-edge (node rest-instrs)
               (trivia:match rest-instrs
                 ((list* next-instr _)
                  (graph:add-edge node (instr->node next-instr)))
                 (_
                  (graph:add-edge node fake-node)))))
      (loop for (instr . rest-instrs) on instrs
            do (serapeum:match-of instr:instr instr
                 ((instr:instr-op _ dsts srcs jumps)
                  (let ((node (instr->node instr)))
                    (fset:includef defs-table node (util:list->set dsts))
                    (fset:includef uses-table node (util:list->set srcs))
                    (serapeum:match-of instr:maybe-jump jumps
                      ((instr:is-jump targets)
                       (cond ((null targets)
                              (graph:add-edge node fake-node))
                             (t
                              (dolist (target targets)
                                (let ((target-node (label->node target)))
                                  (graph:add-edge node target-node))))))
                      (instr:not-jump
                       (add-fall-through-edge node rest-instrs)))))
                 ((or (instr:instr-stack-arg _ dsts srcs _)
                      (instr:instr-call _ dsts srcs _))
                  (let ((node (instr->node instr)))
                    (fset:includef defs-table node (util:list->set dsts))
                    (fset:includef uses-table node (util:list->set srcs))
                    (add-fall-through-edge node rest-instrs)))
                 ((instr:instr-label _ _)
                  (let ((node (instr->node instr)))
                    (fset:includef defs-table node (fset:empty-set))
                    (fset:includef uses-table node (fset:empty-set))
                    (add-fall-through-edge node rest-instrs)))
                 ((instr:instr-move _ dst src)
                  (let ((node (instr->node instr)))
                    (fset:includef defs-table node (fset:with (fset:empty-set) dst))
                    (fset:includef uses-table node (fset:with (fset:empty-set) src))
                    (fset:includef is-move-set node)
                    (add-fall-through-edge node rest-instrs)))))
      (let ((flow-graph (make-instance 'flow-graph)))
        (with-slots ((g graph)
                     (f fake-node)
                     (d defs-table)
                     (u uses-table)
                     (m is-move-set))
            flow-graph
          (setf g graph)
          (setf f fake-node)
          (setf d defs-table)
          (setf u uses-table)
          (setf m is-move-set))
        flow-graph))))
