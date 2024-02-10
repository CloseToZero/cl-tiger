(cl:defpackage :cl-tiger/flow-graph
  (:use :cl)
  (:local-nicknames
   (:temp :cl-tiger/temp)
   (:asm :cl-tiger/asm)
   (:graph :cl-tiger/graph))
  (:export
   #:flow-graph
   #:flow-graph-graph
   #:flow-graph-fake-node
   #:flow-graph-defs-table
   #:flow-graph-uses-table
   #:flow-graph-is-move-table
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
    ;; A map from a node to a list of temp:temp
    :type fset:map
    :initform (fset:empty-map)
    :reader flow-graph-defs-table)
   (uses-table
    ;; A map from a node to a list of temp:temp
    :type fset:map
    :initform (fset:empty-map)
    :reader flow-graph-uses-table)
   (is-move-table
    ;; A map from a node to a boolean
    :type fset:map
    :initform (fset:empty-map)
    :reader flow-graph-is-move-table)))

(defun is-fake-node (flow-graph node)
  (eq (flow-graph-fake-node flow-graph) node))

(defun instrs->flow-graph (instrs)
  (let* ((flow-graph (make-instance 'flow-graph))
         (graph (flow-graph-graph flow-graph))
         (fake-node (let ((node (graph:new-node graph "--fake--")))
                      (with-slots (fake-node) flow-graph
                        (setf fake-node node))
                      node))
         (defs-table (flow-graph-defs-table flow-graph))
         (uses-table (flow-graph-uses-table flow-graph))
         (is-move-table (flow-graph-is-move-table flow-graph))
         (label->node-table (fset:empty-map))
         (instr->node-table (fset:empty-map)))
    ;; Create all nodes first.
    (loop for instr in instrs
          do (let* ((name (asm:format-instr instr))
                    (node (graph:new-node graph name)))
               (fset:includef instr->node-table instr node)
               (trivia:when-match (asm:label-instr _ name) instr
                 (fset:includef label->node-table name node))))
    (labels ((instr->node (instr)
               (let ((node (fset:@ instr->node-table instr)))
                 (unless node
                   (error "Undefined node of the instr: ~A" (asm:format-instr instr)))
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
            do (serapeum:match-of asm:instr instr
                 ((asm:op-instr _ dsts srcs jumps)
                  (let ((node (instr->node instr)))
                    (fset:includef defs-table node dsts)
                    (fset:includef uses-table node srcs)
                    (fset:includef is-move-table node nil)
                    (serapeum:match-of asm:maybe-jump jumps
                      ((asm:is-jump targets)
                       (cond ((null targets)
                              (graph:add-edge node fake-node))
                             (t
                              (dolist (target targets)
                                (let ((target-node (label->node target)))
                                  (graph:add-edge node target-node))))))
                      (asm:not-jump
                       (add-fall-through-edge node rest-instrs)))))
                 ((asm:label-instr _ _)
                  (let ((node (instr->node instr)))
                    (fset:includef defs-table node nil)
                    (fset:includef uses-table node nil)
                    (fset:includef is-move-table node nil)
                    (add-fall-through-edge node rest-instrs)))
                 ((asm:move-instr _ dst src)
                  (let ((node (instr->node instr)))
                    (fset:includef defs-table node (list dst))
                    (fset:includef uses-table node (list src))
                    (fset:includef is-move-table node t)
                    (add-fall-through-edge node rest-instrs)))))
      flow-graph)))
