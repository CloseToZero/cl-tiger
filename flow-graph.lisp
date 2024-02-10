(cl:defpackage :cl-tiger/flow-graph
  (:use :cl)
  (:local-nicknames
   (:temp :cl-tiger/temp)
   (:asm :cl-tiger/asm)
   (:graph :cl-tiger/graph)
   (:cl-ds :cl-data-structures)
   (:skip-list :cl-ds.sets.skip-list)
   (:hamt :cl-data-structures.dicts.hamt))
  (:export
   #:flow-graph
   #:flow-graph-graph
   #:flow-graph-fake-node
   #:flow-graph-defs-table
   #:flow-graph-uses-table
   #:flow-graph-is-move-table
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
    :type hamt:mutable-hamt-dictionary
    :initform (empty-node-dict)
    :reader flow-graph-defs-table)
   (uses-table
    ;; A map from a node to a list of temp:temp
    :type hamt:mutable-hamt-dictionary
    :initform (empty-node-dict)
    :reader flow-graph-uses-table)
   (is-move-table
    ;; A map from a node to a boolean
    :type skip-list:mutable-skip-list-set
    :initform (empty-node-set)
    :reader flow-graph-is-move-table)))

(defun empty-node-dict ()
  (hamt:make-mutable-hamt-dictionary #'sxhash #'graph:node-eq))

(defun empty-node-set ()
  (skip-list:make-mutable-skip-list-set #'graph:node< #'graph:node-eq))

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
         (label->node-table (hamt:make-mutable-hamt-dictionary #'sxhash #'eq))
         (instr->node-table (hamt:make-mutable-hamt-dictionary #'sxhash #'eq)))
    ;; Create all nodes first.
    (loop for instr in instrs
          do (let* ((name (asm:format-instr instr))
                    (node (graph:new-node graph name)))
               (cl-ds:insert! instr->node-table instr node)
               (trivia:when-match (asm:label-instr _ name) instr
                 (cl-ds:insert! label->node-table name node))))
    (labels ((instr->node (instr)
               (let ((node (cl-ds:at instr->node-table instr)))
                 (unless node
                   (error "Undefined node of the instr: ~A" (asm:format-instr instr)))
                 node))
             (label->node (label)
               (let ((node (cl-ds:at label->node-table label)))
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
                    (cl-ds:insert! defs-table node dsts)
                    (cl-ds:insert! uses-table node srcs)
                    (cl-ds:put! is-move-table node)
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
                    (cl-ds:insert! defs-table node nil)
                    (cl-ds:insert! uses-table node nil)
                    (cl-ds:put! is-move-table node)
                    (add-fall-through-edge node rest-instrs)))
                 ((asm:move-instr _ dst src)
                  (let ((node (instr->node instr)))
                    (cl-ds:insert! defs-table node (list dst))
                    (cl-ds:insert! uses-table node (list src))
                    (cl-ds:put! is-move-table node)
                    (add-fall-through-edge node rest-instrs)))))
      flow-graph)))
