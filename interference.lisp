(cl:defpackage :cl-tiger/interference
  (:use :cl)
  (:local-nicknames
   (:utils :cl-tiger/utils)
   (:temp :cl-tiger/temp)
   (:asm :cl-tiger/asm)
   (:graph :cl-tiger/graph)
   (:flow-graph :cl-tiger/flow-graph))
  (:export
   #:interference-graph
   #:interference-graph-graph
   #:interference-graph-temp->node-table
   #:interference-graph-node->temp-table
   #:interference-graph-move-nodes))

(cl:in-package :cl-tiger/interference)

(defclass interference-graph ()
  ((graph
    :type graph:graph
    :reader interference-graph-graph)
   (temp->node-table
    :type fset:map
    :reader interference-graph-temp->node-table)
   (node->temp-table
    :type fset:map
    :reader interference-graph-node->temp-table)
   (moves
    ;; A list of list of the form (node-dst node-src) where
    ;; the node-dst represent the node of dst temp of a move instruction,
    ;; and the node-src represent the node of src temp of the same move instruction.
    :type list
    :reader interference-graph-move-nodes)))

(defun interference-graph (flow-graph live-out-table)
  (let ((graph (flow-graph:flow-graph-graph flow-graph))
        (igraph (graph:graph))
        (defs-table (flow-graph:flow-graph-defs-table flow-graph))
        (uses-table (flow-graph:flow-graph-uses-table flow-graph))
        (is-move-set (flow-graph:flow-graph-is-move-set flow-graph))
        (temp->inode-table (fset:empty-map))
        (inode->temp-table (fset:empty-map))
        (moves nil))
    (flet ((temp->inode (temp)
             (or (fset:@ temp->inode-table temp)
                 (let ((inode (graph:new-node
                               igraph
                               (temp:temp-name temp))))
                   (fset:includef temp->inode-table temp inode)
                   (fset:includef inode->temp-table inode temp)
                   inode))))
      (fset:do-set (node (graph:graph-nodes graph))
        (unless (flow-graph:is-fake-node flow-graph node)
          (let* ((live-out-set (fset:@ live-out-table node))
                 (is-move (fset:contains? is-move-set node))
                 (exclude-temp (and is-move (utils:set-singleton (fset:@ uses-table node)))))
            (when is-move
              (push (list (utils:set-singleton (fset:@ defs-table node)) exclude-temp) moves))
            (fset:do-set (def (fset:@ defs-table node))
              (let ((def-inode (temp->inode def)))
                (fset:do-set (live-out-temp live-out-set)
                  (unless (or (eq live-out-temp def)
                              (eq live-out-temp exclude-temp))
                    (let ((live-out-inode (temp->inode live-out-temp)))
                      (graph:add-edge def-inode live-out-inode)
                      (graph:add-edge live-out-inode def-inode)))))))))
      (let ((result-graph (make-instance 'interference-graph)))
        (with-slots ((g graph) (tn temp->node-table) (nt node->temp-table) (m moves)) result-graph
          (setf g igraph)
          (setf tn temp->inode-table)
          (setf nt inode->temp-table)
          (setf m (nreverse moves)))
        result-graph))))
