(cl:defpackage :cl-tiger/liveness
  (:use :cl)
  (:local-nicknames
   (:temp :cl-tiger/temp)
   (:graph :cl-tiger/graph)
   (:flow-graph :cl-tiger/flow-graph))
  (:export
   #:liveness))

(cl:in-package :cl-tiger/liveness)

(defun nodes-in-postorder (flow-graph)
  (let ((graph (flow-graph:flow-graph-graph flow-graph))
        (visited (fset:empty-set))
        (result nil))
    (labels ((rec (node)
               (fset:includef visited node)
               (fset:do-set (succ (graph:node-succs node))
                 (unless (fset:contains? visited succ)
                   (rec succ)))
               (unless (flow-graph:is-fake-node flow-graph node)
                 (push node result))))
      (fset:do-set (node (graph:graph-nodes graph))
        (unless (fset:contains? visited node)
          (rec node)))
      (reverse result))))

(defun liveness (flow-graph)
  "Return a list of the form (live-in-table live-out-table) of the FLOW-GRAPH.
Each live table is a map from each node of the flow-graph to a live set (temp set)."
  (let ((defs-table (flow-graph:flow-graph-defs-table flow-graph))
        (uses-table (flow-graph:flow-graph-uses-table flow-graph))
        (live-in-table (fset:empty-map))
        (live-out-table (fset:empty-map)))
    (macrolet ((node-live-set (live-table-place node-form)
                 (let ((node-var (gensym "node"))
                       (live-set-var (gensym "live-set")))
                   `(let ((,node-var ,node-form))
                      (or (fset:@ ,live-table-place ,node-var)
                          (let ((,live-set-var (fset:empty-set)))
                            (fset:includef ,live-table-place ,node-var ,live-set-var)
                            ,live-set-var))))))
      (loop with changed = t
            while changed
            do (setf changed nil)
               (loop for node in (nodes-in-postorder flow-graph)
                     for old-live-in-set = (node-live-set live-in-table node)
                     for old-live-out-set = (node-live-set live-out-table node)
                     do (let* ((new-live-out-set
                                 (fset:reduce
                                  (lambda (acc-live-out-set succ)
                                    (fset:union acc-live-out-set (node-live-set live-in-table succ)))
                                  (graph:node-succs node)
                                  :initial-value (fset:empty-set)))
                               (new-live-in-set
                                 (fset:union
                                  (fset:set-difference new-live-out-set (fset:@ defs-table node))
                                  (fset:@ uses-table node))))
                          (fset:includef live-out-table node new-live-out-set)
                          (fset:includef live-in-table node new-live-in-set)
                          (unless (and (fset:equal? new-live-out-set old-live-out-set)
                                       (fset:equal? new-live-in-set old-live-in-set))
                            (setf changed t)))))
      (list live-in-table live-out-table))))
