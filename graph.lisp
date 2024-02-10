(cl:defpackage :cl-tiger/graph
  (:use :cl)
  (:local-nicknames
   (:utils :cl-tiger/utils)
   (:symbol :cl-tiger/symbol))
  (:export
   #:node
   #:node-name
   #:node-index
   #:node-graph
   #:node-succs
   #:node-pres
   #:graph
   #:graph-nodes
   #:graph-next-index
   #:new-node
   #:add-edge
   #:remove-edge
   #:edge-exists
   #:graph->graphviz))

(cl:in-package :cl-tiger/graph)

(defclass node ()
  ((name
    :type symbol:sym
    :initarg :name
    :reader node-name)
   (index
    :type fixnum
    :initarg :index
    :reader node-index
    :documentation "An unique index of each node which can be used to test equality
(only if two nodes are in the same graph).")
   (graph
    :type (or graph null)
    :initarg :graph
    :initform nil
    :reader node-graph
    :documentation "The graph this node belongs to.")
   (succs
    :type fset:set
    :initform (fset:empty-set)
    :accessor node-succs)
   (pres
    :type fset:set
    :initform (fset:empty-set)
    :accessor node-pres)))

(defmethod fset:compare ((x node) (y node))
  (cond ((eq (node-graph x) (node-graph y))
         (let ((index-x (node-index x))
               (index-y (node-index y)))
           (cond ((< index-x index-y) :less)
                 ((> index-x index-y) :greater)
                 (t :equal))))
        (t
         :unequal)))

(defclass graph ()
  ((nodes
    :type fset:set
    :initform (fset:empty-set)
    :accessor graph-nodes)
   (next-index
    :type fixnum
    :initform 0
    :accessor graph-next-index)))

(defun graph ()
  (make-instance 'graph))

(defun new-node (graph name)
  (let* ((next-index (graph-next-index graph))
         (node (make-instance 'node :name name :graph graph :index next-index)))
    (fset:includef (graph-nodes graph) node)
    (incf (graph-next-index graph))
    node))

(defun check-same-graph (node-1 node-2 allow-null-graph)
  (unless (or allow-null-graph (node-graph node-1))
    (error "Node ~S are not in a graph." node-1))
  (unless (eq (node-graph node-1) (node-graph node-2))
    (error "Node ~S and node ~S are not in the same graph."
           node-1 node-2))
  (values))

(defun add-edge (from-node to-node)
  (check-same-graph from-node to-node nil)
  (fset:includef (node-succs from-node) to-node)
  (fset:includef (node-pres to-node) from-node)
  (values))

(defun remove-edge (from-node to-node &optional (error-if-not-exists t))
  (if error-if-not-exists
      (unless (edge-exists from-node to-node)
        (error "Edge ~A -> ~A doesn't exist" from-node to-node))
      (check-same-graph from-node to-node nil))
  (fset:excludef (node-succs from-node) to-node)
  (fset:excludef (node-pres to-node) from-node)
  (values))

(defun edge-exists (from-node to-node)
  (check-same-graph from-node to-node nil)
  (fset:contains? (node-succs from-node) to-node))

(defun graph->graphviz (graph &optional (stream t))
  (let ((visited (fset:empty-set)))
    (labels ((print-node (node)
               (fset:includef visited node)
               (fset:do-set (succ (node-succs node))
                 (format stream "  \"(~A, ~A)\" -> \"(~A, ~A)\"~%"
                         (utils:str-without-newlines (node-name node)) (node-index node)
                         (utils:str-without-newlines (node-name succ)) (node-index succ))
                 (unless (fset:contains? visited succ)
                   (print-node succ)))))
      (format stream "digraph G {~%")
      (fset:do-set (node (graph-nodes graph))
        (unless (fset:contains? visited node)
          (print-node node)))
      (format stream "}~%"))))
