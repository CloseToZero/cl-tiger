(cl:defpackage :cl-tiger/graph
  (:use :cl)
  (:local-nicknames
   (:utils :cl-tiger/utils)
   (:symbol :cl-tiger/symbol)
   (:cl-ds :cl-data-structures)
   (:skip-list :cl-ds.sets.skip-list))
  (:export
   #:node
   #:node-name
   #:node-index
   #:node-graph
   #:node-succs
   #:node-pres
   #:node<
   #:node-eq
   #:empty-node-set
   #:node-set-contains
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
    :type skip-list:mutable-skip-list-set
    :initform (empty-node-set)
    :reader node-succs)
   (pres
    :type skip-list:mutable-skip-list-set
    :initform (empty-node-set)
    :reader node-pres)))

(defun node< (node-1 node-2)
  (< (node-index node-1) (node-index node-2)))

(defun node-eq (node-1 node-2)
  (and (eq (node-graph node-1) (node-graph node-2))
       (eql (node-index node-1) (node-index node-2))))

(defun empty-node-set ()
  (skip-list:make-mutable-skip-list-set #'node< #'node-eq))

(defun node-set-contains (node-set node)
  ;; Discard the second return value.
  (let ((found (cl-ds:at node-set node)))
    found))

(defclass graph ()
  ((nodes
    :type skip-list:mutable-skip-list-set
    :initform (empty-node-set)
    :reader graph-nodes)
   (next-index
    :type fixnum
    :initform 0
    :accessor graph-next-index)))

(defun graph ()
  (make-instance 'graph))

(defun new-node (graph name)
  (let* ((next-index (graph-next-index graph))
         (node (make-instance 'node :name name :graph graph :index next-index)))
    (cl-ds:put! (graph-nodes graph) node)
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
  (cl-ds:put! (node-succs from-node) to-node)
  (cl-ds:put! (node-pres to-node) from-node)
  (values))

(defun remove-edge (from-node to-node &optional (error-if-not-exists t))
  (if error-if-not-exists
      (unless (edge-exists from-node to-node)
        (error "Edge ~A -> ~A doesn't exist" from-node to-node))
      (check-same-graph from-node to-node nil))
  (cl-ds:erase! (node-succs from-node) to-node)
  (cl-ds:erase! (node-pres to-node) from-node)
  (values))

(defun edge-exists (from-node to-node)
  (check-same-graph from-node to-node nil)
  (node-set-contains (node-succs from-node) to-node))

(defun graph->graphviz (graph &optional (stream t))
  (let ((visited (empty-node-set)))
    (labels ((print-node (node)
               (cl-ds:put! visited node)
               (cl-ds:across
                (node-succs node)
                (lambda (succ)
                  (format stream "  \"(~A, ~A)\" -> \"(~A, ~A)\"~%"
                          (utils:str-without-newlines (node-name node)) (node-index node)
                          (utils:str-without-newlines (node-name succ)) (node-index succ))
                  (unless (node-set-contains visited succ)
                    (print-node succ))))))
      (format stream "digraph G {~%")
      (cl-ds:across
       (graph-nodes graph)
       (lambda (node)
         (unless (node-set-contains visited node)
           (print-node node))))
      (format stream "}~%"))))
