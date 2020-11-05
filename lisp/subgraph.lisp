(in-package #:liebler)


(defclass subgraph ()
  ((parent-graph
     :accessor parent-graph
     :initarg :parent-graph


(defmethod make-subgraph (graph &optional predicate)
  (make-instance 'subgraph
                 :parent-graph (color-graph graph 2
                                            :color (if predicate
                                                     (lambda (vertex)
                                                       (if (funcall predicate vertex) 1 0)
                                                       1)))))


                                                       
