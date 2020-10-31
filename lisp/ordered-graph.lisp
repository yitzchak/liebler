(in-package #:liebler)


(defclass ordered-graph (child-graph)
  ((vertex-list
     :reader vertex-list
     :initarg :vertex-list)))


(defmethod order-graph-by-color (graph &optional predicate)
  (make-instance 'ordered-graph
                 :vertex-list (stable-sort (map-vertices 'list #'identity graph)
                                           (or predicate #'<)
                                           :key (lambda (vertex)
                                                  (color graph vertex)))
                 :parent-graph graph))


(defclass ordered-graph-vertex-iterator ()
  ((tail
     :accessor tail
     :initarg :tail)
   (graph
     :reader graph
     :initarg :graph)))


(defmethod vertices ((graph ordered-graph))
  (make-instance 'ordered-graph-vertex-iterator
                 :tail (vertex-list graph)
                 :graph graph))


(defmethod advance ((iterator ordered-graph-vertex-iterator))
  (setf (tail iterator) (cdr (tail iterator)))
  iterator)


(defmethod validp ((iterator ordered-graph-vertex-iterator))
  (and (tail iterator) t))


(defmethod current ((iterator ordered-graph-vertex-iterator))
  (car (tail iterator)))


(defmethod reset ((iterator ordered-graph-vertex-iterator))
  (setf (tail iterator) (vertex-list (graph iterator)))
  iterator)





