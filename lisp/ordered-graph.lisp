(in-package #:lieber)


(defclass ordered-graph ()
  ((vertex-list
     :reader vertex-list
     :initarg :vertex-list)
   (graph
     :reader graph
     :initarg :graph)))


(defmethod order-graph-by-color (graph)
  (make-instance 'ordered-graph
                 :vertex-list (stable-sort (map-vertices 'list #'identity graph)
                                           #'<
                                           :key (lambda (vertex)
                                                  (color graph vertex)))
                 :graph graph))


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


(defmethod valid ((iterator ordered-graph-vertex-iterator))
  (and (tail iterator) t))


(defmethod current ((iterator ordered-graph-vertex-iterator))
  (car (tail iterator)))


(defmethod reset ((iterator ordered-graph-vertex-iterator))
  (setf (tail iterator) (vertex-list (graph iterator)))
  iterator)





