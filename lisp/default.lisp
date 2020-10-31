(in-package #:liebler)


(defmethod degree (graph vertex)
  (reduce-neighbors (lambda (previous neighbor)
                      (+ previous
                         (if (equal vertex neighbor) 2 1)))
                    graph
                    vertex
                    0))


(defmethod ex-degree (graph vertex)
  (reduce-neighbors (lambda (previous neighbor)
                      (+ previous
                         (degree neighbor)))
                    graph
                    vertex
                    0))


(defmethod color-by-degree (graph)
  (let ((result (color-graph graph (order graph))))
    (do-edges (vertex1 vertex2 graph result)
      (incf (color result vertex1))
      (incf (color result vertex2)))))


(defmethod color-by-ex-degree (graph)
  (let ((degrees (color-by-degree graph))
        (result (color-graph graph (order graph))))
    (do-edges (vertex1 vertex2 graph result)
      (incf (color result vertex1) (degree degrees vertex2))
      (incf (color result vertex2) (degree degrees vertex1)))))
