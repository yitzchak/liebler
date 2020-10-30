(in-package #:liebler)


(defmethod degree (graph vertex)
  (reduce-neighbors (lambda (previous neighbor)
                      (+ previous
                         (if (equal vertex neighbor) 2 1)))
                    graph
                    vertex
                    0))

