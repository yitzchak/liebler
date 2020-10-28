(in-package #:liebler)


(defgeneric directedp (graph))


(defgeneric neighborp (graph vertex1 vertex2))


(defgeneric (setf neighborp) (new-value graph vertex1 vertex2))


(defgeneric degree (graph vertex)
  (:method (graph vertex)
    (count-vertices (lambda (v)
                      (neighborp graph vertex v))
                    graph)))


(defgeneric order (graph))


(defgeneric color-graph (graph count &key color))


(defgeneric colors (graph)
  (:method (graph)
    1))


(defgeneric copy-colored-graph (graph)
  (:method (graph)
    (color-graph graph (colors graph) :color (lambda (vertex) (color graph vertex)))))


(defgeneric color (graph vertex)
  (:method (graph vertex)
    (declare (ignore graph vertex))
    0))


(defgeneric (setf color) (new-value graph vertex))


(defgeneric copy-graph (graph))


(defgeneric order-graph-by-color (graph))



