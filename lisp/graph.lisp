(in-package #:liebler)


(defgeneric directedp (graph))


(defgeneric neighborp (graph vertex1 vertex2))


(defgeneric (setf neighborp) (new-value graph vertex1 vertex2))


(defgeneric degree (graph vertex))


(defgeneric ex-degree (graph vertex))


(defgeneric order (graph))


(defgeneric vertices (graph))


(defgeneric edges (graph))


(defgeneric neighbors (graph vertex))


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


(defgeneric order-graph-by-color (graph &optional predicate))


(defgeneric color-by-degree (graph))


(defgeneric color-by-ex-degree (graph))


(defgeneric make-subgraph (graph &optional predicate))
