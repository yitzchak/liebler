(in-package #:liebler)


(defclass child-graph ()
  ((graph
     :reader graph
     :initarg :graph)))


(defmethod directedp ((graph child-graph))
  (directedp (graph graph)))


(defmethod edges ((graph child-graph))
  (edges (graph graph)))


(defmethod neighbors ((graph child-graph))
  (neighbors (graph graph)))


(defmethod vertices ((graph child-graph))
  (vertices (graph graph)))


(defmethod neighborp ((graph child-graph) vertex1 vertex2)
  (neighborp (graph graph) vertex1 vertex2))


(defmethod (setf neighborp) (new-value (graph child-graph) vertex1 vertex2)
  (setf (neighborp (graph graph) vertex1 vertex2) new-value))


(defmethod degree ((graph child-graph) vertex)
  (degree (graph graph) vertex))


(defmethod order ((graph child-graph))
  (order (graph graph)))


(defmethod color-graph ((graph child-graph) count &key color)
  (color-graph (graph graph) count :key color))


(defmethod colors ((graph child-graph))
  (colors (graph graph)))


(defmethod copy-colored-graph ((graph child-graph))
  (copy-colored-graph (graph graph)))


(defmethod color ((graph child-graph) vertex)
  (color (graph graph) vertex))


(defmethod (setf color) (new-value (graph child-graph) vertex)
  (setf (color (graph graph) vertex) new-value))
