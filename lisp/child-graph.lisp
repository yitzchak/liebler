(in-package #:liebler)


(defclass child-graph ()
  ((parent-graph
     :reader parent-graph
     :initarg :parent-graph)))


(defmethod directedp ((graph child-graph))
  (directedp (parent-graph graph)))


(defmethod edges ((graph child-graph))
  (edges (parent-graph graph)))


(defmethod neighbors ((graph child-graph) vertex)
  (neighbors (parent-graph graph) vertex))


(defmethod vertices ((graph child-graph))
  (vertices (parent-graph graph)))


(defmethod neighborp ((graph child-graph) vertex1 vertex2)
  (neighborp (parent-graph graph) vertex1 vertex2))


(defmethod (setf neighborp) (new-value (graph child-graph) vertex1 vertex2)
  (setf (neighborp (parent-graph graph) vertex1 vertex2) new-value))


(defmethod degree ((graph child-graph) vertex)
  (degree (parent-graph graph) vertex))


(defmethod order ((graph child-graph))
  (order (parent-graph graph)))


(defmethod color-graph ((graph child-graph) count &key color)
  (color-graph (parent-graph graph) count :key color))


(defmethod colors ((graph child-graph))
  (colors (parent-graph graph)))


(defmethod copy-colored-graph ((graph child-graph))
  (copy-colored-graph (parent-graph graph)))


(defmethod color ((graph child-graph) vertex)
  (color (parent-graph graph) vertex))


(defmethod (setf color) (new-value (graph child-graph) vertex)
  (setf (color (parent-graph graph) vertex) new-value))
