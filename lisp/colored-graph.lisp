(in-package :liebler)


(defclass colored-graph ()
  ((vertex-colors
     :reader vertex-colors
     :initarg :vertex-colors
     :initform (make-hash-table :test #'equalp))
   (graph
     :reader graph
     :initarg :graph)))


(defmethod color ((graph colored-graph) vertex)
  (gethash vertex (vertex-colors graph) 0))


(defmethod (setf color) (new-value (graph colored-graph) vertex)
  (if (zerop new-value)
    (remhash vertex (vertex-colors graph))
    (setf (gethash vertex (vertex-colors graph)) new-value)))


(defmethod color-graph (graph count &key color)
  (let ((colored-graph (make-instance 'colored-graph :graph graph)))
    (when (and color
               (not (equal 0 color)))
      (map-vertices nil
                    (lambda (vertex)
                      (setf (color colored-graph vertex)
                            (if (functionp color)
                              (funcall color vertex)
                              color)))
                    graph))
    colored-graph))


(defmethod directedp ((graph colored-graph))
  (directp (graph graph)))


(defmethod map-vertices (result-type function (graph colored-graph))
  (map-vertices result-type function (graph graph)))


(defmethod map-edges (result-type function (graph colored-graph))
  (map-edges result-type function (graph graph)))


(defmethod map-neighbors (result-type function (graph colored-graph) vertex)
  (map-neighbors result-type function (graph graph) vertex))


(defmethod neighborp ((graph colored-graph) vertex1 vertex2)
  (neighborp (graph graph) vertex1 vertex2))


(defmethod (setf neighborp) (new-value (graph colored-graph) vertex1 vertex2)
  (setf (neighborp (graph graph) vertex1 vertex2) new-value))


(defmethod degree ((graph colored-graph) vertex)
  (degree (graph graph) vertex))


(defmethod order ((graph colored-graph))
  (order (graph graph)))


(defmethod vertices ((graph colored-graph))
  (vertices (graph graph)))



