(in-package #:liebler)


(defgeneric directedp (graph))


(defgeneric map-vertices (result-type function graph))


(defgeneric map-edges (result-type function graph))


(defgeneric map-neighbors (result-type function graph vertex))


(defgeneric neighborp (graph vertex1 vertex2))


(defgeneric (setf neighborp) (new-value graph vertex1 vertex2))


(defgeneric rank (graph vertex))


(defgeneric order (graph))


(defgeneric color-graph (graph &key count color))


(defgeneric color (graph vertex)
  (:method (graph vertex)
    (declare (ignore graph vertex))))


(defgeneric (setf color) (new-value graph vertex))


(defgeneric count-vertices (predicate graph)
  (:method (predicate graph)
    (let ((count 0))
      (map-vertices nil
                    (lambda (vertex)
                      (when (funcall predicate vertex)
                        (incf count)))
                    graph)
      count)))


(defgeneric copy-graph (graph))
