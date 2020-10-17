(in-package #:liebler)


(defgeneric directedp (graph))


(defgeneric map-vertices (result-type function graph &optional color-function))


(defgeneric map-edges (result-type function graph))


(defgeneric map-neighbors (result-type function graph vertex))


(defgeneric neighborp (graph vertex1 vertex2))


(defgeneric (setf neighborp) (new-value graph vertex1 vertex2))


(defgeneric rank (graph vertex))


(defgeneric order (graph))


(defgeneric color-graph (graph &key colors color-function))


(defgeneric color (graph vertex)
  (:method (graph vertex)
    (declare (ignore graph vertex))
    0))


(defgeneric (setf color) (new-value graph vertex))


(defgeneric count-vertices (predicate graph &optional color-function)
  (:method (predicate graph &optional color-function)
    (let ((count 0))
      (map-vertices nil
                    (lambda (vertex color)
                      (when (funcall predicate vertex color)
                        (incf count)))
                    graph color-function)
      count)))


(defgeneric copy-graph (graph))
