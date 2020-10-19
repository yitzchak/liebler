(in-package #:liebler)


(defgeneric directedp (graph))


(defgeneric map-vertices (result-type function graph))


(defgeneric map-edges (result-type function graph))


(defgeneric map-neighbors (result-type function graph vertex))


(defgeneric neighborp (graph vertex1 vertex2))


(defgeneric (setf neighborp) (new-value graph vertex1 vertex2))


(defgeneric rank (graph vertex))


(defgeneric order (graph))


(defgeneric color-graph (graph count &key color))


(defgeneric color (graph vertex)
  (:method (graph vertex)
    (declare (ignore graph vertex))
    0))


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


(defgeneric all-vertices (predicate graph)
  (:method (predicate graph)
    (catch 'query
      (map-vertices nil
                    (lambda (vertex)
                      (unless (funcall predicate vertex)
                        (throw 'query nil)))
                    graph)
      t)))


(defgeneric notall-vertices (predicate graph)
  (:method (predicate graph)
    (catch 'query
      (map-vertices nil
                    (lambda (vertex)
                      (unless (funcall predicate vertex)
                        (throw 'query t)))
                    graph))))


(defgeneric some-vertices (predicate graph)
  (:method (predicate graph)
    (catch 'query
      (map-vertices nil
                    (lambda (vertex)
                      (when (funcall predicate vertex)
                        (throw 'query t)))
                    graph))))


(defgeneric notany-vertices (predicate graph)
  (:method (predicate graph)
    (catch 'query
      (map-vertices nil
                    (lambda (vertex)
                      (when (funcall predicate vertex)
                        (throw 'query nil)))
                    graph)
      t)))


(defgeneric copy-graph (graph))
