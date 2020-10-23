(in-package #:liebler)


(defgeneric directedp (graph))


(defgeneric vertices (graph))


(defgeneric advance (iterator))


(defgeneric valid (iterator))


(defgeneric current (iterator))


(defgeneric reset (iterator))


(defgeneric map-vertices (result-type function graph))


(defgeneric map-edges (result-type function graph))


(defgeneric map-neighbors (result-type function graph vertex))


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


(defgeneric all-colored-p (graph color)
  (:method (graph color)
    (all-vertices (lambda (vertex)
                    (= color (color graph vertex)))
                  graph)))
