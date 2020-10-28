(in-package #:liebler)


(defgeneric directedp (graph))


(defgeneric edges (graph))


(defgeneric neighbors (graph))


(defgeneric vertices (graph))


(defgeneric advance (iterator))


(defgeneric valid (iterator))


(defgeneric current (iterator))


(defgeneric reset (iterator))


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


(defun map-iterator (result-type function graph)
  (cond
    ((null result-type)
      (do ((iterator (vertices graph) (advance iterator)))
          ((not (valid iterator)) nil)
        (apply function (multiple-value-list (current iterator)))))
    ((subtypep result-type 'list)
      (do ((iterator (vertices graph) (advance iterator))
           result)
          ((not (valid iterator)) (nreverse result))
        (push (apply function (multiple-value-list (current iterator)))
              result)))
    ((subtypep result-type 'vector)
      (do ((iterator (vertices graph) (advance iterator))
           (result (make-array 32
                               :adjustable t
                               :element-type (if (and (listp result-type)
                                                      (not (eql '* (second result-type))))
                                               (second result-type)
                                               t))))
          ((not (valid iterator)) result)
        (vector-push-extend
          (apply function (multiple-value-list (current iterator)))
          result)))
    (t
      (error "Unknown sequence type of ~s." result-type))))


(defun map-vertices (result-type function graph)
  (map-iterator result-type function (vertices graph)))


(defun map-edges (result-type function graph)
  (map-iterator result-type function (edges graph)))


(defun map-neighbors (result-type function graph vertex)
  (map-iterator result-type function (neighbors graph)))


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


(defgeneric order-graph-by-color (graph))



