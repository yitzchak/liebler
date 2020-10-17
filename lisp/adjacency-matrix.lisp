(in-package #:liebler)


(defclass adjacency-matrix ()
  ((directedp
     :reader directedp
     :initarg :directedp)
   (order
     :reader order
     :initarg :order)
   (matrix
     :reader matrix
     :initarg :matrix)))


(defun make-adjacency-matrix (order &key directedp edges)
  (let ((matrix (make-array (list order order) :element-type 'bit)))
    (dolist (edge edges (make-instance 'adjacency-matrix :order order :matrix matrix))
      (setf (aref matrix (car edge) (cdr edge)) 1)
      (unless (or directedp
                  (= (car edge) (cdr edge)))
        (setf (aref matrix (cdr edge) (car edge)) 1)))))


(defmethod neighborp ((graph adjacency-matrix) vertex1 vertex2)
  (not (zerop (aref (matrix graph) vertex1 vertex2))))


(defmethod (setf neighborp) (new-value (graph adjacency-matrix) vertex1 vertex2)
  (let ((bit (if new-value 1 0)))
    (setf (aref (matrix graph) vertex1 vertex2) bit)
    (unless (or (directedp graph)
                (= vertex1 vertex2))
      (setf (aref (matrix graph) vertex2 vertex1) bit))
    new-value))


(defmethod map-vertices (result-type function (graph adjacency-matrix))
  (cond
    ((null result-type)
      (dotimes (vertex (order graph))
        (funcall function vertex)))
    ((subtypep result-type 'list)
      (loop :for vertex :below (order graph)
            :collect (funcall function vertex)))
    ((subtypep result-type 'vector)
      (let ((result (make-array (order graph) :element-type (if (and (listp result-type)
                                                                     (not (eql '* (second result-type))))
                                                              (second result-type)
                                                              t))))
        (dotimes (vertex (order graph) result)
          (setf (aref result vertex) (funcall function vertex)))))
    (t)))


(defclass colored-adjacency-matrix (adjacency-matrix)
  ((vertex-colors
     :reader vertex-colors
     :initarg :vertex-colors)))


(defmethod color-graph ((graph adjacency-matrix) count &key color)
  (make-instance 'colored-adjacency-matrix
                 :matrix (matrix graph)
                 :order (order graph)
                 :vertex-colors (liebler:map-vertices (list 'vector (list 'integer 0 (1- count)))
                                                      (if (functionp color)
                                                        color
                                                        (lambda (vertex)
                                                          (declare (ignore vertex))
                                                          color))
                                                      graph)))

(defmethod color ((graph colored-adjacency-matrix) vertex)
  (aref (vertex-colors graph) vertex))


(defmethod (setf color) (new-value (graph colored-adjacency-matrix) vertex)
  (setf (aref (vertex-colors graph) vertex) new-value))



