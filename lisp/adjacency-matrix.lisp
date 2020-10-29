(in-package #:liebler)


(defclass adjacency-matrix ()
  ((directedp
     :reader directedp
     :initarg :directedp
     :initform nil)
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


(defclass adjacency-matrix-vertex-iterator ()
  ((current
     :accessor current)
   (graph
     :reader graph
     :initarg :graph)))


(defmethod initialize-instance :after ((iterator adjacency-matrix-vertex-iterator) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (reset iterator))


(defmethod vertices ((graph adjacency-matrix))
  (make-instance 'adjacency-matrix-vertex-iterator :graph graph))


(defmethod advance ((iterator adjacency-matrix-vertex-iterator))
  (with-slots (current)
              iterator
    (when current
      (unless (< (incf current) (order (graph iterator)))
        (setf current nil)))
    iterator))


(defmethod valid ((iterator adjacency-matrix-vertex-iterator))
  (and (current iterator) t))


(defmethod reset ((iterator adjacency-matrix-vertex-iterator))
  (setf (current iterator)
        (if (zerop (order (graph iterator)))
          nil
          0))
  iterator)


(defclass adjacency-matrix-edge-iterator ()
  ((vertex1
     :accessor vertex1)
   (vertex2
     :accessor vertex2)
   (graph
     :reader graph
     :initarg :graph)))


(defmethod initialize-instance :after ((iterator adjacency-matrix-edge-iterator) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (reset iterator))


(defmethod edges ((graph adjacency-matrix))
  (make-instance 'adjacency-matrix-edge-iterator :graph graph))


(defmethod current ((iterator adjacency-matrix-edge-iterator))
  (values (vertex1 iterator) (vertex2 iterator)))


(defmethod advance ((iterator adjacency-matrix-edge-iterator))
  (with-slots (vertex1 vertex2 graph)
              iterator
    (when vertex1
      (tagbody
       repeat
        (incf vertex1)
        (unless (or (<= vertex1 vertex2)
                    (and (directedp graph)
                         (< vertex1 (order graph))))
          (setf vertex1 0)
          (incf vertex2))
        (cond
          ((not (< vertex2 (order graph)))
            (setf vertex1 nil
                  vertex2 nil))
          ((not (neighborp graph vertex1 vertex2))
            (go repeat)))))
      iterator))


(defmethod valid ((iterator adjacency-matrix-edge-iterator))
  (and (vertex1 iterator) t))


(defmethod reset ((iterator adjacency-matrix-edge-iterator))
  (cond
    ((zerop (order (graph iterator)))
      (setf (vertex1 iterator) nil
            (vertex2 iterator) nil))
    (t
      (setf (vertex1 iterator) 0
            (vertex2 iterator) 0)
      (when (zerop (aref (matrix (graph iterator)) 0 0))
        (advance iterator))))
  iterator)


(defmethod neighborp ((graph adjacency-matrix) vertex1 vertex2)
  (not (zerop (aref (matrix graph) vertex1 vertex2))))


(defmethod (setf neighborp) (new-value (graph adjacency-matrix) vertex1 vertex2)
  (let ((bit (if new-value 1 0)))
    (setf (aref (matrix graph) vertex1 vertex2) bit)
    (unless (or (directedp graph)
                (= vertex1 vertex2))
      (setf (aref (matrix graph) vertex2 vertex1) bit))
    new-value))


(defclass colored-adjacency-matrix (child-graph)
  ((vertex-colors
     :reader vertex-colors
     :initarg :vertex-colors)
   (colors
     :reader colors
     :initarg :colors)))


(defmethod color-graph ((graph adjacency-matrix) count &key color)
  (make-instance 'colored-adjacency-matrix
                 :parent-graph graph
                 :colors count
                 :vertex-colors (liebler:map-vertices (list 'vector (list 'integer 0 (1- count)))
                                                      (if (functionp color)
                                                        color
                                                        (lambda (vertex)
                                                          (declare (ignore vertex))
                                                          (or color 0)))
                                                      graph)))


(defmethod color-graph ((graph colored-adjacency-matrix) count &key color)
  (make-instance 'colored-adjacency-matrix
                 :parent-graph (parent-graph graph)
                 :colors count
                 :vertex-colors (liebler:map-vertices (list 'vector (list 'integer 0 (1- count)))
                                                      (if (functionp color)
                                                        color
                                                        (lambda (vertex)
                                                          (declare (ignore vertex))
                                                          (or color 0)))
                                                      graph)))


(defmethod color ((graph colored-adjacency-matrix) vertex)
  (aref (vertex-colors graph) vertex))


(defmethod (setf color) (new-value (graph colored-adjacency-matrix) vertex)
  (setf (aref (vertex-colors graph) vertex) new-value))


(defmethod copy-colored-graph ((graph colored-adjacency-matrix))
  (make-instance 'colored-adjacency-matrix
                 :parent-graph (parent-graph graph)
                 :colors (colors graph)
                 :vertex-colors (copy-seq (vertex-colors graph))))
