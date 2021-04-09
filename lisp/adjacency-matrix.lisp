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
     :initarg :matrix)
   (vertices
     :reader vertices)
   (edges
     :reader edges)))


(defmethod neighborp ((graph adjacency-matrix) vertex1 vertex2)
  (= 1 (aref (matrix graph) vertex1 vertex2)))


(defclass adjacency-matrix-vertices (sequence standard-object)
  ((parent
     :reader parent
     :initarg :parent)))


(defmethod sequence:length ((instance adjacency-matrix-vertices))
  (order (parent instance)))


(defmethod sequence:elt ((instance adjacency-matrix-vertices) index)
  (declare (ignore instance))
  index)


(defun adjacency-matrix-vertices-step (sequence iterator from-end)
  (declare (ignore sequence))
  (if from-end
    (decf iterator)
    (incf iterator)))


(defun adjacency-matrix-vertices-endp (sequence iterator limit from-end)
  (declare (ignore sequence from-end))
  (= limit iterator))


(defun adjacency-matrix-vertices-element (sequence iterator)
  (declare (ignore sequence))
  iterator)


(defun adjacency-matrix-vertices-setf-element (new-value sequence iterator)
  (declare (ignore new-value sequence iterator))
  (error "Cannot set the vertex value for an adjacency matrix"))


(defun adjacency-matrix-vertices-index (sequence iterator)
  (declare (ignore sequence))
  iterator)


(defun adjacency-matrix-vertices-copy (sequence iterator)
  (declare (ignore sequence))
  iterator)


(defmethod sequence:make-sequence-iterator ((sequence adjacency-matrix-vertices) &key from-end start end)
  (values (if from-end
            (or end (order (parent sequence)))
            (or start 0))
          (if from-end
            (or start 0)
            (or end (order (parent sequence))))
          from-end
          #'adjacency-matrix-vertices-step
          #'adjacency-matrix-vertices-endp
          #'adjacency-matrix-vertices-element
          #'adjacency-matrix-vertices-setf-element
          #'adjacency-matrix-vertices-index
          #'adjacency-matrix-vertices-copy))


(defclass adjacency-matrix-edges (sequence standard-object)
  ((parent
     :reader parent
     :initarg :parent)))


(defun adjacency-matrix-edges-find-next-edge (graph iterator-state)
  (prog ((index (first iterator-state))
         (vertex1 (second iterator-state))
         (vertex2 (third iterator-state)))
   next
    (when (< vertex1 (order graph))
      (incf vertex2)
      (unless (or (and (not (directedp graph))
                       (<= vertex2 vertex1))
                  (and (directedp graph)
                       (< vertex2 (order graph))))
        (incf vertex1)
        (setf vertex2 -1)
        (go next))
      (when (zerop (aref (matrix graph) vertex1 vertex2))
        (go next)))
    (return (list (1+ index) vertex1 vertex2))))


(defmethod sequence:length ((instance adjacency-matrix-edges))
  (do ((len 0)
       (vertex1 0 (1+ vertex1)))
      ((= (order (parent instance)) vertex1)
       len)
    (dotimes (vertex2 (if (directedp (parent instance))
                        (order (parent instance))
                        (1+ vertex1)))
      (unless (zerop (aref (matrix (parent instance)) vertex1 vertex2))
        (incf len)))))


(defmethod sequence:elt ((instance adjacency-matrix-edges) index)
  (do ((iterator (adjacency-matrix-edges-find-next-edge (parent instance) (list -1 0 -1))
                 (adjacency-matrix-edges-find-next-edge (parent instance) iterator)))
      ((= (car iterator) index) (cdr iterator))))


(defun adjacency-matrix-edges-step (sequence iterator from-end)
  (declare (ignore from-end))
  (adjacency-matrix-edges-find-next-edge (parent sequence) iterator))


(defun adjacency-matrix-edges-endp (sequence iterator limit from-end)
  (declare (ignore sequence from-end))
  (equalp limit (cdr iterator)))


(defun adjacency-matrix-edges-element (sequence iterator)
  (declare (ignore sequence))
  (cdr iterator))


(defun adjacency-matrix-edges-setf-element (new-value sequence iterator)
  (declare (ignore new-value sequence iterator))
  (error "Cannot set the edge value for an adjacency matrix"))


(defun adjacency-matrix-edges-index (sequence iterator)
  (declare (ignore sequence))
  (car iterator))


(defun adjacency-matrix-edges-copy (sequence iterator)
  (declare (ignore sequence))
  (copy-list iterator))


(defmethod sequence:make-sequence-iterator ((sequence adjacency-matrix-edges) &key from-end start end)
  (values (adjacency-matrix-edges-find-next-edge (parent sequence) (list -1 0 -1))
          (list (order (parent sequence)) -1)
          from-end
          #'adjacency-matrix-edges-step
          #'adjacency-matrix-edges-endp
          #'adjacency-matrix-edges-element
          #'adjacency-matrix-edges-setf-element
          #'adjacency-matrix-edges-index
          #'adjacency-matrix-edges-copy))


(defclass adjacency-matrix-neighbors (sequence standard-object)
  ((parent
     :reader parent
     :initarg :parent)
   (vertex
     :reader vertex
     :initarg :vertex)))


(defun adjacency-matrix-neighbors-find-next-edge (instance iterator)
  (prog ((index (first iterator))
         (vertex2 (second iterator))
         (vertex (vertex instance))
         (order (order (parent instance)))
         (matrix (matrix (parent instance))))
   next
    (when (< vertex2 order)
      (incf vertex2)
      (when (and (< vertex2 order)
                 (zerop (aref matrix vertex vertex2)))
        (go next)))
    (return (list (1+ index) vertex2))))


(defmethod sequence:length ((instance adjacency-matrix-neighbors))
  (let ((len 0))
    (dotimes (vertex2 (order (parent instance)) len)
      (unless (zerop (aref (matrix (parent instance)) (vertex instance) vertex2))
        (incf len)))))


(defmethod sequence:elt ((instance adjacency-matrix-neighbors) index)
  (do ((iterator (adjacency-matrix-neighbors-find-next-edge instance (list -1 -1))
                 (adjacency-matrix-neighbors-find-next-edge instance iterator)))
      ((= (car iterator) index) (second iterator))))


(defun adjacency-matrix-neighbors-step (sequence iterator from-end)
  (declare (ignore from-end))
  (adjacency-matrix-neighbors-find-next-edge sequence iterator))


(defun adjacency-matrix-neighbors-endp (sequence iterator limit from-end)
  (declare (ignore sequence from-end))
  (= limit (second iterator)))


(defun adjacency-matrix-neighbors-element (sequence iterator)
  (declare (ignore sequence))
  (second iterator))


(defun adjacency-matrix-neighbors-setf-element (new-value sequence iterator)
  (declare (ignore new-value sequence iterator))
  (error "Cannot set the edge value for an adjacency matrix"))


(defun adjacency-matrix-neighbors-index (sequence iterator)
  (declare (ignore sequence))
  (first iterator))


(defun adjacency-matrix-neighbors-copy (sequence iterator)
  (declare (ignore sequence))
  (copy-list iterator))


(defmethod sequence:make-sequence-iterator ((sequence adjacency-matrix-neighbors) &key from-end start end)
  (values (adjacency-matrix-neighbors-find-next-edge sequence (list -1 -1))
          (order (parent sequence))
          from-end
          #'adjacency-matrix-neighbors-step
          #'adjacency-matrix-neighbors-endp
          #'adjacency-matrix-neighbors-element
          #'adjacency-matrix-neighbors-setf-element
          #'adjacency-matrix-neighbors-index
          #'adjacency-matrix-neighbors-copy))


(defmethod neighbors ((instance adjacency-matrix) vertex)
  (make-instance 'adjacency-matrix-neighbors :parent instance :vertex vertex))



(defmethod initialize-instance :after ((instance adjacency-matrix) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (slot-value instance 'vertices)
        (make-instance 'adjacency-matrix-vertices :parent instance)
        (slot-value instance 'edges)
        (make-instance 'adjacency-matrix-edges :parent instance))
  instance)


(defmethod degree ((graph adjacency-matrix) vertex)
  (do ((other-vertex 0 (1+ other-vertex))
       (order (order graph))
       (degree 0))
      ((>= other-vertex order) degree)
    (unless (zerop (aref (matrix graph) vertex other-vertex))
      (incf degree (if (= vertex other-vertex) 2 1)))))


(defun make-adjacency-matrix (order &key directedp edges)
  (let ((matrix (make-array (list order order) :element-type 'bit)))
    (dolist (edge edges (make-instance 'adjacency-matrix :order order :matrix matrix))
      (setf (aref matrix (car edge) (cdr edge)) 1)
      (unless (or directedp
                  (= (car edge) (cdr edge)))
        (setf (aref matrix (cdr edge) (car edge)) 1)))))


#|(defstruct adjacency-matrix-vertex-iterator
  graph
  (vertex 0))


(defmethod khazern:make-iterator ((instance adjacency-matrix) (type (eql :vertex)))
  (make-adjacency-matrix-vertex-iterator :graph instance))


(defmethod khazern:pop-head ((instance adjacency-matrix-vertex-iterator) &optional index)
  (with-slots (graph vertex)
              instance
    (cond
      ((< vertex (order graph))
        (prog1
          vertex
          (incf vertex)))
      (values))))


(defmethod khazern:head ((instance adjacency-matrix-vertex-iterator) &optional index)
  (with-slots (graph vertex)
              instance
    (if (< vertex (order graph))
      vertex
      (values))))


(defmethod khazern:emptyp ((instance adjacency-matrix-vertex-iterator))
  (>= (adjacency-matrix-vertex-iterator-vertex instance) (order (adjacency-matrix-vertex-iterator-graph instance))))


(defstruct adjacency-matrix-edge-iterator
  graph
  (vertex1 0)
  (vertex2 -1))


(defun find-next-edge (iterator)
  (with-slots (graph vertex1 vertex2)
              iterator
    (tagbody
     next
      (when (< vertex1 (order graph))
        (incf vertex2)
        (unless (or (and (not (directedp graph))
                         (<= vertex2 vertex1))
                    (and (directedp graph)
                         (< vertex2 (order graph))))
          (incf vertex1)
          (setf vertex2 -1)
          (go next))
        (when (zerop (aref (matrix graph) vertex1 vertex2))
          (go next))))
    iterator))


(defmethod khazern:make-iterator ((instance adjacency-matrix) (type (eql :edge)))
  (find-next-edge (make-adjacency-matrix-edge-iterator :graph instance)))


(defmethod khazern:pop-head ((instance adjacency-matrix-edge-iterator) &optional index)
  (with-slots (graph vertex1 vertex2)
              instance
    (cond
      ((< vertex1 (order graph))
        (prog1
          (case index
            (0 vertex1)
            (1 vertex2)
            (otherwise (list vertex1 vertex2)))
          (find-next-edge instance)))
      (t
        (values)))))


(defmethod khazern:head ((instance adjacency-matrix-edge-iterator) &optional index)
  (with-slots (graph vertex1 vertex2)
              instance
    (if (< vertex1 (order graph))
      (case index
        (0 vertex1)
        (1 vertex2)
        (otherwise (list vertex1 vertex2)))
      (values))))


(defmethod khazern:emptyp ((instance adjacency-matrix-edge-iterator))
  (>= (adjacency-matrix-edge-iterator-vertex1 instance) (order (adjacency-matrix-edge-iterator-graph instance))))|#



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
                 :vertex-colors (map (list 'vector (list 'integer 0 (1- count)))
                                     (if (functionp color)
                                       color
                                       (lambda (vertex)
                                         (declare (ignore vertex))
                                         (or color 0)))
                                      (vertices graph))))


(defmethod color-graph ((graph colored-adjacency-matrix) count &key color)
  (make-instance 'colored-adjacency-matrix
                 :parent-graph (parent-graph graph)
                 :colors count
                 :vertex-colors (map (list 'vector (list 'integer 0 (1- count)))
                                     (if (functionp color)
                                       color
                                       (lambda (vertex)
                                         (declare (ignore vertex))
                                         (or color 0)))
                                     (vertices graph))))


(defmethod color ((graph colored-adjacency-matrix) vertex)
  (aref (vertex-colors graph) vertex))


(defmethod (setf color) (new-value (graph colored-adjacency-matrix) vertex)
  (setf (aref (vertex-colors graph) vertex) new-value))


(defmethod copy-colored-graph ((graph colored-adjacency-matrix))
  (make-instance 'colored-adjacency-matrix
                 :parent-graph (parent-graph graph)
                 :colors (colors graph)
                 :vertex-colors (copy-seq (vertex-colors graph))))
