(in-package #:liebler)


(defclass product ()
  ((graphs
     :reader graphs
     :initarg :graphs
     :initform nil)
   (vertices
     :reader vertices)))


(defmethod directedp ((graph product))
  (some #'directedp (graphs graph)))


(defmethod order ((graph product))
  (apply #'* (mapcar #'order (graphs graph))))


(defclass product-vertices (sequence standard-object)
  ((parent
     :reader parent
     :initarg :parent)))


(defmethod initialize-instance :after ((instance product) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (slot-value instance 'vertices)
        (make-instance 'product-vertices :parent instance)))


(defmethod sequence:length ((instance product-vertices))
  (order (parent instance)))


(defmethod sequence:elt ((instance product-vertices) index)
  (do ((sequences (mapcar #'vertices (graphs (parent instance))))
       result pos)
      ((null sequences) (nreverse result))
    (multiple-value-setq (index pos)
                         (floor index (length (car sequences))))
    (push (elt (car sequences) pos) result)
    (pop sequences)))


(defmethod sequence:make-sequence-iterator ((sequence product-vertices) &key from-end start end)
  (make-sequences-iterator (mapcar #'vertices (graphs (parent sequence)))))


(defclass modular-product (product)
  ())


(defun modular-product (&rest graphs)
  (make-instance 'modular-product :graphs graphs))


(defmethod neighborp ((graph modular-product) vertex1 vertex2)
  (reduce (lambda (x y)
            (or (and x y)
                (and (not x) (not y))))
          (mapcar #'neighborp (graphs graph) vertex1 vertex2)))


(defclass tensor-product (product)
  ())


(defun tensor-product (&rest graphs)
  (make-instance 'tensor-product :graphs graphs))


(defmethod neighborp ((graph tensor-product) vertex1 vertex2)
  (some #'neighborp (graphs graph) vertex1 vertex2))



