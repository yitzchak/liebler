(in-package #:liebler)


(defclass product ()
  ((graphs
    :reader graphs
    :initarg :graphs
    :initform nil)))


(defmethod directedp ((graph product))
  (some #'directedp (graphs graph)))


(defmethod order ((graph product))
  (apply #'* (mapcar #'order (graphs graph))))


(defclass product-vertex-iterator ()
  ((iterators
     :reader iterators
     :initarg :iterators)))


(defmethod vertices ((graph product))
  (make-instance 'product-vertex-iterator :iterators (mapcar #'vertices (graphs graph))))


(defmethod valid ((iterator product-vertex-iterator))
  (every #'valid (iterators iterator)))


(defmethod current ((iterator product-vertex-iterator))
  (mapcar #'current (iterators iterator)))


(defmethod advance ((iterator product-vertex-iterator))
  (do ((it (iterators iterator) (cdr it)))
      ((null it) nil)
    (advance (car it))
    (when (valid (car it))
      (return t))
    (when (cdr it)
      (reset (car it)))))


(defmethod reset ((iterator product-vertex-iterator))
  (dolist (it (iterators iterator) (values))
    (reset it)))


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



