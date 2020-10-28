(in-package #:liebler)


(defclass modular-product ()
  ((graphs
    :reader graphs
    :initarg :graphs
    :initform nil)))


(defun modular-product (&rest graphs)
  (make-instance 'modular-product :graphs graphs))


(defmethod directedp ((graph modular-product))
  (some #'directedp (graphs graph)))


(defmethod neighborp ((graph modular-product) vertex1 vertex2)
  (reduce (lambda (x y)
            (or (and x y)
                (and (not x) (not y))))
          (mapcar #'neighborp (graphs graph) vertex1 vertex2)))


(defmethod order ((graph modular-product))
  (apply #'* (mapcar #'order (graphs graph))))


(defclass modular-product-vertex-iterator ()
  ((iterators
     :reader iterators
     :initarg :iterators)))


(defmethod vertices ((graph modular-product))
  (make-instance 'modular-product-vertex-iterator :iterators (mapcar #'vertices (graphs graph))))


(defmethod valid ((iterator modular-product-vertex-iterator))
  (every #'valid (iterators iterator)))


(defmethod current ((iterator modular-product-vertex-iterator))
  (mapcar #'current (iterators iterator)))


(defmethod advance ((iterator modular-product-vertex-iterator))
  (do ((it (iterators iterator) (cdr it)))
      ((null it) nil)
    (advance (car it))
    (when (valid (car it))
      (return t))
    (when (cdr it)
      (reset (car it)))))


(defmethod reset ((iterator modular-product-vertex-iterator))
  (dolist (it (iterators iterator) (values))
    (reset it)))
