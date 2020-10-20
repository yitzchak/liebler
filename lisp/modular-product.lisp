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


(defmethod map-vertices (result-type function (graph modular-product)))


;(defmethod map-edges (result-type function (graph modular-product)))


;(defmethod map-neighbors (result-type function (graph modular-product) vertex))


(defmethod neighborp ((graph modular-product) vertex1 vertex2)
  (reduce (lambda (x y)
            (or (and x y)
                (and (not x) (not y))))
          (mapcar #'neighborp (graphs graph) vertex1 vertex2)))


;(defmethod rank ((graph modular-product) vertex))


(defmethod order ((graph modular-product))
  (apply #'* (mapcar #'order (graphs graph))))


(defclass modular-product-vertex-iterator ()
  ((iterators
     :reader iterators)))


(defmethod vertices ((graph modular-product))
  (make-instance 'modular-product-vertex-iterator :iterators (mapcar #'vertices (graphs graph))))


(defmethod advance ((iterator modular-product-vertex-iterator))
  (prog ((it (iterators iterator)))
   next
    (unless it
      (return nil))
    (when (advance (car it))
      (return t))
    (reset (car it))
    (setf it (cdr it))
    (go next)))


(defmethod reset ((iterator modular-product-vertex-iterator))
  (dolist (it (iterators iterator) (values))
    (reset it)))
