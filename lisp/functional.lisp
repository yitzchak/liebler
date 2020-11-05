(in-package #:liebler)


(defun map-iterator (result-type function iterator)
  (cond
    ((null result-type)
      (tagbody
       repeat
        (when (validp iterator)
          (multiple-value-call function (current iterator))
          (advance iterator)
          (go repeat))))
    ((subtypep result-type 'list)
      (prog (result)
       repeat
        (when (validp iterator)
          (push (multiple-value-call function (current iterator))
                result)
          (advance iterator)
          (go repeat))
        (return (nreverse result))))
    ((subtypep result-type 'vector)
      (prog ((result (make-array 32
                                 :adjustable t
                                 :fill-pointer 0
                                 :element-type (if (and (listp result-type)
                                                        (not (eql '* (second result-type))))
                                                 (second result-type)
                                                 t))))
       repeat
        (when (validp iterator)
          (vector-push-extend
            (multiple-value-call function (current iterator))
            result)
          (advance iterator)
          (go repeat))
        (return result)))
    (t
      (error "Unknown sequence type of ~s." result-type))))


(defun map-vertices (result-type function graph)
  (map-iterator result-type function (vertices graph)))


(defun map-edges (result-type function graph)
  (map-iterator result-type function (edges graph)))


(defun map-neighbors (result-type function graph vertex)
  (map-iterator result-type function (neighbors graph vertex)))


(defun reduce-iterator (function iterator initial-value)
  (prog ((result initial-value))
   repeat
    (unless (validp iterator)
      (return result))
    (setf result (multiple-value-call function result (current iterator)))
    (advance iterator)
    (go repeat)))


(defun reduce-vertices (function graph initial-value)
  (reduce-iterator function (vertices graph) initial-value))


(defun reduce-neighbors (function graph vertex initial-value)
  (reduce-iterator function (neighbors graph vertex) initial-value))


(defun reduce-edges (function graph initial-value)
  (reduce-iterator function (edges graph) initial-value))


(defun count-colored-vertices (graph)
  (count-vertices (lambda (vertex)
                    (not (zerop (color graph vertex))))
                  graph))


(defun count-vertices (predicate graph)
  (reduce-vertices (lambda (previous vertex)
                     (if (funcall predicate vertex)
                      (1+ previous)
                      previous))
                   graph
                   0))


(defun all-vertices (predicate graph)
  (do-vertices (vertex graph t)
    (unless (funcall predicate vertex)
      (return nil))))


(defun notall-vertices (predicate graph)
  (do-vertices (vertex graph nil)
    (unless (funcall predicate vertex)
      (return t))))


(defun some-vertices (predicate graph)
  (do-vertices (vertex graph nil)
    (when (funcall predicate vertex)
      (return t))))


(defun notany-vertices (predicate graph)
  (do-vertices (vertex graph t)
    (when (funcall predicate vertex)
      (return nil))))


(defun all-colored-p (graph color)
  (all-vertices (lambda (vertex)
                  (= color (color graph vertex)))
                graph))

