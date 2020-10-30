(in-package #:liebler)


(defun map-iterator (result-type function iterator)
  (cond
    ((null result-type)
      (tagbody
       repeat
        (when (valid iterator)
          (multiple-value-call function (current iterator))
          (advance iterator)
          (go repeat))))
    ((subtypep result-type 'list)
      (prog (result)
       repeat
        (when (valid iterator)
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
        (when (valid iterator)
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


(defun count-vertices (predicate graph)
  (let ((count 0))
    (map-vertices nil
                  (lambda (vertex)
                    (when (funcall predicate vertex)
                      (incf count)))
                  graph)
    count))


(defun all-vertices (predicate graph)
  (catch 'query
    (map-vertices nil
                  (lambda (vertex)
                    (unless (funcall predicate vertex)
                      (throw 'query nil)))
                  graph)
    t))


(defun notall-vertices (predicate graph)
  (catch 'query
    (map-vertices nil
                  (lambda (vertex)
                    (unless (funcall predicate vertex)
                      (throw 'query t)))
                  graph)))


(defun some-vertices (predicate graph)
  (catch 'query
    (map-vertices nil
                  (lambda (vertex)
                    (when (funcall predicate vertex)
                      (throw 'query t)))
                  graph)))


(defun notany-vertices (predicate graph)
  (catch 'query
    (map-vertices nil
                  (lambda (vertex)
                    (when (funcall predicate vertex)
                      (throw 'query nil)))
                  graph)
    t))


(defun all-colored-p (graph color)
  (all-vertices (lambda (vertex)
                  (= color (color graph vertex)))
                graph))


(defun reduce-iterator (function iterator initial-value)
  (prog ((result initial-value))
   repeat
    (unless (valid iterator)
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
