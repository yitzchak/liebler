(in-package #:liebler)


#|(defmethod degree (graph vertex)
  (reduce-neighbors (lambda (previous neighbor)
                      (+ previous
                         (if (equal vertex neighbor) 2 1)))
                    graph
                    vertex
                    0))


(defmethod ex-degree (graph vertex)
  (reduce-neighbors (lambda (previous neighbor)
                      (+ previous
                         (degree neighbor)))
                    graph
                    vertex
                    0))


(defmethod color-by-degree (graph)
  (let ((result (color-graph graph (order graph))))
    (do-edges (vertex1 vertex2 graph result)
      (incf (color result vertex1))
      (incf (color result vertex2)))))


(defmethod color-by-ex-degree (graph)
  (let ((degrees (color-by-degree graph))
        (result (color-graph graph (order graph))))
    (do-edges (vertex1 vertex2 graph result)
      (incf (color result vertex1) (degree degrees vertex2))
      (incf (color result vertex2) (degree degrees vertex1)))))|#


(defclass edge-sequence ()
  ((parent
     :accessor parent
     :initarg :parent)))


(defmethod edges (graph)
  (make-instance 'edge-sequence :parent graph))


(defmethod sequence:make-sequence-iterator ((sequence edge-sequence) &key from-end start end)
  (multiple-value-bind (iterator limit from-end step endp element setf-element index copy)
                       (make-sequences-iterator (list (vertices (parent sequence)) (vertices (parent sequence)))
                                                :from-end from-end :start start :end end)
    (tagbody
     repeat
      (unless (or (funcall endp sequence iterator limit from-end)
                  (apply #'neighborp (parent sequence) (funcall element sequence iterator)))
        (setf iterator (funcall step sequence iterator from-end))
        (go repeat)))
    (values iterator
            limit
            from-end
            (lambda (sequence iterator from-end)
              (tagbody
               repeat
                (setf iterator (funcall step sequence iterator from-end))
                (unless (or (funcall endp sequence iterator limit from-end)
                            (apply #'neighborp (parent sequence) (funcall element sequence iterator)))
                  (go repeat)))
              iterator)
            endp
            element
            setf-element
            index
            copy)))


