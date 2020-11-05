(in-package :liebler)


(defun colorize (v &optional (kmin 0))
  (do* ((u (color-graph v (1+ (order v)) :color 0))
        (v1 (color-graph v 2 :color 0))
        (vcopy (copy-colored-graph v1) (copy-colored-graph v1))
        (k 1 (1+ k)))
       ((all-colored-p v1 1) u)
    (do-vertices (vertex vcopy)
      (when (zerop (color vcopy vertex))
        (setf (color v1 vertex) 1
              (color vcopy vertex) 1)
        (do-neighbors (neighbor vcopy vertex)
          (setf (color vcopy neighbor) 1))
        (when (> k kmin)
          (setf (color u vertex) k))))))


(defun max-clique-seq (graph)
  (expand (color-graph graph 2 :color 1)
          (order-graph-by-color (color-by-ex-degree graph) #'>)
          (color-graph graph 2)
          (color-graph graph 2)))


(defun expand (v u c cmax)
  (do-vertices (vertex u)
    (unless (zerop (color u vertex))
      (setf (color v vertex) 0)
      (let ((diff (- (count-colored-vertices cmax)
                     (count-colored-vertices c)))
            (new-v (color-graph v 2 :color (lambda (other-vertex)
                                             (if (and (not (zerop (color v other-vertex)))
                                                      (neighborp v vertex other-vertex))
                                               1
                                               0)))))
        (cond
          ((<= (color u vertex) diff))
          ((some-vertices (lambda (other-vertex)
                            (not (zerop (color new-v other-vertex))))
                          new-v)
            (expand new-v (colorize v diff) (color-graph c 2 :color (lambda (other-vertex)
                                                                    (if (equal other-vertex vertex)
                                                                      1
                                                                      (color c other-vertex))))
                                                                      cmax))
          ((>= 0 diff)
            (setf (color cmax vertex) 1)))))))
