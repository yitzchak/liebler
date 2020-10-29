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


