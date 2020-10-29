(in-package :liebler)


(defun colorize (v &optional (kmin 0))
  (do* ((u (color-graph v (1+ (order v)) :color 0))
        (v1 (color-graph v 2 :color 0))
        (vcopy (copy-colored-graph v1) (copy-colored-graph v1))
        (k 1 (1+ k)))
       ((all-colored-p v1 1) u)
    (print "foo")
    (print u)
    (print (map-vertices 'list (lambda (v) (cons v (color u v))) u))
    (print v1)
    (print (map-vertices 'list (lambda (v) (cons v (color v1 v))) v1))
    (print vcopy)
    (print (map-vertices 'list (lambda (v) (cons v (color vcopy v))) vcopy))
    (do-vertices (vertex vcopy)
      (when (zerop (color vcopy vertex))
        (setf (color v1 vertex) 1
              (color vcopy vertex) 1)
        (do-neighbors (neighbor vcopy vertex)
          (setf (color vcopy neighbor) 1))
        (when (> k kmin)
          (setf (color u vertex) k))))))


