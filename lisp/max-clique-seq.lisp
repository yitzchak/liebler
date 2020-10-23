(in-package :liebler)

(defun color-min (v kmin)
  (do* ((u (color-graph v 1024 :color 0))
       (v1 (color-graph v 2 :color 0))
       (vcopy (copy-colored-graph v1) (copy-colored-graph v1))
       (k 1 (1+ k)))
      ((all-colored-p v1 1) u)
    (map-vertices nil
                  (lambda (vertex)
                    (when (zerop (color vcopy vertex))
                      (setf (color v1 vertex) 1)
                      (map-vertices nil (lambda (vertex2)
                                          (unless (neighborp vcopy vertex vertex2)
                                            (setf (color vcopy vertex2) 1)))
                                        vcopy)
                      (when (> k kmin)
                        (setf (color u vertex) k)))))))

  
