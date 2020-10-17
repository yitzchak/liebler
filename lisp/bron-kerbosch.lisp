(in-package #:liebler)


; color 0 is not in clique, 1 is in R, 2 is in P, 3 is in X
(defun bron-kerbosch-1 (graph)
  (if (notany-vertices (lambda (vertex)
                         (> (color graph vertex) 1))
                       graph)
    (list graph)
    (apply #'nconc
           (map-vertices
             'list
             (lambda (vertex)
               (when (= (color graph vertex) 2)
                 (prog1
                   (bron-kerbosch-1
                     (color-graph graph 4
                                  :color (lambda (v)
                                           (cond
                                             ((= v vertex)
                                               1)
                                             ((and (> (color graph v) 1)
                                                   (not (neighborp graph vertex v)))
                                               0)
                                             (t
                                               (color graph v))))))
                   (setf (color graph vertex) 3))))
             graph))))


(defun bron-kerbosch (graph)
  (bron-kerbosch-1 (color-graph graph 4 :color 2)))
