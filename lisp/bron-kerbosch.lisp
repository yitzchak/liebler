(in-package #:liebler)


; color 0 is not in clique, 1 is in R, 2 is in P, 3 is in X
(defun bron-kerbosch-1 (graph)
  (if (zerop (count-vertices (lambda (vertex color)
                               (declare (ignore vertex))
                               (> color 1))
                             graph))
    (list graph)
    (apply #'nconc
           (map-vertices
             'list
             (lambda (vertex color)
               (when (= color 2)
                 (prog1
                   (bron-kerbosch-1
                     (color-graph graph
                                  :colors 4
                                  :color-function (lambda (v c)
                                                    (cond
                                                      ((= v vertex)
                                                        1)
                                                      ((and (> c 1)
                                                            (not (neighborp graph vertex v)))
                                                        0)
                                                      (t
                                                        c)))))
                   (setf (color graph vertex) 3))))
             graph))))


(defun bron-kerbosch (graph)
  (bron-kerbosch-1 (color-graph graph
                                :colors 4
                                :color-function (lambda (vertex color)
                                                  (declare (ignore vertex color))
                                                  2))))
