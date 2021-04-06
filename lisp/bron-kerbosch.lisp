(in-package #:liebler)


; color 0 is not in clique, 1 is in R, 2 is in P, 3 is in X
(defun bron-kerbosch-1 (graph)
  (if (notany (lambda (vertex)
                (> (color graph vertex) 1))
              (vertices graph))
    (list graph)
    (apply #'nconc
          (map 'list
               (lambda (vertex)
                 (when (= (color graph vertex) 2)
                   (prog1
                     (bron-kerbosch-1
                       (color-graph graph 4
                                    :color (lambda (v)
                                             (cond
                                               ((equalp v vertex)
                                                 1)
                                               ((and (> (color graph v) 1)
                                                     (not (neighborp graph vertex v)))
                                                 0)
                                               (t
                                                 (color graph v))))))
                     (setf (color graph vertex) 3))))
               (vertices graph)))))


(defun bron-kerbosch-1-queue (graph)
  (do* ((graphs (list (color-graph graph 4 :color 2)))
        (g (pop graphs) (pop graphs))
        results)
       ((null g) results)
    (if (notany (lambda (vertex)
                  (> (color g vertex) 1))
                (vertices g))
      (push g results)
      (sequence:dosequence (vertex (vertices g))
        (when (= (color g vertex) 2)
          (push (color-graph g 4
                             :color (lambda (v &aux (color (color g v)))
                                       (cond
                                        ((equalp v vertex)
                                          1)
                                        ((and (> color 1)
                                              (not (neighborp g vertex v)))
                                          0)
                                        (t
                                          color))))
                graphs)
          (setf (color g vertex) 3))))))


(defun pivot-gpx (graph)
  (do! (u d
        (v (vertices graph) :sequence))
       (nil u)
    (when (> (color graph v) 1)
      (let ((deg (degree graph v)))
        (when (or (not d) (< d deg))
          (setf u v
                d deg))))))


(defun bron-kerbosch-2-queue (graph)
  (do* ((graphs (list (color-graph graph 4 :color 2)))
        (g (pop graphs) (pop graphs))
        results)
       ((null g) results)
    (if (notany (lambda (vertex)
                  (> (color g vertex) 1))
                (vertices g))
        (push g results)
        (do! ((u (pivot-gpx g))
              (vertex (vertices g) :sequence))
             ()
          (when (and (= (color g vertex) 2)
                     (not (neighborp g vertex u)))
            (push (color-graph g 4
                               :color (lambda (v)
                                        (cond
                                          ((equalp v vertex)
                                            1)
                                          ((and (> (color g v) 1)
                                                (not (neighborp g vertex v)))
                                            0)
                                          (t
                                            (color g v)))))
                  graphs)
            (setf (color g vertex) 3))))))


(defun bron-kerbosch (graph)
  (bron-kerbosch-1 (color-graph graph 4 :color 2)))
