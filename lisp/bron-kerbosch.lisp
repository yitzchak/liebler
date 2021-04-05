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


#|defun bron-kerbosch-1-queue (graph)
  (prog ((graphs (list (color-graph graph 4 :color 2)))
         g results
         (steps 0))
   repeat
    (when (zerop (setf steps (mod (1+ steps) 100)))
      (format t "~A ~A~%" (length results) (length graphs))
      (finish-output))
    (setf g (pop graphs))
    (cond
      ((notany-vertices (lambda (vertex)
                          (> (color g vertex) 1))
                        g)
        (push g results))
      (t
        (map-vertices
          nil
          (lambda (vertex)
            (when (= (color g vertex) 2)
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
              (setf (color g vertex) 3)))
          g)))
      (when graphs
       (go repeat))
      (return results)))


(defun pivot-gpx (graph)
  (let (u d)
    (map-vertices nil
                  (lambda (v)
                    (when (> (color graph v) 1)
                      (let ((deg (degree graph v)))
                        (when (or (not d) (< d deg))
                          (setf u v
                                d deg)))))
                  graph)
    u))


(defun bron-kerbosch-2-queue (graph)
  (prog ((graphs (list (color-graph graph 4 :color 2)))
         g results
         (steps 0))
   repeat
    (when (zerop (setf steps (mod (1+ steps) 100)))
      (format t "~A ~A~%" (length results) (length graphs))
      (finish-output))
    (setf g (pop graphs))
    (cond
      ((notany-vertices (lambda (vertex)
                          (> (color g vertex) 1))
                        g)
        (push g results))
      (t
        (let ((u (pivot-gpx g)))
          (map-vertices
            nil
            (lambda (vertex)
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
                (setf (color g vertex) 3)))
            g))))
      (when graphs
       (go repeat))
      (return results)))||#

(defun bron-kerbosch (graph)
  (bron-kerbosch-1 (color-graph graph 4 :color 2)))
