(ql:quickload :liebler)

(defparameter g (liebler:make-adjacency-matrix 6
                                               :edges '((0 . 1) (0 . 4) (1 . 4) (1 . 2)
                                                        (2 . 3) (3 . 4) (3 . 5))))



(defparameter a (liebler:bron-kerbosch g))


(defun extract (graph)
  (remove nil
          (liebler:map-vertices 'list
                                (lambda (v c)
                                  (unless (zerop c) v))
                                graph)))
