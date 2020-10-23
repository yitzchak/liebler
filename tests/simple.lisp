(ql:quickload :liebler)

(defparameter g (liebler:make-adjacency-matrix 6
                                               :edges '((0 . 1) (0 . 4) (1 . 4) (1 . 2)
                                                        (2 . 3) (3 . 4) (3 . 5))))


(defparameter h (liebler:make-adjacency-matrix 6
                                               :edges '((0 . 1) (0 . 4) (1 . 4) (1 . 2)
                                                        (2 . 3) (3 . 5))))


(defparameter p (liebler::modular-product g h))


(defun extract (graph)
  (remove nil
          (liebler:map-vertices 'list
                                (lambda (v)
                                  (unless (zerop (liebler:color graph v)) v))
                                graph)))
