(in-package :liebler)


(defclass colored-graph (child-graph)
  ((vertex-colors
     :reader vertex-colors
     :initarg :vertex-colors
     :initform (make-hash-table :test #'equalp))))


(defmethod color ((graph colored-graph) vertex)
  (gethash vertex (vertex-colors graph) 0))


(defmethod (setf color) (new-value (graph colored-graph) vertex)
  (if (zerop new-value)
    (remhash vertex (vertex-colors graph))
    (setf (gethash vertex (vertex-colors graph)) new-value)))


(defmethod color-graph (graph count &key color)
  (let ((colored-graph (make-instance 'colored-graph :parent-graph graph)))
    (when (and color
               (not (equal 0 color)))
      (map-vertices nil
                    (lambda (vertex)
                      (setf (color colored-graph vertex)
                            (if (functionp color)
                              (funcall color vertex)
                              color)))
                    graph))
    colored-graph))



