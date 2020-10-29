(in-package :liebler)


(defclass colored-graph (child-graph)
  ((vertex-colors
     :reader vertex-colors
     :initarg :vertex-colors
     :initform (make-hash-table :test #'equalp)))
   (colors
     :reader colors
     :initarg :colors))


(defmethod color ((graph colored-graph) vertex)
  (gethash vertex (vertex-colors graph) 0))


(defmethod (setf color) (new-value (graph colored-graph) vertex)
  (if (zerop new-value)
    (remhash vertex (vertex-colors graph))
    (setf (gethash vertex (vertex-colors graph)) new-value)))


(defmethod color-graph (graph count &key color)
  (let ((colored-graph (make-instance 'colored-graph :parent-graph graph :colors count)))
    (cond
      ((functionp color)
        (do-vertices (vertex colored-graph)
          (setf (color colored-graph vertex) (funcall color vertex))))
      ((and color
            (not (zerop color)))
        (do-vertices (vertex colored-graph)
          (setf (color colored-graph vertex) color))))
    colored-graph))



