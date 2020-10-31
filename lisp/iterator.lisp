(in-package #:liebler)


; iterators don't need to be derived from this class. This is just for convience.
(defclass iterator ()
  ((graph
     :reader graph
     :initarg :graph)))


(defgeneric edges (graph))


(defgeneric neighbors (graph vertex))


(defgeneric vertices (graph))


(defgeneric advance (iterator))


(defgeneric valid (iterator))


(defgeneric current (iterator))


(defgeneric reset (iterator))


(defclass child-iterator ()
  ((parent-iterator
     :reader parent-iterator
     :initarg :parent-iterator)))


(defmethod advance ((iterator child-iterator))
  (advance (parent-iterator iterator)))


(defmethod valid ((iterator child-iterator))
  (valid (parent-iterator iterator)))


(defmethod current ((iterator child-iterator))
  (current (parent-iterator iterator)))


(defmethod reset ((iterator child-iterator))
  (reset (parent-iterator iterator)))


(defclass neighbor-iterator (iterator child-iterator)
  ((vertex
     :reader vertex
     :initarg :vertex)))


(defmethod initialize-instance :after ((iterator neighbor-iterator) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (with-slots (parent-iterator graph vertex)
              iterator
    (when (and (valid parent-iterator)
               (not (neighborp graph vertex (current parent-iterator))))
      (advance parent-iterator))))


(defmethod neighbors (graph vertex)
  (make-instance 'neighbor-iterator
                 :graph graph
                 :parent-iterator (vertices graph)
                 :vertex vertex))


(defmethod advance ((iterator neighbor-iterator))
  (with-slots (parent-iterator graph vertex)
              iterator
    (tagbody
     repeat
      (advance parent-iterator)
      (when (and (valid parent-iterator)
                 (not (neighborp graph vertex (current parent-iterator))))
        (go repeat)))
    iterator))


(defmethod reset ((iterator neighbor-iterator))
  (with-slots (parent-iterator graph vertex)
              iterator
    (reset parent-iterator)
    (when (and (valid parent-iterator)
               (not (neighborp graph vertex (current parent-iterator))))
      (advance parent-iterator))))


(defclass edge-iterator (iterator)
  ((vertex-iterator
     :reader vertex-iterator
     :initarg :vertex-iterator)
   (neighbor-iterator
     :accessor neighbor-iterator
     :initform nil)))


(defmethod initialize-instance :after ((iterator edge-iterator) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (reset iterator))


(defmethod edges (graph)
  (make-instance 'edge-iterator
                 :graph graph
                 :vertex-iterator (vertices graph)))


(defmethod valid ((iterator edge-iterator))
  (and (valid (vertex-iterator iterator))
       (neighbor-iterator iterator)
       (valid (neighbor-iterator iterator))))


(defmethod current ((iterator edge-iterator))
  (values (current (vertex-iterator iterator))
          (when (neighbor-iterator iterator)
            (current (neighbor-iterator iterator)))))


(defmethod advance ((iterator edge-iterator))
  (with-slots (vertex-iterator graph neighbor-iterator)
              iterator
    (when neighbor-iterator
      (advance neighbor-iterator))
    (unless (and neighbor-iterator
                 (valid neighbor-iterator))
      (tagbody
       repeat
        (advance vertex-iterator)
        (when (valid vertex-iterator)
          (setf neighbor-iterator (neighbors graph (current vertex-iterator)))
          (unless (valid neighbor-iterator)
            (go repeat))))))
  iterator)


(defmethod reset ((iterator edge-iterator))
  (with-slots (vertex-iterator neighbor-iterator graph)
              iterator
    (reset vertex-iterator)
    (cond
      ((valid vertex-iterator)
        (setf neighbor-iterator (neighbors graph (current vertex-iterator)))
        (unless (valid neighbor-iterator)
          (advance iterator)))
      (t
        (setf neighbor-iterator nil))))
  iterator)
