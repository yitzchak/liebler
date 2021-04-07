(in-package #:liebler)


(defclass sequence-iterator ()
  ((sequence
     :accessor sequence-iterator-sequence
     :initarg :sequence)
   (limit
     :accessor sequence-iterator-limit
     :initarg :limit)
   (from-end
     :accessor sequence-iterator-from-end
     :initarg :from-end)
   (step
     :accessor sequence-iterator-step
     :initarg :step)
   (endp
     :accessor sequence-iterator-endp
     :initarg :endp)
   (element
     :accessor sequence-iterator-element
     :initarg :element)
   (setf-element
     :accessor sequence-iterator-setf-element
     :initarg :setf-element)
   (index
     :accessor sequence-iterator-index
     :initarg :index)
   (copy
     :accessor sequence-iterator-copy
     :initarg :copy)
   (initial
     :accessor sequence-iterator-initial)))


(defun make-sequence-iterator (sequence &rest initargs &key &allow-other-keys)
  (let ((instance (make-instance 'sequence-iterator :sequence sequence)))
    (with-slots (initial limit from-end step endp element setf-element index copy)
                instance
      (multiple-value-setq (initial limit from-end step endp element setf-element index copy)
                           (apply #'sequence:make-sequence-iterator sequence initargs)))
    instance))


(defclass product ()
  ((graphs
     :reader graphs
     :initarg :graphs
     :initform nil)
   (vertices
     :reader vertices)))


(defmethod directedp ((graph product))
  (some #'directedp (graphs graph)))


(defmethod order ((graph product))
  (apply #'* (mapcar #'order (graphs graph))))


(defclass product-vertices (sequence standard-object)
  ((parent
     :reader parent
     :initarg :parent)))


(defmethod initialize-instance :after ((instance product) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (slot-value instance 'vertices)
        (make-instance 'product-vertices :parent instance)))


(defmethod sequence:length ((instance product-vertices))
  (order (parent instance)))


(defmethod sequence:elt ((instance product-vertices) index)
  (declare (ignore instance))
  index)


(defmethod sequence:make-sequence-iterator ((sequence product-vertices) &key from-end start end)
  (let ((subs (mapcar #'make-sequence-iterator (mapcar #'vertices (graphs (parent sequence))))))
    (values (mapcar (lambda (sub)
                      (funcall (sequence-iterator-copy sub)
                               (sequence-iterator-sequence sub)
                               (sequence-iterator-initial sub)))
                    subs)
            ; limit
            nil
            ; from-end
            from-end
            ; step
            (lambda (sequence iterator from-end)
              (declare (ignore sequence from-end))
              (let* (copy-rest
                     (new-iterator (mapcar (lambda (sub sub-iterator)
                                             (let ((new-sub-iterator (if copy-rest
                                                                       (funcall (sequence-iterator-copy sub)
                                                                                (sequence-iterator-sequence sub)
                                                                                sub-iterator)
                                                                       (funcall (sequence-iterator-step sub)
                                                                                (sequence-iterator-sequence sub)
                                                                                sub-iterator
                                                                                (sequence-iterator-from-end sub)))))
                                               (unless (funcall (sequence-iterator-endp sub)
                                                                (sequence-iterator-sequence sub)
                                                                new-sub-iterator
                                                                (sequence-iterator-limit sub)
                                                                (sequence-iterator-from-end sub))
                                                 (setf copy-rest t))
                                               new-sub-iterator))
                                             subs iterator)))
                (if copy-rest
                  (mapcar (lambda (sub sub-iterator)
                            (if (funcall (sequence-iterator-endp sub)
                                         (sequence-iterator-sequence sub)
                                         sub-iterator
                                         (sequence-iterator-limit sub)
                                         (sequence-iterator-from-end sub))
                              (funcall (sequence-iterator-copy sub)
                                       (sequence-iterator-sequence sub)
                                       (sequence-iterator-initial sub))
                              sub-iterator))
                          subs new-iterator)
                  new-iterator)))
            ; endp
            (lambda (sequence iterator limit from-end)
              (declare (ignore sequence limit from-end))
              (every (lambda (sub sub-iterator)
                       (funcall (sequence-iterator-endp sub)
                                (sequence-iterator-sequence sub)
                                sub-iterator
                                (sequence-iterator-limit sub)
                                (sequence-iterator-from-end sub)))
                     subs iterator))
            ; element
            (lambda (sequence iterator)
              (declare (ignore sequence))
              (mapcar (lambda (sub sub-iterator)
                        (funcall (sequence-iterator-element sub)
                                 (sequence-iterator-sequence sub)
                                 sub-iterator))
                      subs iterator))
            ; setf-element
            (lambda (new-value sequence iterator)
              (declare (ignore sequence))
              (mapcar (lambda (sub sub-new-value sub-iterator)
                        (funcall (sequence-iterator-setf-element sub)
                                 sub-new-value
                                 (sequence-iterator-sequence sub)
                                 sub-iterator))
                      subs new-value iterator))
            ; index
            (lambda (sequence iterator)
              (declare (ignore sequence))
              (mapcar (lambda (sub sub-iterator)
                        (funcall (sequence-iterator-index sub)
                                 (sequence-iterator-sequence sub)
                                 sub-iterator))
                      subs iterator))
            ; copy
            (lambda (sequence iterator)
              (declare (ignore sequence))
              (mapcar (lambda (sub sub-iterator)
                        (funcall (sequence-iterator-copy sub)
                                 (sequence-iterator-sequence sub)
                                 sub-iterator))
                      subs iterator)))))


(defclass modular-product (product)
  ())


(defun modular-product (&rest graphs)
  (make-instance 'modular-product :graphs graphs))


(defmethod neighborp ((graph modular-product) vertex1 vertex2)
  (reduce (lambda (x y)
            (or (and x y)
                (and (not x) (not y))))
          (mapcar #'neighborp (graphs graph) vertex1 vertex2)))


(defclass tensor-product (product)
  ())


(defun tensor-product (&rest graphs)
  (make-instance 'tensor-product :graphs graphs))


(defmethod neighborp ((graph tensor-product) vertex1 vertex2)
  (some #'neighborp (graphs graph) vertex1 vertex2))



