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


(defun make-sequences-iterator (sequences &key from-end start end)
  (let ((subs (mapcar #'make-sequence-iterator sequences)))
    (values (cons 0
                  (mapcar (lambda (sub)
                            (funcall (sequence-iterator-copy sub)
                                     (sequence-iterator-sequence sub)
                                     (sequence-iterator-initial sub)))
                          subs))
            ; limit
            nil
            ; from-end
            from-end
            ; step
            (lambda (sequence iterator from-end)
              (declare (ignore sequence from-end))
              (let* (copy-rest
                     (new-iterator (cons (1+ (first iterator))
                                         (mapcar (lambda (sub sub-iterator)
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
                                             subs (cdr iterator)))))
                (if copy-rest
                  (cons (first new-iterator)
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
                          subs (cdr new-iterator)))
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
                     subs (cdr iterator)))
            ; element
            (lambda (sequence iterator)
              (declare (ignore sequence))
              (mapcar (lambda (sub sub-iterator)
                        (funcall (sequence-iterator-element sub)
                                 (sequence-iterator-sequence sub)
                                 sub-iterator))
                      subs (cdr iterator)))
            ; setf-element
            (lambda (new-value sequence iterator)
              (declare (ignore sequence))
              (mapcar (lambda (sub sub-new-value sub-iterator)
                        (funcall (sequence-iterator-setf-element sub)
                                 sub-new-value
                                 (sequence-iterator-sequence sub)
                                 sub-iterator))
                      subs new-value (cdr iterator)))
            ; index
            (lambda (sequence iterator)
              (declare (ignore sequence))
              (first iterator))
            ; copy
            (lambda (sequence iterator)
              (declare (ignore sequence))
              (cons (first iterator)
                    (mapcar (lambda (sub sub-iterator)
                              (funcall (sequence-iterator-copy sub)
                                       (sequence-iterator-sequence sub)
                                       sub-iterator))
                            subs (cdr iterator)))))))






