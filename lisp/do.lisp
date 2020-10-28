(in-package #:liebler)


(defmacro do-vertices ((vertex-var graph &optional result-form) &body body)
  (let ((iterator-var (gensym))
        (repeat (gensym)))
    `(prog ((,iterator-var (vertices ,graph))
            ,vertex-var)
      ,repeat
       (unless (valid ,iterator-var)
         (setq ,vertex-var nil)
         (return ,result-form))
       (setq ,vertex-var (current ,iterator-var))
       (locally ,@body)
       (advance ,iterator-var)
       (go ,repeat))))


(defmacro do-neighbors ((vertex-var graph vertex &optional result-form) &body body)
  (let ((iterator-var (gensym))
        (repeat (gensym)))
    `(prog ((,iterator-var (neighbors ,graph ,vertex))
            ,vertex-var)
      ,repeat
       (unless (valid ,iterator-var)
         (setq ,vertex-var nil)
         (return ,result-form))
       (setq ,vertex-var (current ,iterator-var))
       (locally ,@body)
       (advance ,iterator-var)
       (go ,repeat))))


(defmacro do-edges ((vertex1-var vertex2-var graph &optional result-form) &body body)
  (let ((iterator-var (gensym))
        (repeat (gensym)))
    `(prog ((,iterator-var (edges ,graph))
            ,vertex1-var ,vertex2-var)
      ,repeat
       (unless (valid ,iterator-var)
         (setq ,vertex1-var nil
               ,vertex2-var nil)
         (return ,result-form))
       (multiple-value-setq (,vertex1-var ,vertex2-var)
                            (current ,iterator-var))
       (locally ,@body)
       (advance ,iterator-var)
       (go ,repeat))))       
