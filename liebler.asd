(defsystem "liebler"
  :version "0.1.0"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on ()
  :components ((:module "lisp"
                :components
                ((:file "package")
                 (:file "graph")
                 (:file "adjacency-matrix")
                 (:file "bron-kerbosch"))))
  :description "")


