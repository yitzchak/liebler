(defsystem "liebler"
  :version "0.1.0"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on (:do-bang :trivial-extensible-sequences)
  :components ((:module "lisp"
                :components
                ((:file "package")
                 (:file "graph")
                 (:file "iterator")
                 ;(:file "do")
                 ;(:file "functional")
                 (:file "default")
                 (:file "child-graph")
                 (:file "adjacency-matrix")
                 ;(:file "colored-graph")
                 ;(:file "ordered-graph")
                 (:file "product")
                 (:file "bron-kerbosch")
                 ;(:file "max-clique-seq"))))
                 )))
  :description "")


