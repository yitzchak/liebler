(in-package "liebler")


(defgeneric map-vertices (result-type function graph))


(defgeneric map-edges (result-type function graph))


(defgeneric map-neighbors (result-type function graph vertex))


(defgeneric neighborp (graph vertex))


(defgeneric rank (graph vertex))


(defgeneric order (graph))
