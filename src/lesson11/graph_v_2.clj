;(def d (ref {}))
;(dosync (ref-set d (assoc @d "new-key" "new-value")))
;(println d)

(defrecord Graph [vertices edges])
(defn make-graph []
  (Graph. (ref {}) (ref '())))

(defrecord Vertex [label lat lon neighbors])
(defrecord Edge [from to label weight])

(defn make-vertex [label lat lon]
  (Vertex. label lat lon (ref '())))
(defn make-edge [from to label weight]
  (Edge. from to label (ref weight)))

(defn graph-add-vertex! [graph label lat lon]
  (let [vertices (:vertices graph)
        new-vertex (make-vertex label lat lon)]
    (dosync
      (ref-set (:vertices graph) (assoc @vertices label new-vertex)))))


(defn graph-add-edge! [graph from to label weight]
  (let [edges (:edges graph)
        new-edge (make-edge from to label weight)
        vertices @(:vertices graph)
        from-vertex (get vertices from)
        to-vertex (get vertices to)
        from-neighbors (:neighbors from-vertex)
        to-neighbors (:neighbors to-vertex)]
    (dosync
      (ref-set edges (conj @edges new-edge))
      (ref-set from-neighbors (conj @from-neighbors to))
      (ref-set to-neighbors (conj @to-neighbors from)))))


(load-file "e-roads-2020-full.clj")
(println g)





;(defn graph-add-edge! [graph from to label weight]
;  (let [edges (:edges graph)
;        new-edge (make-edge from to label weight)]
;    (dosync
;      (ref-set edges (conj @edges new-edge)))))
