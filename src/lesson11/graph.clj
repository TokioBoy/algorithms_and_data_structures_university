(defrecord Graph [vertices edges])

(defn make-graph []
  (Graph. (ref {}) (ref '())))





(defn graph-add-vertex!
  [g name lat lon]
  (dosync
    (alter (:vertices g) assoc name [lat lon])
    (alter (:edges g) conj [name []])))

(defn graph-add-edge! [graph from to label weight]
  (dosync
    (alter (:edges graph) conj [from to label weight])))

(def g (make-graph))
(graph-add-vertex! g "Hirtshals" 57.5908031 9.9649737)
(graph-add-edge! g "Vercelli" "Alessandria" "E25" 50)


(def d (ref {}))
(dosync (ref-set d (assoc @d "new-key" "new-value")))



;(load-file "e-roads-2020-full.clj")
(println g)