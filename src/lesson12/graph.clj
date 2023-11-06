(defrecord Graph [vertices edges])
(defn make-graph []
  (Graph. (ref {}) (ref '())))

(defrecord Vertex [label lat lon neighbors status])
(defrecord Edge [from to label weight])
"------------------------------------------------------------------------------------"
(defn make-vertex [label lat lon]
  (Vertex. label lat lon (ref '()) (ref 0)))

(defn make-edge [from to label weight]
  (Edge. from to label (ref weight)))

(defn graph-add-vertex! [graph label lat lon]
  (let [vertices (:vertices graph)
        new-vertex (make-vertex label lat lon)]
    (dosync
      (ref-set (:vertices graph)
               (assoc @vertices label new-vertex)))))

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
"-----------------------------------------------------------------------------------"
(defn graph-print-info [graph]
  ((println "Vertices :" (count @(:vertices graph)))
   (println "Edges:" (count @(:edges graph)))))
"-----------------------------------------------------------------------------------"
(defn vertex-unseen? [vertex]
  (= @(:status vertex) 0))


(defn dfs-add-to-queue [queue graph neighbors]
  (loop [queue queue
         neighbors neighbors]
    (if (empty? neighbors)
      queue
      (let [neighbor-name (first neighbors)
            neighbor (get @(:vertices graph) neighbor-name)]
        (if (vertex-unseen? neighbor)
          (do
            (dosync
              (ref-set (:status neighbor) 1))
            (recur (conj queue neighbor-name)
                   (rest neighbors)))
          (recur queue
                 (rest neighbors)))))))


(defn graph-dfs [graph start]
  (loop [queue (list start)]
    (when (not (empty? queue))
      (let [current-label (first queue)
            current-vertex (get @(:vertices graph)current-label)]
        (dosync (ref-set (:status current-vertex) 2))
        (println current-label)
        (dosync (ref-set (:status current-vertex) 3))
        (recur (dfs-add-to-queue (rest queue) graph @(:neighbors current-vertex)))))))
"-----------------------------------------------------------------------------------"
(load-file "e-roads-2020-full.clj")
;(println g)
;(graph-print-info g)
;(graph-dfs g "Hirtshals")
(println (get @(:vertices g) "Prague"))
