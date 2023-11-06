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

(defn add-to-queue
  ([queue graph neighbors]
   (add-to-queue queue graph neighbors false))
  ([queue graph neighbors bfs?]
   (loop [queue (if bfs? (reverse queue) queue)
          neighbors neighbors]
     (if (empty? neighbors)
     (if bfs? (reverse queue) queue)
     (let [neighbor-name (first neighbors)
           neighbor (get @(:vertices graph) neighbor-name)]
       (if (vertex-unseen? neighbor)
         (do
           (dosync
             (ref-set (:status neighbor) 1))
           (recur (conj queue neighbor-name)
                  (rest neighbors)))
         (recur queue
                (rest neighbors))))))))

(defn graph-iter!
  ([graph start]
   (graph-iter! graph start false))
  ([graph start bfs?]
   (graph-iter! graph start bfs? (fn [x] nil)))
  ([graph start bfs? proc]
   (loop [queue (list start)]
     (when (not (empty? queue))
       (let [current-label (first queue)
             current-vertex (get @(:vertices graph) current-label)]
         (dosync
           (ref-set (:status current-vertex) 2))
         (proc current-vertex)
         (dosync
           (ref-set (:status current-vertex) 3))
         (recur (add-to-queue (rest queue)
                              graph
                              @(:neighbors current-vertex)bfs?)))))))


(defn graph-reset! [graph]
  (doseq [vertex (vals @(:vertices graph))]
    (dosync
      (ref-set (:status vertex) 0)
      (ref-set (:status distance) nil))))


(defn graph-count-components! [graph]
  (graph-reset! graph)
  (loop [vertices (vals @(:vertices graph))
         components 0]
    (if (empty? vertices)
      components
      (let [vertex (first vertices)]
        (if (vertex-unseen? vertex)
          (do
            (graph-iter! graph (:label vertex))
            (recur (rest vertices)
                   (inc components)))
          (recur  (rest vertices)
                   components))))))
"-----------------------------------------------------------------------------------"

"djikstra-mark"
"djikstra-func"
"-----------------------------------------------------------------------------------"
(load-file "e-roads-2020-full.clj")
;(println g)
;(graph-print-info g)
;(graph-iter! g "Kiev" false (fn [x] (println (:label x))))
;(println (get @(:vertices g) "Dublin"))
;(println (get @(:vertices g) "Drogheda"))
;(println(map (fn [x] (get @(:vertices g) x)) @(:neighbors (get @(:vertices g) "Prague"))))
(graph-reset! g)
(println(graph-count-components! g))

