(defrecord Graph [vertices edges])
(defn make-graph []
  (Graph. (ref {}) (ref '())))

(defrecord Vertex [label lat lon neighbors status distance])
(defrecord Edge [from to label weight])
"------------------------------------------------------------------------------------"
(defn make-vertex [label lat lon]
  (Vertex. label lat lon (ref '()) (ref 0) (ref nil)))

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
      ;(ref-set (:status distance) nil)
      )))


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
(defn graph-print-info [graph]
  (println "Vertices :" (count @(:vertices graph)))
  (println "Edges:" (count @(:edges graph)))
  (println "Visited:" (graph-count-components! graph)))
"-----------------------------------------------------------------------------------"
(defn dijkstra-mark! [graph finish]
  (graph-reset! graph)
  (dosync
    (ref-set (:distance (get @(:vertices graph) finish)) 0))
  (graph-iter! graph
               finish
               true? (fn [v]
                       (let [my-distance @(:distance v)]
                         (println (:label v) my-distance)
                         (doseq [neighbor-label @(:neighbors v)]
                           (let [neighbor (get @(:vertices graph) neighbor-label)]
                             (if (nil? @(:distance neighbor))
                               (dosync
                                 (ref-set (:distance neighbor) (inc my-distance))))))))))


(defn dijkstra-best [graph label]
  (let [vertices @(:vertices graph)
        vertex (get vertices label)]
    (loop [neighbors-labels @(:neighbors vertex)
           best-distance nil
           best-label nil]
      (if (empty? neighbors-labels)
        best-label
        (let [neighbor-label (first neighbors-labels)
              neighbor-vertex (get vertices neighbor-label)
              neighbor-distance @(:distance neighbor-vertex)]
          (if (or (nil? best-label)
                  (< neighbor-distance best-distance))
            (recur (rest neighbors-labels)
                   neighbor-distance
                   neighbor-label)
            (recur (rest neighbors-labels)
                   best-distance
                   best-label)))))))

(defn dijkstra-trace [graph start]
  (loop [current start]
    (let [vertex (get @(:vertices graph) current)]
      (println current)
      (if (nil? @(:distance vertex))
        (println "No path here!")
        (when (> @(:distance vertex) 0)
          (recur (dijkstra-best graph current)))))))


(defn graph-dijkstra! [graph start finish]
  (dijkstra-mark! graph finish)
  (println "===============================")
  (dijkstra-trace graph start))
"-----------------------------------------------------------------------------------"
(load-file "e-roads-2020-full.clj")
;(println g)
;(graph-print-info g)
;(graph-iter! g "Kiev" false (fn [x] (println (:label x))))
;(println (get @(:vertices g) "Dublin"))
;(println (get @(:vertices g) "Drogheda"))
;(println(map (fn [x] (get @(:vertices g) x)) @(:neighbors (get @(:vertices g) "Prague"))))
(graph-reset! g)
;(println(graph-count-components! g))
;(dijkstra-mark! g "Kiev")
;(println(dijkstra-best g "Kiev"))
;(graph-dijkstra! g "Belfast" "Dublin")
(graph-dijkstra! g "Paris" "Hamburg")
