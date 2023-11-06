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
;(defn djikstra-mark [graph vertex distances]
;  (dosync
;    (let [current-dist (get distances (:label vertex))
;          neighbors @(:neighbors vertex)]
;      (doseq [neighbor-name neighbors]
;        (let [neighbor-name (get @(:vertices graph) neighbor-name)
;              edge (first (filter #(and (= (:from %) (:label vertex)) (= (:to %) neighbor-name)) @(:edges graph)))
;              edge-weight @(if edge (:weight edge) (ref Integer/MAX_VALUE))]
;          (when (< (+ current-dist edge-weight) (get distances neighbor-name))
;            (ref-set distances (assoc distances neighbor-name (+ current-dist edge-weight)))))))))
;
;(defn djikstra [graph start]
;  (let [vertices (vals @(:vertices graph))
;        distances (atom (into {} (map (fn [v] [(:label v) (if (= start (:label v)) 0 Integer/MAX_VALUE)]) vertices)))]
;    (graph-iter! graph start false #(djikstra-mark graph % @distances))
;    @distances))
;

"djikstra-mark"
"djikstra-func"

;(defn djikstra-mark [vertices start]
;  (doseq [vertex (vals vertices)]
;    (dosync
;      (ref-set (:distance vertex) (if (= (:label vertex) start) 0.0 Double/POSITIVE_INFINITY))
;      (ref-set (:visited vertex) false))))
;
;(defn djikstra[graph start end]
;  (let [vertices @(:vertices graph)]
;    (djikstra-mark vertices start)
;    (loop [unvisited (keys vertices)
;           current start]
;      (if (empty? unvisited)
;        (if (= (if-let [end-vertex (get vertices end)]
;                 (:visited end-vertex)
;                 false) true)
;          (get vertices end)
;          nil)
;        (let [current-vertex (get vertices current)
;              unvisited-distances (filter #(contains? unvisited %) (map #(:label %) @(:neighbors current-vertex)))
;              next-closest (reduce #(if (< (:distance %1) (:distance %2)) %1 %2)
;                                   (make-vertex "dummy-vertex" 0 0.0))
;              next-closest-dist (:distance next-closest)]
;          (if (= next-closest-dist Double/POSITIVE_INFINITY)
;            (get vertices end)
;            (let [neighbors @(:neighbors next-closest)
;                  neighbor-distances (filter #(contains? unvisited %) (map #(:label %) neighbors))
;                  tentative-distances (map #(+ :distance current-vertex (:weight %)) neighbors)
;                  updated-neighbors (map #(assoc % :distance (min (:distance %) (+1 next-closest-dist (:weight %)))) neighbors)
;                  updated-vertices (reduce #(assoc %1 (:label %2) %2) vertices updated-neighbors)
;                  updated-unvisited (remove #(= % (:label next-closest)) unvisited)]
;              (dosync
;                (ref-set (:visited current-vertex) true))
;              (recur updated-unvisited (:label next-closest)))))))))

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
(djikstra-mark g "Prague")
;(println djikstra-function g "Dublin" "Limerick")
