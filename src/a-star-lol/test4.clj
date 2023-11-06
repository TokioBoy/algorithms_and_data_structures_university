(defrecord Graph [vertices edges])
;; Definition of Graph record

(defn make-graph []
  (Graph. (ref {}) (ref '())))
;; Function to create a new graph

(defrecord Vertex [label lat lon status neighbors distance estimate]) ;; Definition of Vertex record
(defrecord Edge [from to label weight]);; Definition of Edge record
(defrecord Neighbor [label weight]) ;; Definition of Neighbor record
"------------------------------------------------------------------------------------"
(defn make-vertex [label lat lon]
  (Vertex. label lat lon (ref 0) (ref '()) (ref nil) (ref nil)))
;; Function to create a new vertex

(defn make-edge [from to label weight]
  (Edge. from to label (ref weight)))
;; Function to create a new edge

(defn graph-add-vertex! [graph label lat lon]
  (let [vertices (:vertices graph)
        new-vertex (make-vertex label lat lon)]
    (dosync
      (ref-set (:vertices graph)
               (assoc @vertices label new-vertex))))
  nil)
;; Function to add a vertex to the graph

(defn graph-add-edge! [graph label1 label2 label weight]
  (let [edges (:edges graph)
        new-edge (make-edge label1 label2 label weight)
        vertices @(:vertices graph)
        from-vertex (get vertices label1)
        to-vertex (get vertices label2)
        from-neighbors (:neighbors from-vertex)
        to-neighbors (:neighbors to-vertex)]
    (dosync
      (ref-set edges (conj @edges new-edge))
      (ref-set from-neighbors (conj @from-neighbors
                                    (Neighbor. label2 weight)))
      (ref-set to-neighbors (conj @to-neighbors
                                  (Neighbor. label1 weight)))))nil)
;; Function to add an edge to the graph

(load-file "e-roads-2020-full.clj")
"-----------------------------------------------------------------------------------"
(defn vertex-unseen? [vertex]
  (= @(:status vertex) 0))
;; Function to check if a vertex is unseen

(defn add-to-queue
  ([queue graph neighbors]
   (add-to-queue queue graph neighbors false))
  ([queue graph neighbors bfs?]
   (loop [queue (if bfs? (reverse queue) queue)
          neighbors neighbors]
     (if (empty? neighbors)
       (if bfs? (reverse queue) queue)
       (let [neighbor-rec (first neighbors)
             neighbor-name (:label neighbor-rec)
             neighbor (get @(:vertices graph) neighbor-name)]
         (if (vertex-unseen? neighbor)
           (do
             (dosync
               (ref-set (:status neighbor) 1))
             (recur (conj queue neighbor-name)
                    (rest neighbors)))
           (recur queue
                  (rest neighbors))))))))
;; Function to add vertices to the queue for processing

(defn graph-iter!
  ([graph start]
   (graph-iter! graph start false))
  ([graph start bfs?]
   (graph-iter! graph start bfs? (fn [x] nil)))
  ([graph start bfs? proc]
   (graph-iter! graph start bfs? proc first))
  ([graph start bfs? proc get-next]
   (loop [queue (list start)]
     (when (not (empty? queue))
       (let [current-label (get-next queue)
             current-vertex (get @(:vertices graph) current-label)]
         (dosync
           (ref-set (:status current-vertex) 2))
         (let [stop? (proc current-vertex)]
           (dosync
             (ref-set (:status current-vertex) 3))
           (when (not (= stop? 'stop))
             (recur (add-to-queue (filter (fn [label]
                                            (not (= label current-label)))
                                          queue)
                                  graph
                                  @(:neighbors current-vertex)
                                  bfs?)))))))))
;; Function to add vertices to the queue for processing

(defn graph-reset! [graph]
  (doseq [vertex (vals @(:vertices graph))]
    (dosync
      (ref-set (:status vertex) 0)
      (ref-set (:distance vertex) nil)
      (ref-set (:estimate vertex) nil)
      )))
;; Function to reset the graph's vertices to their initial state

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
;; Function to count the number of connected components in the graph
"-----------------------------------------------------------------------------------"
(defn graph-print-info [graph]
  (println "Vertices :" (count @(:vertices graph)))
  (println "Edges:" (count @(:edges graph)))
  (println "Unseen:" (count (filter vertex-unseen? (vals @(:vertices graph))))))
;; Function to print information about the graph
"-----------------------------------------------------------------------------------"
(defn dijkstra-mark!
  ([graph finish]
   (dijkstra-mark! graph finish false))
  ([graph finish weights]
   (graph-reset! graph)
   (dosync
     (ref-set (:distance (get @(:vertices graph) finish)) 0))
   (graph-iter! graph
                finish
                (not weights)
                (fn [v]
                  (let [my-distance @(:distance v)]
                    ;(println (:label v) my-distance)
                    (doseq [neighbor-rec @(:neighbors v)]
                      (let [neighbor-label (:label neighbor-rec)
                            edge-weight (:weight neighbor-rec)
                            new-distance (if weights
                                           (+ my-distance edge-weight)
                                           (inc my-distance))
                            neighbor (get @(:vertices graph) neighbor-label)]
                        (if (or (nil? @(:distance neighbor))
                                (< new-distance @(:distance neighbor)))
                          (dosync
                            (ref-set (:distance neighbor)
                                     new-distance)))))))
                (if weights
                  (fn [queue]
                    (loop [queue queue
                           best-label nil
                           best-distance nil]
                      (if (empty? queue)
                        best-label
                        (let [label (first queue)
                              vertex (get @(:vertices graph) label)
                              distance @(:distance vertex)]
                          (if (or (nil? best-label)
                                  (< distance best-distance))
                            (recur (rest queue)
                                   label
                                   distance)
                            (recur (rest queue)
                                   best-label
                                   best-distance))))))
                  first))))
;; Function to perform Dijkstra's algorithm and mark the distances

(defn find-best-neighbor [vertex graph weighted]
  (loop [neighbors @(:neighbors vertex)
         best-neighbor nil
         min-distance nil]
    (if (empty? neighbors)
      best-neighbor
      (let [neighbor-rec (first neighbors)
            neighbor-label (:label neighbor-rec)
            edge-weight (if weighted (:weight neighbor-rec) 1)
            neighbor (get @(:vertices graph) neighbor-label)
            neighbor-distance @(:distance neighbor)
            my-distance @(:distance vertex)]
        (if (and (or (nil? min-distance)
                     (< neighbor-distance min-distance))
                 (or (not weighted)
                     (= edge-weight (- my-distance neighbor-distance))))
          (recur (rest neighbors) neighbor-label (+ neighbor-distance edge-weight))
          (recur (rest neighbors) best-neighbor min-distance))))))
;; Function to find the best neighbor of a vertex


(defn dijkstra-trace [graph start finish weighted]
  (loop [current-label start
         path [start]]
    (let [vertex (get @(:vertices graph) current-label)
          distance @(:distance vertex)]
      (println distance current-label)
      (if (nil? @(:distance vertex))
        (println "Sorry, there is no path")
        (if (= current-label finish)
          (println)
          (let [current-vertex (get @(:vertices graph) current-label)
                best-neighbor (find-best-neighbor current-vertex graph weighted)]
            (recur best-neighbor (conj path best-neighbor))))))))
;; Function to trace the shortest path found by Dijkstra's algorithm


(defn great-circle-distance
  ([graph label1 label2]
   (great-circle-distance (get @(:vertices graph) label1)
                          (get @(:vertices graph) label2)))
  ([vertex1 vertex2]
   (let [φ1 (:lat vertex1)
         λ1 (:lon vertex1)
         φ2 (:lat vertex2)
         λ2 (:lon vertex2)
         ∆λ (Math/abs (- λ2 λ1))
         ∆λr (Math/toRadians ∆λ)
         φ1r (Math/toRadians φ1)
         φ2r (Math/toRadians φ2)
         dist1 (Math/acos (+ (* (Math/sin φ1r) (Math/sin φ2r))
                             (* (Math/cos φ1r) (Math/cos φ2r) (Math/cos ∆λr))))
         r 6378]
     (* dist1 r))))
;; Function to calculate the great circle distance between two vertices

(defn A*-mark! [graph start finish]
  (graph-reset! graph)
  (let [start-vertex (get @(:vertices graph) finish)]
    (dosync
      (ref-set (:distance start-vertex) 0)
      (ref-set (:estimate start-vertex)
               (great-circle-distance graph finish start)))
    (graph-iter! graph
                 finish
                 false
                 (fn [v]
                   (let [my-distance @(:distance v)]
                     ;(println (:label v) my-distance @(:estimate v))
                     (doseq [neighbor-rec @(:neighbors v)]
                       (let [neighbor-label (:label neighbor-rec)
                             edge-weight (:weight neighbor-rec)
                             new-distance (+ my-distance edge-weight)
                             neighbor (get @(:vertices graph) neighbor-label)
                             new-estimate (+ new-distance
                                             (great-circle-distance graph neighbor-label start))]
                         (if (or (nil? @(:distance neighbor))
                                 (< new-distance @(:distance neighbor)))
                           (dosync
                             (ref-set (:distance neighbor) new-distance)
                             (ref-set (:estimate neighbor) new-estimate))))))
                   (if (= (:label v) start)
                     'stop
                     nil))
                 (fn [queue]
                   (loop [queue queue
                          best-label nil
                          best-estimate nil]
                     (if (empty? queue)
                       best-label
                       (let [label (first queue)
                             vertex (get @(:vertices graph) label)
                             estimate @(:estimate vertex)]
                         (if (or (nil? best-estimate)
                                 (< estimate best-estimate))
                           (recur (rest queue)
                                  label
                                  estimate)
                           (recur (rest queue)
                                  best-label
                                  best-estimate)))))))))
;; Function to perform A* search and mark the distances and estimates

(defn graph_dijkstra! [graph start finish weighted]
  (graph-reset! graph)
  (dijkstra-mark! graph finish weighted)
  (dijkstra-trace graph start finish weighted))
;; Function to find the shortest path using Dijkstra's algorithm

(defn graph-A*! [graph start finish weighted]
  (graph-reset! graph)
  (A*-mark! graph start finish)
  (dijkstra-trace graph start finish weighted))
;; Function to find the shortest path using A* search
"-----------------------------------------------------------------------------------"
(println "Problem 1")
(println "The shortest path from Kyzylorda to Prague according to the number of cities crossed is:")
(println "")
(graph_dijkstra! g "Kyzylorda" "Prague" false)
(println "-----------------------------------")
(println "The shortest path from Cork to Prague according to the number of cities crossed is:")
(println "")
(graph_dijkstra! g "Cork (city)" "Prague" false)
(println "-----------------------------------")
(println "")
(println "Problem 2")
(println "The shortest path from Kyzylorda to Prague according to the sum of lengths of roads traversed:")
(println "")
(graph-A*! g "Kyzylorda" "Prague" true)
(println "-----------------------------------")
(println "The shortest path from Cork to Prague according to the sum of lengths of roads traversed:")
(println "")
(graph-A*! g "Cork (city)" "Prague" true)
(println "-----------------------------------")
