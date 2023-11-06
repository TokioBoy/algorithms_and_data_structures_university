(defrecord Graph [vertices edges])
(defn make-graph []
  (Graph. (ref {}) (ref '())))

(defrecord Vertex [label lat lon status neighbors distance estimate])
(defrecord Edge [from to label weight])
(defrecord Neighbor [label weight])
"------------------------------------------------------------------------------------"
(defn make-vertex [label lat lon]
  (Vertex. label lat lon (ref 0) (ref '()) (ref nil) (ref nil)))

(defn make-edge [from to label weight]
  (Edge. from to label (ref weight)))

(defn graph-add-vertex! [graph label lat lon]
  (let [vertices (:vertices graph)
        new-vertex (make-vertex label lat lon)]
    (dosync
      (ref-set (:vertices graph)
               (assoc @vertices label new-vertex))))
  nil)

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

(defn graph-reset! [graph]
  (doseq [vertex (vals @(:vertices graph))]
    (dosync
      (ref-set (:status vertex) 0)
      (ref-set (:distance vertex) nil)
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
  (println "Unseen:" (count (filter vertex-unseen? (vals @(:vertices graph))))))
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
                    (println (:label v) my-distance)
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
;


(defn dijkstra-trace [graph start finish weighted]
  (loop [current-label start
         path [start]]
    (let [vertex (get @(:vertices graph) current-label)
          distance @(:distance vertex)]
      (println distance current-label)
      (if (nil? @(:distance vertex))
        (println "No path found")
        (if (= current-label finish)
          (println)
          (let [current-vertex (get @(:vertices graph) current-label)
                best-neighbor (find-best-neighbor current-vertex graph weighted)]
            (recur best-neighbor (conj path best-neighbor))))))))


(defn graph_dijkstra! [graph start finish weighted]
  (graph-reset! graph)
  (dijkstra-mark! graph finish weighted)
  (println "===========================")
  (dijkstra-trace graph start finish weighted))


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

(defn heuristic-distance [vertex1 vertex2]
  (great-circle-distance vertex1 vertex2))

(defn a-star-mark!
  [graph start finish]
  (dijkstra-mark! graph finish true)
  (let [start-vertex (get @(:vertices graph) start)
        finish-vertex (get @(:vertices graph) finish)
        start-distance @(:distance start-vertex)
        finish-distance @(:distance finish-vertex)]
    (if (nil? start-distance)
      (do (println "No path found"))
      (do (println "Start to Finish distance:" finish-distance)
          (println "Heuristic (Finish to Start) distance:" (heuristic-distance finish-vertex start-vertex))))))

(defn a-star-trace [graph start finish]
  (let [visited (atom {})
        distances (atom {})
        priority-queue (atom [])
        path (atom [start])]
    (loop [current-label start]
      (let [vertex (get @(:vertices graph) current-label)
            distance @(:distance vertex)
            priority-queue-clone (vec @priority-queue)]
        (if (nil? distance)
          (do (println "No path found")
              (reset! visited @visited)
              (reset! distances @distances)
              (println "Visited vertices:", @visited)
              (println "Distances:", @distances)
              (println "Priority Queue:", priority-queue-clone)
              (println "Path:", @path))
          (if (= current-label finish)
            (do (reset! visited @visited)
                (reset! distances @distances)
                (println "Visited vertices:", @visited)
                (println "Distances:", @distances)
                (println "Priority Queue:", priority-queue-clone)
                (println "Path:", @path))
            (let [current-vertex (get @(:vertices graph) current-label)
                  current-distance @(:distance current-vertex)
                  best-neighbor (find-best-neighbor current-vertex graph true)]
              (swap! path conj best-neighbor)
              (swap! visited assoc current-label current-distance)
              (swap! distances assoc current-label current-distance)
              (swap! priority-queue conj [best-neighbor current-distance])
              (recur best-neighbor))))))))

(defn graph_a-star!
  [graph start finish]
  (graph-reset! graph)
  (a-star-mark! graph start finish)
  (println "--------------------------")
  (a-star-trace graph start finish))
"-----------------------------------------------------------------------------------"
(load-file "e-roads-2020-full.clj")
;(println(great-circle-distance g "Prague" "Paris"))
;(println g)
;(graph-print-info g)
;(graph-iter! g "Kiev" false (fn [x] (println (:label x))))
;(println (get @(:vertices g) "Dublin"))
;(println (get @(:vertices g) "Drogheda"))
;(println(map (fn [x] (get @(:vertices g) x)) @(:neighbors (get @(:vertices g) "Prague"))))
;(println(graph-count-components! g))
;(dijkstra-mark! g "Prague")
;(println(dijkstra-best g "Kiev"))
;
;(graph_dijkstra! g "Kyzylorda" "Prague" true)
;(graph_dijkstra! g "Cork (city)" "Prague" false)

;(dijkstra-mark! g "Prague")

;(graph_a-star! g "Kiev" "Prague")

(graph_a-star! g "Cork (city)" "Prague")
