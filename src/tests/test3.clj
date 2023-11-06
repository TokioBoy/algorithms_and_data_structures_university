(defrecord Graph [vertices edges])

(defn make-graph []
  (Graph. (ref {}) (ref '())))

(defrecord Vertex [label lat lon status neighbors distance estimate])
(defrecord Edge [from to label weight])
(defrecord Neighbor [label weight])

(defn make-vertex [label lat lon]
  (Vertex. label lat lon (ref 0) (ref '()) (ref nil) (ref nil)))

(defn make-edge [from to label weight]
  (Edge. from to label (ref weight)))

(defn graph-add-vertex! [graph label lat lon]
  (let [vertices (:vertices graph)
        new-vertex (make-vertex label lat lon)]
    (dosync
      (alter vertices assoc label new-vertex))))

(defn graph-add-edge! [graph label1 label2 label weight]
  (let [edges (:edges graph)
        new-edge (make-edge label1 label2 label weight)
        vertices @(:vertices graph)
        from-vertex (get vertices label1)
        to-vertex (get vertices label2)
        from-neighbors (:neighbors from-vertex)
        to-neighbors (:neighbors to-vertex)]
    (dosync
      (alter edges conj new-edge)
      (alter from-neighbors conj (Neighbor. label2 weight))
      (alter to-neighbors conj (Neighbor. label1 weight)))))

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
               (alter (:status neighbor) (constantly 1)))
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
           (alter (:status current-vertex) (constantly 2)))
         (let [stop? (proc current-vertex)]
           (dosync
             (alter (:status current-vertex) (constantly 3)))
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
      (alter (:status vertex) (constantly 0))
      (alter (:distance vertex) (constantly nil)))))

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

(defn graph-print-info [graph]
  (println "Vertices:" (count @(:vertices graph)))
  (println "Edges:" (count @(:edges graph)))
  (println "Unseen:" (count (filter vertex-unseen? (vals @(:vertices graph))))))

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


(defn heuristic-cost [graph label1 label2]
  (let [vertex1 (get @(:vertices graph) label1)
        vertex2 (get @(:vertices graph) label2)]
    (great-circle-distance vertex1 vertex2)))

(defn update-neighbor [graph current finish neighbor-rec weights]
  (let [my-distance (:distance current)
        my-estimate (heuristic-cost graph (:label current) finish)
        neighbor-label (:label neighbor-rec)
        edge-weight (:weight neighbor-rec)
        neighbor (get (:vertices graph) neighbor-label)
        neighbor-distance (:distance neighbor)
        new-distance (if weights
                       (+ my-distance edge-weight)
                       (inc my-distance))
        new-estimate (+ new-distance (heuristic-cost graph neighbor-label finish))]
    (when (or (nil? neighbor-distance)
              (< new-estimate (:estimate neighbor)))
      (swap! neighbor assoc :distance new-distance)
      (swap! neighbor assoc :estimate new-estimate))))

(defn a-star-mark! [graph finish weights]
  (let [vertices (:vertices graph)
        finish-vertex (get vertices finish)]
    (swap! finish-vertex assoc :distance 0)
    (doseq [current (vals @vertices)]
      (println (:label current) (:distance current))
      (doseq [neighbor-rec (:neighbors current)]
        (update-neighbor graph current finish neighbor-rec weights)))))

(defn a-star-trace [graph start finish weighted]
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

(defn graph-a-star! [graph start finish weighted]
       (graph-reset! graph)
       (a-star-mark! graph finish weighted)
       (println "===========================")
       (a-star-trace graph start finish weighted))

     (load-file "e-roads-2020-full.clj")
(a-star-mark! g "Prague" true)
     ;(graph_dijkstra! g "Kyzylorda" "Prague" true)
     ;(graph_dijkstra! g "Cork (city)" "Prague" false)
     ;(graph-a-star! g "Kiev" "Prague" false)
