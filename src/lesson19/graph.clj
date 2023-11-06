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
  (println "Visited:" (graph-count-components! graph)))
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
                     ;(println (:label v) my-distance @(:estimate))
                     (doseq [neighbor-rec @(:neighbors v)]
                       (let [neighbor-label (:label neighbor-rec)
                             edge-weight (:weight neighbor-rec)
                             new-distance (+ my-distance edge-weight)
                             neighbor (get @(:vertices graph)neighbor-label)
                             new-estimate (+ new-distance
                                             (great-circle-distance graph neighbor-label start))]
                         (if (or (nil? @(:distance neighbor))
                                 (< new-distance @(:distance neighbor)))
                           (dosync
                             (ref-set (:distance neighbor)
                                      new-distance)
                             (ref-set (:estimate neighbor)
                                      new-estimate))))))
                   (if (= (:label v) start)
                     'stop
                     nil))
                 (fn [queue]
                   (loop [queue queue
                          best-label nil
                          best-estimate nil]
                     (if (empty? queue)
                       best-label
                       ))))))
"-----------------------------------------------------------------------------------"
(load-file "e-roads-2020-full.clj")
;(println(great-circle-distance g "Prague" "Paris"))
;(println g)
;(graph-print-info g)
;(graph-iter! g "Kiev" false (fn [x] (println (:label x))))
;(println (get @(:vertices g) "Dublin"))
;(println (get @(:vertices g) "Drogheda"))
;(println(map (fn [x] (get @(:vertices g) x)) @(:neighbors (get @(:vertices g) "Prague"))))
(graph-reset! g)
;(println(graph-count-components! g))
;(dijkstra-mark! g "Prague")
;(println(dijkstra-best g "Kiev"))
(dijkstra-best g  "Hamburg")
