(defrecord RedBlackTree [root])

(defrecord Node [key value color left right])

(def BLACK :black)
(def RED :red)

(defn tree-insert [tree key value]
  (letfn [(insert-fixup [node]
            (cond
              (and (-> node :left :color) (-> node :left :left :color) (-> node :right :color))
              (let [node-left (-> node :left)
                    node-right (-> node :right)]
                (assoc node :color RED
                            :left (-> node-left :left (assoc :color BLACK))
                            :right (assoc node-right :color BLACK)))

              (and (-> node :left :color) (-> node :left :right :color) (-> node :right :color))
              (let [node-left (-> node :left)
                    node-right (-> node :right)]
                (assoc node :color RED
                            :left (-> node-left :right (assoc :color BLACK))
                            :right (assoc node-right :color BLACK)))

              (and (-> node :right :color) (-> node :left :color))
              (let [node-left (-> node :left)
                    node-right (-> node :right)]
                (assoc node :color RED
                            :left (assoc node-left :color BLACK)
                            :right (-> node-right :right (assoc :color BLACK))))

              (and (-> node :left :color) (-> node :right :color))
              (let [node-left (-> node :left)
                    node-right (-> node :right)]
                (assoc node :color RED :left (assoc node-left :color BLACK) :right (assoc node-right :color BLACK)))

              :else node))]

    (letfn [(insert [node key value]
              (if (nil? node)
                (Node. key value RED nil nil)
                (let [node-key (:key node)]
                  (if (< key node-key)
                    (let [new-left (insert (:left node) key value)]
                      (insert-fixup (Node. node-key (:value node) (:color node) new-left (:right node))))
                    (let [new-right (insert (:right node) key value)]
                      (insert-fixup (Node. node-key (:value node) (:color node) (:left node) new-right)))))))]

      (-> tree :root (insert key value) (assoc tree :root)))))

(defn tree-search [tree key default-val]
  (letfn [(search [node key]
            (cond
              (nil? node) default-val
              (= key (:key node)) (:value node)
              (< key (:key node)) (search (:left node) key)
              :else (search (:right node) key)))]

    (search (:root tree) key)))

(defrecord Graph [vertices edges])

(defn make-graph []
  (Graph. (ref (-> (RedBlackTree. nil) :root))
          (ref '())))

(defrecord Vertex [label lat lon status neighbors distance estimate])
(defrecord Edge [from to label weight])
(defrecord Neighbor [label weight])

(defn make-vertex [label lat lon]
  (Vertex. label lat lon (ref 0) (ref (-> (RedBlackTree. nil) :root)) (ref nil) (ref nil)))

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
(graph_dijkstra! g "Kyzylorda" "Prague" true)
;(graph_dijkstra! g "Cork (city)" "Prague" true)
