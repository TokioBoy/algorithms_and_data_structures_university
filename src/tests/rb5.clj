(defrecord RBNode [key value color left right])

(defn make-rbnode [key value color left right]
  (RBNode. key value color left right))

(defn empty-tree []
  nil)

(defn black? [node]
  (if (nil? node)
    true
    (= (:color node) :black)))

(defn red? [node]
  (and (not (nil? node))
       (= (:color node) :red)))

(defn make-node [key value]
  (make-rbnode key value :red nil nil))

(defn left-rotate [node]
  (let [right (:right node)
        right-left (:left right)]
    (make-rbnode (:key right) (:value right) (:color node)
                 (make-rbnode (:key node) (:value node) :red (:left node) right-left)
                 (:right right))))

(defn right-rotate [node]
  (let [left (:left node)
        left-right (:right left)]
    (make-rbnode (:key left) (:value left) (:color node)
                 left-right
                 (make-rbnode (:key node) (:value node) :red left-right (:right node)))))

(defn rb-insert-fix [tree node]
  (if (nil? tree)
    node
    (if (< (:key node) (:key tree))
      (let [new-left (rb-insert-fix (:left tree) node)]
        (if (red? (:left tree))
          (if (red? (:left new-left))
            (assoc tree :left (assoc new-left :color :black))
            (if (red? (:right new-left))
              (assoc (assoc tree :left (left-rotate (:left new-left))) :color :black)
              tree))
          (assoc tree :left new-left)))
      (let [new-right (rb-insert-fix (:right tree) node)]
        (if (red? (:left tree))
          (if (red? (:left new-right))
            (assoc (assoc tree :right (right-rotate (:right new-right))) :color :black)
            (if (red? (:right new-right))
              (assoc tree :right (assoc new-right :color :black))
              tree))
          (assoc tree :right new-right))))))

(defn rb-insert [tree key value]
  (let [node (make-node key value)]
    (-> tree
        (assoc-in [:right] (rb-insert-fix (get-in tree [:right]) node))
        (assoc :color :black))))


(defn rb-find [tree key]
  (let [node (get-in tree [:right])]
    (if (nil? node)
      nil
      (let [node-key (:key node)]
        (cond
          (< key node-key) (rb-find (assoc tree [:right] (:left node)) key)
          (> key node-key) (rb-find (assoc tree [:right] (:right node)) key)
          :else (:value node))))))

(defrecord Graph [vertices edges])

(defn make-graph []
  (Graph. (empty-tree) (ref '())))


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
    (assoc-in graph [:vertices] (rb-insert vertices label new-vertex))))


(defn graph-add-edge! [graph label1 label2 label weight]
  (let [edges (:edges graph)
        new-edge (make-edge label1 label2 label weight)
        vertices (:vertices graph)
        from-vertex (rb-find vertices label1)
        to-vertex (rb-find vertices label2)
        from-neighbors (:neighbors from-vertex)
        to-neighbors (:neighbors to-vertex)]
    (dosync
      (ref-set edges (conj @edges new-edge))
      (ref-set from-neighbors (conj @from-neighbors (Neighbor. label2 weight)))
      (ref-set to-neighbors (conj @to-neighbors (Neighbor. label1 weight))))
    graph))
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
             neighbor (rb-find @(:vertices graph) neighbor-name)]
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
             current-vertex (rb-find @(:vertices graph) current-label)]
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
  (let [vertices (vals @(:vertices graph))]
    (doseq [vertex vertices]
      (dosync
        (ref-set (:status vertex) 0)
        (ref-set (:distance vertex) nil))))
  graph)


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
   (let [vertices @(:vertices graph)
         finish-vertex (rb-find vertices finish)]
     (dosync
       (ref-set (:distance finish-vertex) 0)))
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
                            neighbor (rb-find @(:vertices graph) neighbor-label)]
                        (if (or (nil? @(:distance neighbor))
                                (< new-distance @(:distance neighbor)))
                          (dosync
                            (ref-set (:distance neighbor)
                                     new-distance))))))))))

(defn find-best-neighbor [vertex graph weighted]
  (let [neighbors @(:neighbors vertex)]
    (loop [neighbors neighbors
           best-neighbor nil
           min-distance nil]
      (if (empty? neighbors)
        best-neighbor
        (let [neighbor-rec (first neighbors)
              neighbor-label (:label neighbor-rec)
              edge-weight (if weighted (:weight neighbor-rec) 1)
              neighbor (rb-find @(:vertices graph) neighbor-label)
              neighbor-distance @(:distance neighbor)
              my-distance @(:distance vertex)]
          (if (and (or (nil? min-distance)
                       (< neighbor-distance min-distance))
                   (or (not weighted)
                       (= edge-weight (- my-distance neighbor-distance))))
            (recur (rest neighbors) neighbor-label (+ neighbor-distance edge-weight))
            (recur (rest neighbors) best-neighbor min-distance)))))))


(defn dijkstra-trace [graph start finish weighted]
  (loop [current-label start
         path [start]]
    (let [vertex (rb-find @(:vertices graph) current-label)
          distance @(:distance vertex)]
      (println distance current-label)
      (if (nil? @(:distance vertex))
        (println "No path found")
        (if (= current-label finish)
          (println)
          (let [current-vertex (rb-find @(:vertices graph) current-label)
                neighbor-label (find-best-neighbor current-vertex graph weighted)]
            (if (nil? neighbor-label)
              (println "No path found")
              (recur neighbor-label (conj path neighbor-label)))))))))


(defn graph_dijkstra! [graph start finish weighted]
  (graph-reset! graph)
  (dijkstra-mark! graph finish weighted)
  (println "===========================")
  (dijkstra-trace graph start finish weighted))
"-----------------------------------------------------------------------------------"
(load-file "e-roads-2020-full.clj")
;(println(great-circle-distance g "Prague" "Paris"))
;(println g)
(graph-print-info g)
;(graph-iter! g "Kiev" false (fn [x] (println (:label x))))
;(println (get @(:vertices g) "Dublin"))
;(println (get @(:vertices g) "Drogheda"))
;(println(map (fn [x] (get @(:vertices g) x)) @(:neighbors (get @(:vertices g) "Prague"))))
;(println(graph-count-components! g))
;(dijkstra-mark! g "Prague")
;(println(dijkstra-best g "Kiev"))
(graph-reset! g)
;(graph_dijkstra! g "Kyzylorda" "Prague" true)
;(graph_dijkstra! g "Cork (city)" "Prague" false)