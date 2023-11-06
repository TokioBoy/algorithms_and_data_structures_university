(defrecord AVLTree [value left right height])

(defn avl-tree-empty?
  "Check if the AVL tree is empty."
  [tree]
  (nil? tree))

(defn avl-tree-height
  "Get the height of the AVL tree."
  [tree]
  (if (avl-tree-empty? tree)
    0
    (:height tree)))

(defn avl-tree-balance-factor
  "Calculate the balance factor of the AVL tree."
  [tree]
  (- (avl-tree-height (:right tree))
     (avl-tree-height (:left tree))))

(defn avl-tree-rotate-right
  "Perform a right rotation on the AVL tree."
  [tree]
  (let [left (:left tree)
        right-left (:right left)
        left-height (avl-tree-height left)
        right-height (avl-tree-height (:right tree))
        new-left (AVLTree. (:value left) (:left left) right-left left-height)
        new-right (AVLTree. (:value tree) new-left (:right tree) right-height)]
    (AVLTree. (:value left) new-right (:right tree) (inc (avl-tree-height new-right)))))

(defn avl-tree-rotate-left
  "Perform a left rotation on the AVL tree."
  [tree]
  (let [right (:right tree)
        left-right (:left right)
        right-height (avl-tree-height right)
        left-height (avl-tree-height (:left tree))
        new-right (AVLTree. (:value right) left-right (:right right) right-height)
        new-left (AVLTree. (:value tree) (:left tree) new-right left-height)]
    (AVLTree. (:value right) new-left new-right (inc (avl-tree-height new-left)))))

(defn avl-tree-balance
  "Balance the AVL tree if necessary."
  [tree]
  (let [balance-factor (avl-tree-balance-factor tree)]
    (cond
      (> balance-factor 1)
      (if (<= (avl-tree-balance-factor (:right tree)) 0)
        (avl-tree-rotate-right (AVLTree. (:value tree) (:left tree) (avl-tree-rotate-left (:right tree)) (avl-tree-height tree)))
        (avl-tree-rotate-right tree))

      (< balance-factor -1)
      (if (>= (avl-tree-balance-factor (:left tree)) 0)
        (avl-tree-rotate-left (AVLTree. (:value tree) (avl-tree-rotate-right (:left tree)) (:right tree) (avl-tree-height tree)))
        (avl-tree-rotate-left tree))

      :else
      tree)))

(defn avl-tree-insert
  "Insert a value into the AVL tree."
  [tree value]
  (if (avl-tree-empty? tree)
    (AVLTree. value nil nil 1)
    (let [cmp (compare value (:value tree))]
      (if (zero? cmp)
        tree
        (if (neg? cmp)
          (avl-tree-balance (AVLTree. (:value tree) (avl-tree-insert (:left tree) value) (:right tree) (avl-tree-height tree)))
          (avl-tree-balance (AVLTree. (:value tree) (:left tree) (avl-tree-insert (:right tree) value) (avl-tree-height tree))))))))

(defn avl-tree-find
  "Find a value in the AVL tree."
  [tree value]
  (if (avl-tree-empty? tree)
    nil
    (let [cmp (compare value (:value tree))]
      (cond
        (zero? cmp) (:value tree)
        (neg? cmp) (avl-tree-find (:left tree) value)
        :else (avl-tree-find (:right tree) value)))))
(defn avl-tree-values [tree]
  (when tree
    (concat (avl-tree-values (:left tree))
            [(:value tree)]
            (avl-tree-values (:right tree)))))

(defn avl-tree-count [tree]
  (if tree
    (+ 1 (avl-tree-count (:left tree)) (avl-tree-count (:right tree)))
    0))



(defrecord Graph [vertices edges])

(defn make-graph []
  (Graph. (ref {}) (ref '())))




(defrecord Vertex [label lat lon status neighbors distance estimate])
(defrecord Edge [from to label weight])
(defrecord Neighbor [label weight])

(defn make-vertex [label lat lon]
  (Vertex. label lat lon (ref 0) (ref (avl-tree-empty? nil)) (ref nil) (ref nil)))

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
        from-vertex (avl-tree-find vertices label1)
        to-vertex (avl-tree-find vertices label2)
        from-neighbors (:neighbors from-vertex)
        to-neighbors (:neighbors to-vertex)
        from-neighbors-updated (assoc from-neighbors label2 (Neighbor. label2 weight))
        to-neighbors-updated (assoc to-neighbors label1 (Neighbor. label1 weight))]
    (dosync
      (ref-set edges (conj @edges new-edge))
      (ref-set from-vertex (assoc from-vertex :neighbors from-neighbors-updated))
      (ref-set to-vertex (assoc to-vertex :neighbors to-neighbors-updated)))))





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
               neighbor (avl-tree-find @(:vertices graph) neighbor-name)]
           (if (vertex-unseen? neighbor)
             (do
               (dosync
                 (ref-set (:status neighbor) 1))
               (recur (conj queue neighbor-name)
                      (rest neighbors)))
             (recur queue
                    (rest neighbors))))))))

  (defn graph-iter! [graph start bfs? proc get-next]
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
              (let [new-queue (add-to-queue (filter (fn [label]
                                                      (not (= label current-label)))
                                                    queue)
                                            graph
                                            (avl-tree-values (:neighbors current-vertex))
                                            bfs?)]
                (recur new-queue))))))))


  (defn graph-reset! [graph]
    (doseq [vertex (avl-tree-values @(:vertices graph))]
      (dosync
        (ref-set (:status vertex) 0)
        (ref-set (:distance vertex) nil)
        )))


  (defn graph-print-info [graph]
    (println "Vertices :" (avl-tree-count @(:vertices graph)))
    (println "Edges:" (count @(:edges graph)))
    (println "Unseen:" (count (filter vertex-unseen? (avl-tree-values @(:vertices graph))))))

  (defn dijkstra-mark!
    ([graph finish]
     (dijkstra-mark! graph finish false))
    ([graph finish weights]
     (graph-reset! graph)
     (dosync
       (ref-set (:distance (avl-tree-find @(:vertices graph) finish)) 0))
     (graph-iter! graph
                  finish
                  (not weights)
                  (fn [v]
                    (let [my-distance @(:distance v)]
                      (println (:label v) my-distance)
                      (doseq [neighbor-rec (avl-tree-values (:neighbors v))]
                        (let [neighbor-label (:label neighbor-rec)
                              edge-weight (:weight neighbor-rec)
                              new-distance (if weights
                                             (+ my-distance edge-weight)
                                             (inc my-distance))
                              neighbor (avl-tree-find @(:vertices graph) neighbor-label)]
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
                                vertex (avl-tree-find @(:vertices graph) label)
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
    (loop [neighbors (avl-tree-values (:neighbors vertex))
           best-neighbor nil
           min-distance nil]
      (if (empty? neighbors)
        best-neighbor
        (let [neighbor-rec (first neighbors)
              neighbor-label (:label neighbor-rec)
              edge-weight (if weighted (:weight neighbor-rec) 1)
              neighbor (avl-tree-find @(:vertices graph) neighbor-label)
              neighbor-distance @(:distance neighbor)
              my-distance @(:distance vertex)]
          (if (and (or (nil? min-distance)
                       (< neighbor-distance min-distance))
                   (or (not weighted)
                       (= edge-weight (- my-distance neighbor-distance))))
            (recur (rest neighbors) neighbor-label (+ neighbor-distance edge-weight))
            (recur (rest neighbors) best-neighbor min-distance))))))


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
(graph-print-info g)
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


