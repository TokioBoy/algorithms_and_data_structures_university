
;; Red-Black Tree implementation starts here

;; Color constants
(def ^:private RED :red)
(def ^:private BLACK :black)

;; Node representation
(defrecord Node [key value left right color])

;; Empty tree representation
(def empty-tree nil)

;; Function to check if a node is red
(defn red? [node]
  (= (:color node) RED))

;; Function to rotate the tree left
(defn rotate-left [node]
  (let [right (:right node)]
    (-> node
        (assoc :right (:left right))
        (assoc :color RED)
        (assoc :left (assoc right :left node :color (:color node)))
        (update :right dissoc :left))))

;; Function to rotate the tree right
(defn rotate-right [node]
  (let [left (:left node)]
    (-> node
        (assoc :left (:right left))
        (assoc :color RED)
        (assoc :right (assoc left :right node :color (:color node)))
        (update :left dissoc :right))))

;; Function to flip the colors of the node and its children
(defn flip-colors [node]
  (assoc node
    :color (if (red? node) BLACK RED)
    :left (assoc (:left node) :color (if (red? (:left node)) BLACK RED))
    :right (assoc (:right node) :color (if (red? (:right node)) BLACK RED))))

;; Function to insert a key-value pair into the Red-Black Tree
(defn rb-insert [tree key value]
  (let [insert-rec (fn insert-rec [node]
                     (cond
                       (nil? node) (-> (Node. key value nil nil RED) (assoc :color BLACK))
                       (< key (:key node)) (let [left (insert-rec (:left node))]
                                             (if (and (red? left) (red? (:left left)))
                                               (-> node
                                                   (assoc :left left)
                                                   rotate-right
                                                   flip-colors)
                                               (assoc node :left left)))
                       (> key (:key node)) (let [right (insert-rec (:right node))]
                                             (if (and (red? right) (red? (:right right)))
                                               (-> node
                                                   (assoc :right right)
                                                   rotate-left
                                                   flip-colors)
                                               (assoc node :right right)))
                       :else (assoc node :value value)))]
    (-> tree insert-rec)))

;; Red-Black Tree implementation ends here

(defrecord Graph [vertices edges tree]) ; Added 'tree' field to the Graph record

(defn make-graph []
  (Graph. (ref '()) empty-tree)) ; Initialized 'tree' field with empty-tree

(defrecord Vertex [label lat lon status neighbors distance estimate road])
(defrecord Edge [from to label weight])
(defrecord Neighbor [label weight])

(defn make-vertex [label lat lon]
  (Vertex. label lat lon (ref 0) (ref '()) (ref nil) (ref nil) (ref nil)))

(defn make-edge [from to label weight]
  (Edge. from to label (ref weight)))

(defn graph-add-vertex! [graph label lat lon]
  (let [vertices (:vertices graph)
        new-vertex (make-vertex label lat lon)]
    (dosync
      (ref-set (:vertices graph)
               (assoc @vertices label new-vertex)))
    graph) ; Return the updated graph
  )

(defn graph-add-edge! [graph label1 label2 label weight]
  (let [vertices (:vertices graph)
        edges (:edges graph)
        vertex1 (@vertices label1)
        vertex2 (@vertices label2)
        new-edge (make-edge label1 label2 label weight)]
    (dosync
      (ref-set (:edges graph)
               (conj @edges new-edge))
      (alter vertex1 update :neighbors
             (fn [neighbors]
               (conj neighbors (Neighbor. label2 weight))))
      (alter vertex2 update :neighbors
             (fn [neighbors]
               (conj neighbors (Neighbor. label1 weight)))))
    graph) ); Return the updated graph

  (defn graph-remove-edge! [graph label1 label2]
    (let [edges (:edges graph)
          vertex1 (@(:vertices graph) label1)
          vertex2 (@(:vertices graph) label2)]
      (dosync
        (ref-set (:edges graph)
                 (remove #(and (= (:from %) label1)
                               (= (:to %) label2)) @edges))
        (alter vertex1 update :neighbors
               (fn [neighbors]
                 (remove #(= (:label %) label2) neighbors)))
        (alter vertex2 update :neighbors
               (fn [neighbors]
                 (remove #(= (:label %) label1) neighbors)))))
    graph )


