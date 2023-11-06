;; Define the Vertex, Edge, and Neighbor records
(defrecord Vertex [label lat lon status neighbors distance])
(defrecord Edge [from to label weight])
(defrecord Neighbor [label weight])

;; Define the Red-Black Tree node structure
(defrecord RBNode [key value color left right])

;; Define the Red-Black Tree structure
(defrecord RBTree [root])

;; Constants for color representation
(def red :red)
(def black :black)

;; Function to rotate a tree node to the left
(defn rotate-left [node]
  (let [right (:right node)]
    (-> node
        (assoc :right (:left right))
        (assoc-in [:right :left] node)
        (assoc :key (:key right)))))

;; Function to rotate a tree node to the right
(defn rotate-right [node]
  (let [left (:left node)]
    (-> node
        (assoc :left (:right left))
        (assoc-in [:left :right] node)
        (assoc :key (:key left)))))

;; Function to flip the colors of a node and its children
(defn color-flip [node]
  (let [flip-color (fn [color] (if (= color red) black red))]
    (assoc node :color (flip-color (:color node))
                :left (assoc (:left node) :color (flip-color (:color (:left node))))
                :right (assoc (:right node) :color (flip-color (:color (:right node)))))))

;; Function to check if a node is red
(defn red? [node]
  (and (not (nil? node)) (= (:color node) red)))

;; Function to blacken a node
(defn blacken [node]
  (when node
    (assoc node :color black)))


;; Helper function to find a vertex in the tree by label
(defn tree-find-vertex [node label]
  (if (nil? node)
    nil
    (let [vertex (:value node)]
      (cond
        (= label (:label vertex)) vertex
        (< label (:label vertex)) (tree-find-vertex (:left node) label)
        (> label (:label vertex)) (tree-find-vertex (:right node) label)))))


;; Function to print the tree
(defn print-tree [tree]
  (letfn [(print-node [node level]
            (when node
              (print-node (:right node) (inc level))
              (println (str "" (repeat level "  ")) (:value node))
              (print-node (:left node) (inc level))))]

    (print-node (:root tree) 0)))

;; Function to balance the tree after insertion
(defn balance [node]
  (cond
    (and (red? (:right node)) (red? (:right (:right node))))
    (-> node
        (rotate-left)
        (color-flip))

    (and (red? (:left node)) (red? (:left (:left node))))
    (-> node
        (rotate-right)
        (color-flip))

    (and (red? (:left node)) (red? (:right node)))
    (-> node
        (rotate-left)
        (rotate-right)
        (color-flip))

    :else node))


;; Create an empty Red-Black Tree
(defn create-tree []
  (RBTree. nil))

;; Function to add a vertex to the tree
(defn tree-add-vertex [tree vertex]
  (letfn [(insert-helper [node k v]
            (if (nil? node)
              (RBNode. k v red nil nil)
              (if (< k (:key node))
                (balance (RBNode. (:key node) (:value node) (:color node) (insert-helper (:left node) k v) (:right node))))
              (if (> k (:key node))
                (balance (RBNode. (:key node) (:value node) (:color node) (:left node) (insert-helper (:right node) k v))))
              (RBNode. k v (:color node) (:left node) (:right node))))
(RBTree. (blacken (insert-helper (:root tree) (:label vertex) vertex)))]))

;; Function to add an edge between two vertices
(defn tree-add-edge [tree edge]
  (let [update-vertex (fn [vertex]
                        (assoc vertex :neighbors (conj (:neighbors vertex) (Neighbor. (:to edge) (:weight edge)))))]
    (update-in tree [:root] (fn [node] (tree-add-vertex node (update-vertex (tree-find-vertex node (:from edge))))))))



;; Usage example
(let [tree (create-tree)
      vertex1 (Vertex. 1 0.0 0.0 nil nil nil)
      vertex2 (Vertex. 2 1.0 1.0 nil nil nil)
      edge (Edge. 1 2 "edge1" 10.0)]

  (def tree (tree-add-vertex tree vertex1))
  (def tree (tree-add-vertex tree vertex2))
  (def tree (tree-add-edge tree edge))
  (print-tree tree))
