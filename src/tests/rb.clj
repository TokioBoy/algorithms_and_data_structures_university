;; Red-Black Tree implementation

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

;; Example usage
(def tree (-> empty-tree
              (rb-insert 5 "A")
              (rb-insert 3 "B")
              (rb-insert 7 "C")
              ))

;; Print the tree
(println tree)
