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

;(defn graph-add-edge! [graph label1 label2 label weight]
;  (let [edges (:edges graph)
;        new-edge (make-edge label1 label2 label weight)
;        vertices (:vertices graph)
;        from-vertex (rb-find vertices label1)
;        to-vertex (rb-find vertices label2)
;        from-neighbors (:neighbors from-vertex)
;        to-neighbors (:neighbors to-vertex)]
;    (dosync
;      (ref-set edges (conj @edges new-edge))
;      (ref-set from-neighbors (conj @from-neighbors (Neighbor. label2 weight)))
;      (ref-set to-neighbors (conj @to-neighbors (Neighbor. label1 weight))))
;    graph))

;(defn graph-add-edge! [graph from-label to-label edge-label weight]
;  (let [vertices (:vertices graph)
;        edges (:edges graph)
;        from-vertex (rb-find vertices from-label)
;        to-vertex (rb-find vertices to-label)]
;    (if (and from-vertex to-vertex)
;      (let [new-edge (make-edge from-vertex to-vertex edge-label weight)]
;        (dosync (ref-set edges (conj @edges new-edge)))
;        (dosync (alter (:neighbors from-vertex) conj (Neighbor. to-label weight)))
;        graph)
;      (throw (Exception. "One or both vertices not found.")))))
"-----------------------------------------------------------------------------------"
(load-file "e-roads-2020-full.clj")
(println g)