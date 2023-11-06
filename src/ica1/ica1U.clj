(defrecord SListNode [next data])
(defrecord SList [head])

(defn make-slist []
  (SList. (ref nil)))

(defn slist-prepend! [lst val]
  (dosync
    (ref-set (:head lst)
             (SListNode. (ref (deref (:head lst)))val)))
  val)

(defn slist-append! [lst & values]
  (doseq [value values]
    (let [new-node (SListNode. (ref nil) value)]
      (if (nil? (deref (:head lst)))
        (dosync
          (ref-set (:head lst) new-node))
        (loop [node (deref (:head lst))]
          (if (nil? (deref (:next node)))
            (dosync
              (ref-set (:next node) new-node))
            (recur (deref (:next node)))))))))

(def lst1 (make-slist))
(slist-prepend! lst1 8)
(slist-append! lst1 31 2007)

(defn find-node [lst val]
  (letfn [(search [node]
            (when node
              (if (= val (:data node))
                true
                (search (deref (:next node))))))]
    (if (search (deref (:head lst)))
      (str "Node with value " val " found!")
      (str "Node with value " val " not found."))))

(println (find-node lst1 9))
(println (find-node lst1 31))
(println (find-node lst1 93239))

(defn slist-iter [lst func]
  (loop [node (deref (:head lst))]
    (if (not (nil? node))
      (do
        (func (:data node))
        (recur (deref (:next node)))))))

(defn cartesian-product [lst]
  (slist-iter lst (fn [y]
                    (slist-iter lst (fn [x]
                                      (print (str "[" x " " y "] "))))))
  (println))
(cartesian-product lst1)