;(def make-slist-node (fn [data next] (fn [acc] (acc data next))))
;(def slist-node-data (fn [node] (node (fn [data next] data))))
;(make-slist-node 1 2)
;(println (slist-node-data (make-slist-node 1 2)))
;
;
;
;(def slist-node-next (fn [node] (node (fn [data next] next))))
;(println (slist-node-next (make-slist-node 1 2)))
;

(defrecord DList [head tail])
(defrecord DListNode [prev data next])

(defn make-dlist [] (DList. (ref nil) (ref nil)))
(defn dlist-empty? [lst] (nil? (deref (:head lst))))
(defn dlist-ok? [lst]
  (= (nil? (deref (:head lst)))
  (nil? (deref (:tail lst)))))
(defn dlist? [lst] (= (class lst) DList))
(defn dlist-prepend! [lst val]
  (let [new-node (DListNode. (ref nil) val (ref (deref (:head lst))))]
    (if (dlist-empty? lst)
      (dosync (ref-set (:head lst) new-node)
              (ref-set (:tail lst) new-node))
      (dosync (ref-set (:prev (deref (:head lst)))new-node)
              (ref-set (:head lst)new-node))))val)
(defn dlist-iter [lst func]
  (loop [node (deref (:head lst))]
    (if (not (nil? node))
      (do
        (func (:data node))
        (recur (deref (:next node)))))))


(def lst1 (make-dlist))
(dlist-prepend! lst1 1)
(dlist-prepend! lst1 2)
(dlist-prepend! lst1 3)
(dlist-iter lst1 println)


(defn dlist-rem-first! [lst]
  (if (not (dlist-empty? lst))
    (if (= (deref (:head lst)) (deref (:tail lst)))
    (dosync (ref-set (:head lst)nil)
            (ref-set (:tail lst) nil)))))


