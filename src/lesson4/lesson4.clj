;(defrecord Point [x y])
;(println (Point. 1 2))
;(println (class (Point. 1 2)))
;(println (= (class (Point. 1 2)) Point))

;
;(defn print-countdown [num]
;  (println num)
;  (if (> num 0)
;    (recur (- num 1))))
;(println (print-countdown 10))



; (loop [num 10]
;  (println num)
;  (if (> num 0)
;    (recur (- num 1))))


;(defrecord SListNode [next data])
;Single-linked list node
;(defrecord SList [head])
;Single-linked list

(defrecord SListNode [next data])
(defrecord SList [head])
(defn make-slist []
  (SList. (ref nil)))
;(println make-slist)



(defn slist? [val] (= (class val) SList))
(println slist?)


(defn slist-empty? [lst]
  (nil? (deref (:head lst))))


(defn slist-prepend! [lst val]
  (dosync
  (ref-set (:head lst)
           (SListNode. (ref (deref (:head lst)))val)))
val)



(def lst1 (make-slist))
(slist-prepend! lst1 123)
(println lst1)




(defn slist-prepend [lst val]
  (SList.
    (ref
      (SListNode. (ref (deref (:head lst)))val))))




(defn slist-first [lst]
  (:data (deref (:head lst))))
(println(slist-first lst1))


(defn slist-rest [lst] (SList. (ref (deref (:next (deref (:head lst)))))))


(defn slist-iter-helper [node func]
  (if (not (nil? node))
    (do
      (func (:data node)
          )
      (slist-iter-helper (deref (:next node))func))))



