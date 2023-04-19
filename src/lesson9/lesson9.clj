;(def lst [5 3 7 1 4 6 8])
;(defn bst-insert [bst val]
;  (if (empty? bst)
;    [val [] []]
;    (let [[node-val left right] bst]
;      (if (< val node-val)
;        (vector node-val (bst-insert left val) right)
;        (vector node-val left (bst-insert right val))))))
;
;(defn build-bst [lst]
;  (reduce bst-insert [] lst))
;(def bst (build-bst lst))
;(println bst)



(defn make-dict []
  (ref {}))

(defn dict-empty? [dict]
  (not (deref dict)))

(defn dict-set! [dict key value]
  (dosync (alter dict assoc key value)))

(defn dict-get [dict key]
  (@dict key))

(defn dict-contains? [dict key]
  (contains? @dict key))

(defn dict-remove! [dict key]
  (dosync (alter dict dissoc key)))

(defn dict-iter [dict func]
  (doseq [[k v] @dict]
    (func k v)))

(defn dict-count [dict]
  (count @dict))

(defn dict-reduce [dict]
  (reduce conj [] @dict))

;test the dictionary functions with sample data
(def my-dict (make-dict))
(dict-set! my-dict "a" 1)
(dict-set! my-dict "b" 2)
(dict-set! my-dict "c" 3)
(dict-set! my-dict "d" 4)
(dict-set! my-dict "e" 5)
(println my-dict)

;check if the dictionary is empty
(println(dict-empty? my-dict))


;get value from dictionary
(println(dict-get my-dict "b"))

;check if key is present in dictionary
(println(dict-contains? my-dict "f"))

;remove key-value pair from dictionary
(dict-remove! my-dict "c")
(println(dict-count my-dict))

;iterate over dictionary
(println(dict-iter my-dict (fn [k v] (println k "=>" v))))
;reduce dictionary to a list of key-value pairs
(println(dict-reduce my-dict))