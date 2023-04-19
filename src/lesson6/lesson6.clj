(def arr (int-array [1 9 5 20 30]))
(count arr)
(aget arr 3)
;get element 3
(aset arr 3 4)
;make element 3 = 4
;(println (aget arr 4))

(defn slist-mode-pair? [node]
  (and (not (nil? @node))
       (not (nil? @(:next @node)))))


(defn slist-pair-sorted? [pair]
  (< (:data @pair)
     (:data @(:next @pair))))



