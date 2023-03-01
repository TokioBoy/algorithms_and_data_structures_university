(def lst '(1 2 3 4 5))

(defn print-lst [lst] (if (empty? lst) nil (do (println (first lst)) (print-lst (rest lst)))))



(def lst1 '(1 2 3 4 5))
(def lst2 '(3 4 5 6 7 89))


(defn print-cartesian [lst1 lst2] (if (empty? lst2) nil (do (println (first lst1) (first lst2)) (print-cartesian lst1 (rest lst2)))))

(defn print-cartesian-list [lst1 print-cartesian lst2]
  (if (empty? lst1)
    nil
    (do
      (println (first lst1) (first (print-cartesian lst1 lst2)))
      (print-cartesian-list (rest lst1) print-cartesian lst2))))

(def lst `(0 1 7 2 5 8 16 3 19 6 14 9 9 17 17 4))
(def lst3 `(1 2 3 4))

(defn find-smallest [lst]
  (if (empty? (rest lst))
    (first lst)
    (if (< (first lst) (first (rest lst)))
      (find-smallest (cons (first lst) (rest (rest lst))))
      (find-smallest (rest lst)))))

(println (find-smallest lst))

(defn lists-powerset [lst]
  (if (empty? lst)
    (list ())
    (concat (lists-powerset (rest lst)) (map #(cons (first lst) %) (lists-powerset (rest lst))))))


(println (lists-powerset lst3))