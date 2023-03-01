(def lst '(1 2 3 4))
(defn print-lst [lst] (if (empty? lst) nil (do (println (first lst)) (print-lst (rest lst)))))
(println (print-lst lst))