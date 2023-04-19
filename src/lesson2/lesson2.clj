;(. System (nanoTime))

;(let [name1 value1
;      name2 value2]
; sexpres)

;(let [a 1 b 2] (println a b))
;((fn [a b] (println a b))1 2)


;(defn fib [n]
;(if (< n 2) nil (do (println n (fib (- n 1))))))
;
;(fib 10)
;WRONG
;
;(defn fib [n] (if (< n 2) n  (+ (fib (- n 1)) (fib (- n 2)))))
;(println (fib 5))
;Fibonachi num

;(defn fibl-step [i fib_n-2 fib_n-1 n]
;  (let [fib_n (+ fib_n-2 fib_n-1)]
;    (if (= i n)
;      fib_n
;      (fibl-step (inc i) fib_n-1 fib_n n))))
;(defn fibl [n]
;  (if (< n 2)
;    n
;    (fibl-step 2 0N 1N n)))
;(println (fibl-step 1 2 3 4))


;(defn factorial [n]
;  (if (< n 2) n  (* n (factorial (- n 1)))))
;(println (factorial 5))
;Factorial working