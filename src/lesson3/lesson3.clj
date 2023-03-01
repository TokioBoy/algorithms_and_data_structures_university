;(defn nano-time [] (. System (nanoTime)))
;(defn measure-function [func]
;  (let [start-time (nano-time)
;        end-time (nano-time)]
;    (- end-time start-time)))
;(println measure-function)
;WORKING


;(defn nanotime [] (. System (nanoTime)))
;
;(defn factorial [n]
;  (if (< n 2) n  (* n (factorial (- n 1)))))
;(println (factorial 6))
;(def start-time (nanotime))
;(def end-time (nanotime))
;(def duration (- end-time start-time))
;(println duration)
;calc factorial and func time in nanosec
;WORKING


;(defn wedone [i]
;(defn nanotime [] (. System (nanoTime)))
;(defn factorial [n]
;  (if (< n 2) n  (* n (factorial (- n 1)))))
;(println (factorial i))
;(def start-time (nanotime))
;(def end-time (nanotime))
;(def duration (- end-time start-time))
;(println duration)
;)
;(println (wedone 9))



;(defn analyze-function [func min max]
;  (measure-function func min)
;  (if (< min max)
;    (analyze-function func (inc min)max)))
