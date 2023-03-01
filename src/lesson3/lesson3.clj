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

