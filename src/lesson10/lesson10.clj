(defrecord Node [el left right])
(defn insert [{:keys [el left right] :as tree} value]
  (cond
    (nil? tree) (Node. value nil nil)
    (< value el) (Node. el (insert left value) right)
    (> value el) (Node. el left (insert right value))
    :else tree))
(defn remove [{:keys [el left right] :as tree} value]
  (cond
    (nil? tree) nil
    (< value el) (Node. el (remove left value) right)
    (> value el) (Node. el left (remove right value))
    (nil? left) right
    (nil? right) left
    :else (let [min-value (min right)]
            (Node. min-value left (remove right min-value)))))
(defn min [{:keys [el left]}]
  (if left
    (recur left)
    el))
(defn max [{:keys [el right]}]
  (if right
    (recur right)
    el))
(defn contains? [{:keys [el left right] :as tree} value]
  (cond
    (nil? tree) false
    (< value el) (recur left value)
    (> value el) (recur right value)
    :else true))
(defn count [{:keys [left right] :as tree}]
  (if tree
    (+ 1 (count left) (count right))
    0))
(defn height
  ([tree] (height tree 0))
  ([tree count]
   (if tree
     (let [left-height (height (:left tree) (inc count))           right-height (height (:right tree) (inc count))]
       (clojure.core/max left-height right-height))
     count)))
(defn bst?
  ([tree] (bst? tree Integer/MIN_VALUE Integer/MAX_VALUE))
  ([{:keys [el left right] :as tree} min max]
   (cond
     (nil? tree) true
     (or (< el min) (> el max)) false
     :else (and (bst? left min (dec el))
                (bst? right (inc el) max)))))
(def to-tree #(reduce insert nil %))
(defn to-list [{:keys [el left right] :as tree}]
  (when tree
    `(~@(to-list left) ~el ~@(to-list right))))

(def tree (to-tree '(5 8 2 3 4 1)))
(bst? tree) ; true
(count tree) ; 6
(height tree) ; 4
(max tree) ; 8
(min tree) ; 1
(to-list (remove tree 3)) ; (1 2 4 5 8)
(contains? tree 2) ; true

