(defrecord BST [root])
(defn make-bst [root] (BST. (ref nil)))
(defn bst-empty? [bst] (nil? @(:root bst)))
(defn bst-root [bst] (:root bst))
(defrecord BSTNode [data left right])
(defn make-bst-node [val] (BSTNode. val (ref nil) (ref nil)))
(defn bst-node-data [node] (:data node))
(defn bst-node-left [node] (:left node))
(defn bst-node-right [node] (:right node))
(defn bst-node-empty? [node] (nil? node))
(defn print-bst-node [node]
  (if (bst-node-empty? node)
    (print "nil")
    (do
      (print (bst-node-data node))
      (print-bst-node (deref (bst-node-left node)))
      (print-bst-node (deref (bst-node-right node))))))
(defn print-bst [bst]
  (print-bst-node (deref (bst-root bst))))
(defn bst-insert [bst val]
  (loop [node (:root bst)]
    (if [nil? @node]
      (dosync (ref-set node (make-bst-node val)))
      (let [node-data (bst-node-data @node)]
        (if (< val node-data)
          (recur (bst-node-left @node))
          (recur (bst-node-right @node)))))))
(def bst (make-bst nil))
(print-bst bst)
(bst-insert bst 5)
(bst-insert bst 3)
(print-bst bst)