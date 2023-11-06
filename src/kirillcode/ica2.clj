(defrecord Graph [nodes node-map])
(defrecord Node [name stat neighbours distance])
(defrecord Neighbour [index weight])

(defn make-graph []
  (Graph.  (ref (vector))
           (ref (hash-map))))

;; nodes as vector for random access
;; node-map as a hash-map for conversion between neighbour index and node name
;; stat: 0 open 1 reserved 2 occupied 3 passed 4 backtraced

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;helpers;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-index-by-name [graph name]
  (get @(:node-map graph) name))

(defn get-node-by-name [graph name]
  (get @(:nodes graph) (get-index-by-name graph name)))

(defn get-distance-from-index [graph index]
  (:distance (get @(:nodes graph) index)))

(defn set-node-stat! [graph index val]
  (let [node (get @(:nodes graph) index)]
    (dosync
      (ref-set (:stat node) val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;utils;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def mersenne 2147483647)

(defn graph-add-vertex! [graph name]
  (let* [nodes (:nodes graph)
         node-map (:node-map graph)
         index (count @nodes)
         node (Node. name (ref 0) (ref nil) (ref mersenne))]
    (dosync
      (ref-set node-map (assoc @node-map name index))
      (ref-set nodes (conj @nodes node)))))

(defn graph-add-edge [graph name-one name-two weight]
  (let* [node-one (get-node-by-name graph name-one)
         node-two (get-node-by-name graph name-two)
         neighbours-one (:neighbours node-one)
         neighbours-two (:neighbours node-two)]
    (dosync
      (ref-set neighbours-one
               (conj @neighbours-one
                     (Neighbour. (get-index-by-name graph name-two) weight)))
      (ref-set neighbours-two
               (conj @neighbours-two
                     (Neighbour. (get-index-by-name graph name-one) weight))))))

(defn reset-graph! [graph]
  (dosync
    (doseq [node @(:nodes graph)]
      (ref-set (:stat node) 0)
      (ref-set (:distance node) mersenne))))


(defn graph-print [graph]
  (doseq [node @(:nodes graph)]
    (println (str "Name: " (:name node)))
    (println (str "Index: " (get-index-by-name graph (:name node))))
    (println (str "Stat: " @(:stat node)))
    (println (str "Distance: " @(:distance node)))
    (println (str "======"))))
; (println (str "Neighbours: "))
; (doseq [neighbour @(:neighbours node)]
;   (println "  Index: " (:index neighbour))
;   (println "  Weight: " (:weight neighbour))
;   (println (str "============")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;dlist;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord DList [head tail])
(defrecord DListPack [prev data next])

(defn dlist-make []
  (DList. (ref nil) (ref nil)))

(defn dlist-empty? [lst]
  (nil? @(:tail lst)))

(defn dlist-remove-first! [lst]
  (let* [head (:head lst)
         tail (:tail lst)]
    (if (not (dlist-empty? lst))
      (if (nil? @(:next @head))
        (dosync (ref-set head nil)
                (ref-set tail nil))
        (dosync (ref-set head @(:next @head))
                (ref-set (:prev @head) nil))))))

(defn dlist-remove-last! [lst]
  (let* [head (:head lst)
         tail (:tail lst)]
    (if (not (dlist-empty? lst))
      (if (nil? @(:next @head))
        (dosync (ref-set head nil)
                (ref-set tail nil))
        (dosync (ref-set tail @(:prev @tail))
                (ref-set (:next @tail) nil))))))

(defn dlist-append! [lst val]
  (let* [head (:head lst)
         tail (:tail lst)]
    (if (dlist-empty? lst)
      (dosync
        (let [nPack (DListPack. (ref nil) val (ref nil))]
          (ref-set head nPack)
          (ref-set tail nPack)))
      (dosync
        (let [nPack (DListPack. (ref @tail) val (ref nil))]
          (ref-set (:next @tail) nPack)
          (ref-set tail nPack))))))

(defn dlist-prepend! [lst val]
  (let* [head (:head lst)
         tail (:tail lst)]
    (if (dlist-empty? lst)
      (dosync
        (let [nPack (DListPack. (ref nil) val (ref nil))]
          (ref-set head nPack)
          (ref-set tail nPack)))
      (dosync
        (let [nPack (DListPack. (ref nil) val (ref @head))]
          (ref-set (:prev @head) nPack)
          (ref-set head nPack))))))

(defn insert-by-distance! [graph dlist index]
  (let* [head (:head dlist)
         tail (:tail dlist)
         node (get @(:nodes graph) index)
         distance (:distance node)]
    (if (dlist-empty? dlist)
      (dlist-prepend! dlist index)
      (dosync
        (if (<= @distance @(:distance (get @(:nodes graph) (:data @head))))
          (dlist-prepend! dlist index)
          (if (>= @distance @(:distance (get @(:nodes graph) (:data @tail))))
            (dlist-append! dlist index)
            (loop [dNode @head]
              (when (not (nil? dNode))
                (let* [dNode-next @(:next dNode)
                       nNode (DListPack. (ref dNode) index (ref dNode-next))
                       node-dNode (get @(:nodes graph) (:data dNode))
                       distance-dNode (:distance node-dNode)
                       node-dnext (get @(:nodes graph) (:data dNode-next))
                       distance-dnext (:distance node-dnext)]
                  (if (and (>= @distance @distance-dNode) (< @distance @distance-dnext))
                    (do
                      (ref-set (:next dNode) nNode)
                      (ref-set (:prev dNode-next) nNode))
                    (recur @(:next dNode))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;marking distance;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn prepare-finish-node [graph dlist finish]
  (reset-graph! graph)
  (let [index-finish (get-index-by-name graph finish)]
    (dlist-append! dlist index-finish)
    (dosync (ref-set (get-distance-from-index graph index-finish) 0))))

(defn set-distance! [graph neighbour current weighted]
  (let* [weight (if weighted (:weight neighbour) 1)
         distance-current (:distance current)
         distance-neighbour (get-distance-from-index graph (:index neighbour))
         distance-sum (+ @distance-current weight)]
    (when (< distance-sum @distance-neighbour)
      (dosync (ref-set distance-neighbour distance-sum)))) )

(defn graph-dijkstra-mark [graph dlist weighted]
  (loop []
    (when (not (empty? @(:head dlist)))
      (let* [current (get @(:nodes graph) (:data @(:head dlist)))]
        (when (= (:stat current) 1)
          (set-node-stat! graph (get-index-by-name graph (:name current)) 2))
        (dlist-remove-first! dlist)
        (doseq [neighbour @(:neighbours current)]
          (let [stat-neighbour @(:stat (get @(:nodes graph) (:index neighbour)))]
            (when (or (= stat-neighbour 0) (= stat-neighbour 1))
              (set-distance! graph neighbour current weighted))
            (when (= stat-neighbour 0)
              (insert-by-distance! graph dlist (:index neighbour))
              (set-node-stat! graph (:index neighbour) 1))))
        (set-node-stat! graph (get-index-by-name graph (:name current)) 3)
        (recur)))))

(defn is-connected? [graph name]
  (not (= @(:stat (get-node-by-name graph name)) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;main dijkstra;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn prepare-start-node [graph dlist start]
  (let [index-start (get-index-by-name graph start)]
    (dlist-prepend! dlist index-start)))

(defn graph-dijkstra-trace [graph dlist finish weighted]
  (loop [current (get @(:nodes graph) (:data @(:head dlist)))]
    (if (not (= (:name current) finish))
      (do
        (doseq [neighbour @(:neighbours current)]
          (let* [stat-neighbour @(:stat (get @(:nodes graph) (:index neighbour)))
                 distance-current (:distance current)
                 distance-neighbour (get-distance-from-index graph (:index neighbour))
                 weight (if weighted (:weight neighbour) 1)
                 difference (- @distance-current @distance-neighbour)]
            (when (= weight difference)
              (dlist-prepend! dlist (:index neighbour)))))
        (recur (get @(:nodes graph) (:data @(:head dlist))))))))

(defn shortest-path [graph dlist]
  (loop [dNode @(:head dlist)]
    (when (not (nil? dNode))
      (let [node (get @(:nodes graph) (:data dNode))]
        (println (:name node) "(" @(:distance node) ")")
        (recur @(:next dNode))))))

(defn graph-dijkstra [graph start finish weighted]
  (let [dlist (dlist-make)]
    (prepare-finish-node graph dlist finish)
    (graph-dijkstra-mark graph dlist weighted)
    (if (is-connected? graph start)
      (do (prepare-start-node graph dlist start)
          (graph-dijkstra-trace graph dlist finish weighted)
          (shortest-path graph dlist))
      (println start "is not connected with" finish))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;test;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "e-roads-2020-full.clj")