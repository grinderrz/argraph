(ns argraph.core
  (:require [com.rpl.specter :as spr]
            [clojure.data.priority-map :refer [priority-map-keyfn]]))

(def ^:dynamic *MAX-WEIGHT* 20)

(defn rand-weight [] (-> *MAX-WEIGHT* rand-int inc))

(defn seq-graph
  [d g s]
  ((fn rec-seq [explored frontier]
     (lazy-seq
       (if (empty? frontier)
         nil
         (let [v (peek frontier)
               neighbors (-> g v keys)]
           (cons v (rec-seq
                     (into explored neighbors)
                     (into (pop frontier) (remove explored neighbors))))))))
   #{s} (conj d s)))

(def seq-graph-dfs (partial seq-graph []))
(def seq-graph-bfs (partial seq-graph (clojure.lang.PersistentQueue/EMPTY)))

(defn rand-int-in
  [from to]
  (-> to
      (- from)
      rand-int
      (+ from)))

(defn rand-tree-paths
  [N]
  (loop [acc []
         i   0]
    (if (= i (dec N))
      acc
      (let [connection (rand-int i)
            more       (rand-int-in (inc i) N)]
        (recur (conj acc [connection i more])
               more)))))

(defn path->edges
  [[connect-to from to]]
  ;; [connect-to from to] -> [[connect-to from] [from from+1] ...[to-1 to]]
  (->> (inc to)
       (range (inc from))
       (cons connect-to)
       dedupe
       (partition 2 1)))

(defn edges->graph
  [N edges]
  (->> edges
       (group-by first)
       (spr/transform
         [spr/MAP-VALS]
         #(into {} (map (fn [[_ to]] [to (rand-weight)])
                        %)))
       (merge (zipmap (range N) ;; not all vertices have outgoing edges
                      (repeat {})))))

(defn capacities
  [N graph]
  (->> graph
       (map (fn [[k v]]
              [k (->> v count (- N 1))]))
       (filter #(< 0 (second %)))
       (into {})))

(defn fill-with-edges
  [N S G]
  (loop [g    G
         caps (capacities N g)
         left (- S (dec N))]
    (if (= left 0)
      g
      (let [from (->> caps keys shuffle first) ;; choose non full vertice
            to   (-> from g keys set
                     (conj from)        ;; avoid loops
                     (remove (range N)) ;; avoid duplicate edges
                     shuffle first)]
        (recur (spr/transform [from to]
                              (constantly (rand-weight))
                              g)
               (if (= (caps from) 1)
                 (dissoc caps from)
                 (update caps from dec))
               (dec left))))))

(defn update-vertices
  [f g]
  (->> g
       (spr/transform [spr/MAP-KEYS] f)
       (spr/transform [spr/MAP-VALS spr/MAP-KEYS] f)))

(defn rand-graph
  [N S]
  {:pre [(< 0 N)
         (<= (dec N) S)
         (<= S (* N (dec N)))]}
  (->> (rand-tree-paths N)
       (mapcat path->edges)
       (map shuffle)
       (edges->graph N)
       (fill-with-edges N S)
       (update-vertices (comp keyword str))))

(defn collect-paths
  ([g start] (collect-paths g start nil))
  ([g start end]
   (loop [unvisited (->> (dissoc g start)
                         (map (fn [[k v]]
                                [k [##Inf []]]))
                         (into (priority-map-keyfn first
                                                   start
                                                   [0 [start]])))
          visited   {}]
     (if-let [[distance path] (visited end)]
       visited
       (if-let [[current [distance path]] (peek unvisited)]
         (let [unvisited (pop unvisited)
               unvisited (->> (g current)
                              (filter #(->> % first (contains? visited) not))
                              (map (fn [[vertice weight]]
                                     (let [[d _] (unvisited vertice)]
                                       (when (and d
                                                  (< (+ distance weight) d))
                                         [vertice [(+ distance weight)
                                                   (conj path vertice)]]))))
                              (filter seq)
                              (into unvisited))]
           (recur
             unvisited
             (assoc visited current [distance path])))
         visited)))))

(defn shortest-path
  [g start end]
  (-> (collect-paths g start end)
      end
      second))

(defn eccentricity
  [g start]
  (->> (collect-paths g start)
       (apply max-key #(-> % val first))
       val
       first))

(defn radius
  [g]
  (->> g
       keys
       (map (partial eccentricity g))
       (apply min)))

(defn diameter
  [g]
  (->> g
       keys
       (map (partial eccentricity g))
       (apply max)))

(comment

  (def G {:1 {:2 1 :3 2},
          :2 {:4 3},
          :3 {:4 1},
          :4 {} })

  (seq-graph-dfs G :1)

  (seq-graph-bfs G :1)

  (->> (rand-graph 50 500)
       ;vals (map count) (apply +)
       (->>save G6)
       )

  (->> G3
       (map (fn [[k v]]
              [k (shortest-path G3 :1 k)])))

  (-> G3
      (collect-paths :0)
      (dissoc :13)
      (->> (apply max-key #(-> % val first))))

  (eccentricity G3 :13)
  (eccentricity G5 :2)

  (radius G6)
  (diameter G6)

  (->> G4
       keys
       (map (partial eccentricity G3))
       (apply min))

  (->> [1 2 3 4]
       (apply max))

  (let [g G2
        start :16
        end :2]
    (loop [unvisited (->> (dissoc G1 start)
                          (map (fn [[k v]]
                                 [k [##Inf []]]))
                          (into (priority-map-keyfn first
                                                    start
                                                    [0 [start]])))
           visited {}]
      (if-let [[distance path] (visited end)]
        path
        (if-let [[current [distance path]] (peek unvisited)]
          (let [_ (prn "0" visited)
                _ (prn "0.1" (visited end))
                _ (prn "1" [current [distance path]])
                _ (prn "2" (g current))
                _ (prn "3" unvisited)
                unvisited (pop unvisited)
                _ (prn "4" unvisited)
                unvisited (->> (g current)
                               (filter #(->> % first (contains? visited) not))
                               (map (fn [[v w]]
                                      (prn "4.1" [v w])
                                      (when-let [[d p] (unvisited v)]
                                        (prn "4.2" [d p])
                                        (if (< (+ distance w) d)
                                          [v [(+ distance w) (conj path v)]]
                                          [v [d p]]))))
                               (filter seq)
                               (into unvisited))
                _ (prn "5" unvisited)
                ]
            (recur
              unvisited
              (assoc visited current [current [distance path]])))
          []))))




  (-> (priority-map-keyfn first :1 [4 [1 2 3]] :2 [2 [4 5 6]])
      (assoc :3 [1 [7 8 9]])
      peek)


  (-> (spr/transform [spr/MAP-VALS] (constantly (vector ##Inf [])) G1)
      (update-in [:7 0] (constantly 0)))

  (->> (dissoc G1 :7)
       (map (fn [[k v]]
              [k [##Inf []]]))
       (into (priority-map-keyfn first :7 [0 [:7]]))
       (->>save PM1)
       )


  (< Integer/MAX_VALUE ##Inf)

  (vector 1 2 3)

  (if-let [[d p] ({:1 [4 [:2 :3]]} :1)]
    p
    "no")

  (->> (G1 :8)
       (filter #(->> % first (contains? {:6 [1 [2 3 4]]}) not))
       #_(map (fn [[k v]]
              (let [[d p] (PM1 k)]
                (if (< (+ 100 v) d)
                  [k [(+ 100 v) p]]
                  [k [d p]]))))
       #_(into PM1))

  (-> :17 G1 keys set
      (conj :17)
      ;(remove (range 30)) shuffle first
      )

  (let [[k v] ({:1 [:a :b]} :2)]
    [k v])

  ({:1 [:a :b]} nil )

(first nil)

  )
