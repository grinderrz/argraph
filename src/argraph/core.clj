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
  "Returns a random integer between from (inclusive) and to (exclusive)."
  [from to]
  (-> to
      (- from)
      rand-int
      (+ from)))

(defn rand-tree-paths
  "Returns a vector of random paths in compact representation, containing N vertices.
   Vertices are consequtive integers from 0 up to N-1.
   Each path is looks like [connection vertice-from vertice-to], where
   - `connection` is a vertice from previous paths, so this path connects
   - `vertice-from` is the next unused integer
   - `vertice-to` is the end of this path."
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
  "Returns sequence of edges expanded from compact path representation.
   [connect-to from to] -> [[connect-to from] [from from+1] ...[to-1 to]]."
  [[connect-to from to]]
  (->> (inc to)
       (range (inc from))
       (cons connect-to)
       dedupe
       (partition 2 1)))

(defn edges->graph
  "Takes number of vertices and a sequence of directed edges.
   Assumes vertices are consecutive integers up to N-1.
   Returns weighted directed graph consisting on N vertices with given edges in our map representation. Weights are generated with `rand-weight`.
   (we could infer N from edges, but passing is more optimal)"
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
  "Given a graph with N vertices,
   returns a map of outgoing capacity (how much outgoing edges we could add)
   of each vertice."
  [N graph]
  (->> graph
       (map (fn [[k v]]
              [k (->> v count (- N 1))]))
       (filter #(< 0 (second %)))
       (into {})))

(defn fill-with-edges
  "Takes number of vertices, desired sparseness and a tree with N vertices.
   Returns a weighted graph with random edges added to initial tree.
   Avoids adding loops and duplicate edges."
  [N S T]
  (loop [g    T
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
  "Returns a graph with all vertices replaced with the result of f on them."
  [f g]
  (->> g
       (spr/transform [spr/MAP-KEYS] f)
       (spr/transform [spr/MAP-VALS spr/MAP-KEYS] f)))

(defn rand-graph
  "Returns a random weighted, weakly connected, simple directed graph
   of N vertices with sparseness S."
  [N S]
  {:pre [(< 0 N)
         (<= (dec N) S)
         (<= S (* N (dec N)))]}
  (->> (rand-tree-paths N)
       (mapcat path->edges)
       (map shuffle) ;; choose random directon
       (edges->graph N)
       (fill-with-edges N S)
       ;; up until here vertices are consecutive integers
       (update-vertices (comp keyword str))))

(defn collect-paths
  "Returns a map of vertice to shortest path to this vertice from `start` in `g` by Dijkstra's algorithm.
   If `end` is specified, stops as soon as `end` is visited."
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
  "Returns the shortest path from `start` to `end` in `g` by Dijkstra's algorithm."
  [g start end]
  (-> (collect-paths g start end)
      end
      second))

(defn eccentricity
  "Returns eccentricity of vertice `start` in `g`."
  [g start]
  (->> (collect-paths g start)
       (apply max-key #(-> % val first))
       val
       first))

(defn radius
  "Returns radius of `g`."
  [g]
  (->> g
       keys
       (map (partial eccentricity g))
       (apply min)))

(defn diameter
  "Returns diameter of `g`."
  [g]
  (->> g
       keys
       (map (partial eccentricity g))
       (apply max)))



(defn weakly-connected?
  "Checks if g is weakly connected."
  [g]
  (->> g
       (mapcat (fn [[k v]] ;; transform to sequence of edges
                 (map (partial into [k]) v)))
       (mapcat (fn [[f t w]] ;; for each edge add it's reverse, to make the graph like undirected
                 (list [f t w] [t f w])))
       (group-by first)
       (spr/transform ;; gather graph back
         [spr/MAP-VALS]
         #(into {} (map (fn [[_ to w]] [to w])
                        %)))
       diameter ;; now if graph is connected, each pair of vertices should have a path from one to another
       (not= ##Inf)))

(comment

  (def G {:1 {:2 1 :3 2},
          :2 {:4 3},
          :3 {:4 1},
          :4 {} })

  (seq-graph-dfs G :1)

  (seq-graph-bfs G :1)

  (->> (rand-graph 50 500)
       ;vals (map count) (apply +)
       ;(->>save G6)
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

  (repeatedly 10 (partial rand-int-in 5 6))

  (rand-int 0)

  (rand-graph 10 9)

  (->> (rand-graph 20 50)
       (->>save G2))

  (->> G2
       (update-vertices #(-> % name Integer/parseInt (+ 30) str keyword))
       (into G1)
       (mapcat (fn [[k v]]
                 (map (partial into [k]) v)))
       (mapcat (fn [[f t w]]
                 (list [f t w] [t f w])))
       (group-by first)
       (spr/transform
         [spr/MAP-VALS]
         #(into {} (map (fn [[_ to w]] [to w])
                        %)))
       diameter
       )

  (->> G2
       (update-vertices #(-> % name Integer/parseInt (+ 30) str keyword))
       (into G1)
       weakly-connected?
       )

  (apply shortest-path G1 (->> G1 keys shuffle (take 2)))

  )
