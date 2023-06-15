(ns argraph.core
  (:require [com.rpl.specter :as spr]))

(def ^:dynamic *MAX-WEIGHT* 20)

(defn rand-weight []
  (-> *MAX-WEIGHT* rand-int inc))

(defn seq-graph [d g s]
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

(defn rand-int-in [from to]
  (-> to
      (- from)
      rand-int
      (+ from)))

(defn random-tree [N]
  (let [paths (loop [acc []
                     i   0]
                (if (= i (dec N))
                  acc
                  (let [connection (rand-int i)
                        more       (rand-int-in (inc i) N)]
                    (recur (conj acc [connection i more])
                           more))))]
    (mapcat (fn [[c f t]]
              ;; [conn from to] -> [[conn from] [from from+1] ...[to-1 to]]
              (->> (inc t)
                   (range (inc f))
                   (cons c)
                   dedupe
                   (partition 2 1)))
            paths)))

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

(defn fill-with-edges
  [N S G]
  (loop [g    G
         capacities (->> g
                         (map (fn [[k v]] [k (->> v count (- N 1))]))
                         (filter (fn [x] (-> x second (> 0))))
                         (into {}))
         left (- S (dec N))]
    (if (= left 0)
      g
      (let [from (->> capacities keys shuffle first) ;; choose non full vertice
            to   (-> from g keys set (remove (range 30)) shuffle first)] ;; avoid duplicate edges
        (recur (spr/transform [from to]
                              (constantly (rand-weight))
                              g)
               (if (= (capacities from) 1)
                 (dissoc capacities from)
                 (update capacities from dec))
               (dec left))))))



(defn rand-graph
  [N S]
  {:pre [(< 0 N)
         (<= (dec N) S)
         (<= S (* N (dec N)))]}
  (->> (random-tree N)
       (#(do (prn %) %))
       (map shuffle)
       (#(do (prn %) %))
       (edges->graph N)
       (#(do (prn %) %))
       (fill-with-edges N S)))

(comment

  (def G {:1 {:2 1 :3 2},
          :2 {:4 3},
          :3 {:4 1},
          :4 {} })

  (seq-graph-dfs G :1)

  (seq-graph-bfs G :1)

  (->> (rand-graph 20 379)
       vals (map count) (apply +)
       )

  (* 20 19)

  (->> (random-tree 20)
       (->>save t3)
       (map shuffle)
       (->>save tr2)
       (group-by first)
       (spr/transform [spr/MAP-VALS]
                      #(->> %
                            (map (fn [[_ to]]
                                   [to (rand-weight)]))
                            (into {})))
       (merge (zipmap (range 20) (repeat {})))
       )

  (->> *MAX-WEIGHT* rand-int inc (conj [3]))

  (->> (random-tree 30)
       (map shuffle)
       (edges->graph 30)
       (->>save g1)
       (#(let [N 30
              S 50]
          (loop [g          %
                 capacities (->> g
                                 (map (fn [[k v]] [k (->> v count (- N 1))]))
                                 (filter (fn [x] (-> x second (> 0))))
                                 (into {}))
                 left       (- S (dec N))]
            (if (= left 0)
              g
              (let [from (->> capacities keys shuffle first)
                    to   (-> from g keys set (remove (range 30)) shuffle first)]
                (recur (spr/transform [from to]
                                      (-> *MAX-WEIGHT*
                                          rand-int
                                          inc
                                          constantly)
                                      g)
                       (if (= (capacities from) 1)
                         (dissoc capacities from)
                         (update capacities from dec))
                       (dec left)))))))
       ;;(->>save gf1)
       )

  (-> 17 g1 keys set (remove (range 30)) shuffle first)

  (->> gf1
       vals (map count) (apply +)
       )

  (->> g1
       (map (fn [[k v]] [k (->> v count (- 30 1 27))]))
       (filter #(-> % second (> 0)))
       (into {})
       keys
       shuffle first
       )

  (-> 9 g1 (get 8) nil?)

  (->> g1
       (spr/transform [19 0] (constantly 1)))

  (->> [[0 0 1] [0 1 7] [6 7 8] [4 8 9]]
       (mapcat (fn [[c f t]]
                 (->> (inc t)
                      (range (inc f))
                      (cons c)
                      dedupe
                      (partition 2 1)))))

  (->> [1 2 3]
       shuffle)

  ((-> *MAX-WEIGHT* rand-int inc constantly) :a)

  (rand-int-in 2 4)

  (-> [1 2 3 4] (conj 5))


  ((fn [[c f t]]
              ;; [conn from to] -> [[conn from] [from from+1] ...[to-1 to]]
              (->> (inc t)
                   (range (inc f))
                   (cons c)
                   dedupe
                   (partition 2 1)))
   [0 4 8])

  )
