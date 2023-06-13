(ns argraph.core
  (:require [com.rpl.specter :as spr]))

(def ^:dynamic *MAX-WEIGHT* 20)

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
                     i 0]
                (if (= i (dec N))
                  acc
                  (let [connection (rand-int i)
                        more (rand-int-in (inc i) N)]
                    (recur (conj acc [connection i more])
                           more))))]
    (prn paths)
    (->> paths
         (mapcat (fn [[c f t]]
                   (->> (inc t)
                        (range (inc f))
                        (cons c)
                        dedupe
                        (partition 2 1)))))))

(defn random-swap
  [[a b]]
  (if (= 0 (rand-int 2))
    [a b]
    [b a]))

(defn edges->graph
  [edges]
  (->> edges
       (map random-swap)
       (group-by first)
       (spr/transform [spr/MAP-VALS]
                      #(->> %
                            (map (fn [[_ to]]
                                   (->> *MAX-WEIGHT*
                                        rand-int
                                        inc
                                        (conj [to]))))
                            (into {})))
       (merge (zipmap (range 20) (repeat {})))))


(comment

  (def G {:1 {:2 1 :3 2},
          :2 {:4 3},
          :3 {:4 1},
          :4 {} })

  (seq-graph-dfs G :1)

  (seq-graph-bfs G :1)


  (->> (random-tree 20)
       (->>save t3)
       (map random-swap)
       (->>save tr2)
       (group-by first)
       (spr/transform [spr/MAP-VALS]
                      #(->> %
                            (map (fn [[_ to]]
                                   (->> *MAX-WEIGHT*
                                        rand-int
                                        inc
                                        (conj [to]))))
                            (into {})))
       (merge (zipmap (range 20) (repeat {})))
       )

  (->> *MAX-WEIGHT* rand-int inc (conj [3]))

  (->> (random-tree 30)
       edges->graph)

  (->> [[0 0 1] [0 1 7] [6 7 8] [4 8 9]]
       (mapcat (fn [[c f t]]
                 (->> (inc t)
                      (range (inc f))
                      (cons c)
                      dedupe
                      (partition 2 1)))))


  (rand-int-in 2 4)

  (-> [1 2 3 4] (conj 5))

  )
