(ns argraph.core-test
  (:require [clojure.test :refer :all]
            [argraph.core :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :as ct]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))


(deftest test-rand-int
  (testing "rand-int-in generates in the interval."
    (prop/for-all [[f t] (->> (gen/tuple gen/nat gen/nat)
                              (gen/fmap sort))]
      (let [r (rand-int-in f t)]
        (if (= f t)
          (= r t)
          (and (<= f r) (< r t)))))))

(deftest test-rand-graph
  (testing "Any generated graph has correct number of vertices and edges."
    (binding [ct/*report-trials* true]
      (tc/quick-check
        100
        (prop/for-all
          [[N S] (gen/bind gen/nat
                           (fn [n]
                             (gen/tuple
                               (gen/return (inc n))
                               (gen/choose n (* (inc n) n)))))]
          (let [g (rand-graph N S)]
            (and (< 0 N)
                 (<= (dec N) S)
                 (<= S (* N (dec N)))
                 (->> g keys count (= N))
                 (->> g
                      vals (map count) (apply +)
                      (= S)))))))))
