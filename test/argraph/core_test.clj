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
    (tc/quick-check
      100
      (prop/for-all
        [[N S] (gen/bind gen/nat
                         (fn [n]
                           (gen/tuple
                             (gen/return (inc n))
                             (gen/choose n (* (inc n) n)))))]
        (let [g (rand-graph N S)]
          (and (->> g keys count (= N))
               (->> g vals (map count) (apply +) (= S))))))))

(deftest test-weakly-connected?
  (testing "weakly-connected? works as expected."
    (tc/quick-check
      100
      (prop/for-all
        [[N1 S1] (gen/bind gen/nat
                           (fn [n]
                             (gen/tuple
                               (gen/return (inc n))
                               (gen/choose n (* (inc n) n)))))
         [N2 S2] (gen/bind gen/nat
                           (fn [n]
                             (gen/tuple
                               (gen/return (inc n))
                               (gen/choose n (* (inc n) n)))))]
        (let [G1 (rand-graph N1 S1)
              G2 (rand-graph N2 S2)]
          (and (->> G2
                    (update-vertices #(-> % name Integer/parseInt
                                          (+ N2 10) ;; make vertices different
                                          str keyword))
                    (into G1)
                    weakly-connected?
                    not)
               (weakly-connected? G1)
               (weakly-connected? G2)))))))
