# argraph

Generate and measure directed graphs.

## Random graph generation

It's required that graph is connected. But it's implied that shortest path is not always defined for any two vertices.
So we assume the (directed) graph should not be strongly connected, thus we generate weakly connected directed graph.

Lemma: Each weakly connected directed graph contains undirected (loosing direction) subtree.

We can prove it by induction. On each step we remove an edge while the graph remains is still not a tree.
As if it's not a tree, that it contains a cycle, so if we remove an edge on the cycle, the remaining graph is still connected.

We can then represent a tree as a set of paths (obvious to prove: just use edges as one step paths).

So to generate random graph guaranteeing covering of all the set of possible graphs, we could work backwards on the given graph dissection.
- We generate a random tree gradually adding paths connected to already generated part.
- We choose random directions for all vertices.
- We add random edges until reaching desired sparseness.

We rely on consecutive integers as vertices to ease generation.


## Usage

```clojure

(def G1 (rand-graph 20 50))
G1
;; {:0 {:1 2, ...}, ...}

(apply shortest-path G1 (->> G1 keys shuffle (take 2)))
;; [:1 :2 ...]

(eccentricity G1 (->> G1 keys shuffle first))
;; 10

(radius G1)
;; 5

(diameter G1)
;; 15

(weakly-connected? G1)
;; true

(->> G1 keys count (= 20))
;; true

(->> G1
     vals (map count) (apply +)
     (= 50))
;; true

```
