# argraph

Some graph algorithms

## Random graph generation

It's required that graph is connected. But it's implied that shortest path is not always defined for any two vertices.
So we assume the (directed) graph should not be strongly connected, thus we generate weakly connected directed graph.

Lemma: Each weakly connected directed graph contains undirected (loosing direction) subtree.

We can prove it by induction. On each step we remove an edge while the graph remains is still not a tree.
As if it's not a tree, that it contains a cycle, so if we remove an edge on the cycle, the remaining graph is still connected.

We can then represent a tree as a set of paths (obvious to prove: just use edges as one step paths).

So to generate random graph guaranteeing covering of all the set of possible graphs, we could work backwards on the given graph dissection.
We generate a random tree gradually adding paths connected to already generated part.
We choose random directions for all vertices.
We add random edges until reaching desired sparseness.


## Usage

FIXME

## License

Copyright © 2023 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
