# Blanché-Bonamy-Bonichon algorithm for path-decomposition of planar graphs

## Introduction

A path-decomposition of a graph is a partition of its edges into simple paths. In 1968, Tibor Gallai conjectured that any connected graph with n vertices has a path-decomposition into ceiling(n/2) paths. The conjecture is still open as of today.
For more information, my PhD thesis: https://www.labri.fr/perso/ablanche/data/Manuscript.pdf

A planar graph is one that can be embedded (or drawn) in the plane without edge-crossings.
In 2021, my PhD advisors, Marthe Bonamy and Nicolas Bonichon, and I proved that Gallai'sconjecture holds on planar graphs.
Our paper: https://arxiv.org/abs/2110.08870

## Summary of the proof

We prove a stronger version of the result: apart from two exceptions, K3 and K5- (K5 minus one edge), all connected planar graphs have a path-decomposition into floor(n/2) paths.

Our proof operates by considering a hypothetical counterexample to the conjecture: a connected planar graph that does not have a path-decomposition into floor(n/2) paths. We consider such a counterexample of minimum size. This way, whenever we delete some vertices (2 or 4), the remaining graph has a "good" decomposition (if it is connected). If we can use this decomposition to build a decomposition of the whole graph with the help of (1 or 2) new colors, we say that the configuration of the vertices we deleted is "reducible".

To prove our result, we operate in three steps:
- First, we prove that our minimum counterexample cannot have two vertices of degree at most 4 linked by a path (a "CI-configuration"), by showing that each such case corresponds to a reducible configuration;
- Then, we prove that our minimum counterexample cannot have four vertices of degree 5 linked by a K4-subdivision (a "CII-configuration"), again by showing that each such case corresponds to a reducible configuration;
- Finally, we prove that a minimum counterexample with such properties cannot exist, by showing a contradiction with Euler's formula.

To reduce a configuration, we remove the 2 or 4 vertices, along with the edges of the path or K4-subdivision. The resulting "reduced" graph is made up of connected components: the ones isomorphic to K3 and K5- are decomposed into one cycle or one path and one cycle, the others into a good decomposition. Then one (for 2 vertices) or two (for 4 vertices) paths are added, the existing paths are redirected (in a way described in each reduction rule), and a decomposition of the whole graph is built, into paths and cycles. The cycles that remain are combined with some paths and turned into a pair of paths (as explained in the proof of a lemma by Chu, Fan, Liu (2021)). A good path-decomposition is thus built, which contradicts the nature of the counterexample.


## Program

In this project, I attempt to code the algorithm we implicitly describe in our proof. The proof 95 pages long, but it is constructive. However, two parts of it will be especially tricky to implement: the use of a proof by Yu (1998) to prove the existence of a K4-subdivision in our "almost 4-connected" planar graph, and a lemma by Chu, Fan, Liu (2021) to show how to decompose our K3 and K5- components into 2 paths.
I am looking for alternative algorithms for these parts.

### Files

- graph_types.ml: Definition of the basic types for graphs, along with their accessors and mutators, conversion functions between types, and miscellaneous graph-related functions.
- miscellaneous.ml: Miscellaneous non-graph-related functions.
- dijkstra.ml: Dijkstra's algorithm
