# Blanché-Bonamy-Bonichon algorithm for path-decomposition of planar graphs

## Introduction

A path-decomposition of a graph is a partition of its edges into simple paths. In 1968, Tibor Gallai conjectured that any connected graph with n vertices has a path-decomposition into ceiling(n/2) paths. The conjecture is still open as of today.
For more info, my PhD thesis: https://www.labri.fr/perso/ablanche/data/Manuscript.pdf

A planar graph is one that can be embedded (or drawn) in the plane without edge-crossings.
In 2021, my PhD advisors, Marthe Bonamy and Nicolas Bonichon, and I proved that Gallai'sconjecture holds on planar graphs.
Our paper: https://arxiv.org/abs/2110.08870

## Program

In this project, I attempt to code the algorithm we implicitly describe in our proof. The proof 95 pages long, but it is constructive. However, two parts of it will be especially tricky to implement: the use of a proof by Yu (1998) to prove the existence of a K4-subdivision in our "almost 4-connected" planar graph, and a lemma by Chu, Fan, Liu (2021) to show how to decompose our K3 and K5- components into 2 paths.
