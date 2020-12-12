# Problem Set 7

## Question 1

Suppose we are given a directed graph `G=(V,E)` in which every edge has a distinct positive edge weight. A directed graph is acyclic if it has no directed cycle. Suppose that we want to compute the maximum-weight acyclic subgraph of `G` (where the weight of a subgraph is the sum of its edges' weights). Assume that `G` is weakly connected, meaning that there is no cut with no edges crossing it in either direction.

Here is an analog of **Prim's algorithm** for directed graphs. Start from an arbitrary vertex `s`, initialize `S={s}` and `F=∅`. While `S≠V`, find the maximum-weight edge `(u,v)` with one endpoint in `S` and one endpoint in `V−S`. Add this edge to `F`, and add the appropriate endpoint to `S`.

Here is an analog of **Kruskal's algorithm**. Sort the edges from highest to lowest weight. Initialize `F=∅`. Scan through the edges; at each iteration, add the current edge `i` to `F` if and only if it does not create a directed cycle. Which of the following is true?

**Answer:** Both algorithms might fail to compute a maximum-weight acyclic subgraph.

**Explanation:** Indeed. Any ideas for a correct algorithm?

## Question 2

Consider a connected undirected graph `G` with edge costs that are not necessarily distinct. Suppose we replace each edge cost `ce` by `−ce`; call this new graph `G'`. Consider running either **Kruskal's** or **Prim's** minimum spanning tree algorithm on `G'`, with ties between edge costs broken arbitrarily, and possibly differently, in each algorithm. Which of the following is true?

**Answer:** Both algorithms compute a maximum-cost spanning tree of `G`, but they might compute different ones.

**Explanation:** 	Different tie-breaking rules generally yield different spanning trees.

## Question 3

Consider the following algorithm that attempts to compute a minimum spanning tree of a connected undirected graph `G` with distinct edge costs. First, sort the edges in decreasing cost order (i.e., the opposite of **Kruskal's** algorithm). Initialize `T` to be all edges of `G`. Scan through the edges (in the sorted order), and remove the current edge from `T` if and only if it lies on a cycle of `T`.

Which of the following statements is true?

**Answer:** The algorithm always outputs a minimum spanning tree.

**Explanation:** During the iteration in which an edge is removed, it was on a cycle `C` of `T`. By the sorted ordering, it must be the maximum-cost edge of `C`. By an exchange argument, it cannot be a member of any minimum spanning tree. Since every edge deleted by the algorithm belongs to no **MST**, and its output is a spanning tree (no cycles by construction, connected by the **Lonely Cut Corollary**), its output must be the (unique) **MST**.

## Question 4

Consider an alphabet with five letters, `{a,b,c,d,e}`, and suppose we know the frequencies `fa=0.32`, `fb=0.25`, `fc=0.2`, `fd=0.18`, and `fe=0.05`. What is the expected number of bits used by **Huffman's** coding scheme to encode a `1000`-letter document?

**Answer:** 2230

**Explanation:** 	For example, `a=00`, `b=01`, `c=10`, `d=110`, `e=111`

## Question 5

Which of the following statements holds for Huffman's coding scheme?

**Answer:** If the most frequent letter has frequency less than `0.33`, then all letters will be coded with at least two bits.

**Explanation:** Such a letter will endure a merge in at least two iterations: the last one (which involves all letters), and at least one previous iteration. In the penultimate iteration, if the letter has not yet endured a merge, at least one of the two other remaining subtrees has cumulative frequency at least `(1−0.33)/2>0.33`, so the letter will get merged in this iteration.

## Programming Question 1

In this programming problem and the next you'll code up the clustering algorithm from lecture for computing a max-spacing `k`-clustering. Download the text file [here](http://spark-public.s3.amazonaws.com/algo2/datasets/clustering1.txt). This file describes a distance function (equivalently, a complete graph with edge costs). It has the following format:

```
[number_of_nodes]
[edge 1 node 1] [edge 1 node 2] [edge 1 cost]
[edge 2 node 1] [edge 2 node 2] [edge 2 cost]
...
```

There is one edge `(i,j)` for each choice of `1≤i<j≤n`, where `n` is the number of nodes. For example, the third line of the file is "`1 3 5250`", indicating that the distance between nodes `1` and `3` (equivalently, the cost of the edge `(1,3)`) is `5250`. You can assume that distances are positive, but you should NOT assume that they are distinct.

Your task in this problem is to run the clustering algorithm from lecture on this data set, where the target number `k` of clusters is set to `4`. What is the maximum spacing of a `4`-clustering?

**ADVICE:** If you're not getting the correct answer, try debugging your algorithm using some small test cases.

**Answer:** `106`

## Programming Question 2

In this question your task is again to run the clustering algorithm from lecture, but on a MUCH bigger graph. So big, in fact, that the distances (i.e., edge costs) are only defined implicitly, rather than being provided as an explicit list.

The data set is [here](http://spark-public.s3.amazonaws.com/algo2/datasets/clustering2.txt). The format is:

```
[# of nodes] [# of bits for each node's label]
[first bit of node 1] ... [last bit of node 1]
[first bit of node 2] ... [last bit of node 2]
...
```

For example, the third line of the file "`0 1 1 0 0 1 1 0 0 1 0 1 1 1 1 1 1 0 1 0 1 1 0 1`" denotes the `24` bits associated with node `#2`.

The distance between two nodes `u` and `v` in this problem is defined as the **Hamming distance**--- the number of differing bits --- between the two nodes' labels. For example, the **Hamming distance** between the `24`-bit label of node `#2` above and the label "`0 1 0 0 0 1 0 0 0 1 0 1 1 1 1 1 1 0 1 0 0 1 0 1`" is `3` (since they differ in the `3-rd`, `7-th`, and `21-st` bits).

The question is: what is the largest value of `k` such that there is a `k`-clustering with spacing at least `3`? That is, how many clusters are needed to ensure that no pair of nodes with all but `2` bits in common get split into different clusters?

**NOTE:** The graph implicitly defined by the data file is so big that you probably can't write it out explicitly, let alone sort the edges by cost. So you will have to be a little creative to complete this part of the question. For example, is there some way you can identify the smallest distances without explicitly looking at every pair of nodes?

**Answer:** `16508`