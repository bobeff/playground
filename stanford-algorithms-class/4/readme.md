# Problem Set 4

## Question 1

Given an adjacency-list representation of a directed graph, where each vertex maintains an array of its outgoing edges (but *not* its incoming edges), how long does it take, in the worst case, to compute the in-degree of a given vertex? As usual, we use `n` and `m` to denote the number of vertices and edges, respectively, of the given graph. Also, let `k` denote the maximum in-degree of a vertex. (Recall that the in-degree of a vertex is the number of edges that enter it.)

**Answer:** `θ(m)`

**Explanation:** 	Without explicitly maintaining a list of incoming edges, you might have to scan all the edges to identify the incoming arcs.

## Question 2

Consider the following problem: given an undirected graph `G` with `n` vertices and `m` edges, and two vertices `s` and `t`, does there exist at least one `s-t` path?

If `G` is given in its adjacency list representation, then the above problem can be solved in `O(m+n)` time, using `BFS` or `DFS`. (Make sure you see why this is true.)

Suppose instead that `G` is given in its adjacency **matrix** representation. What running time is required, in the worst case, to solve the computational problem stated above? (Assume that `G` has no parallel edges.)

**Answer:** `θ(n^2)`

**Explanation:** For the lower bound, observe that you might need to look at every entry of the adjacency matrix (e.g., if it has only one "1" and the rest are zeroes). One easy way to prove the upper bound is to first build an adjacency list representation (in `Θ(n^2)` time, with a single scan over the given adjacency matrix) and then run `BFS` or `DFS` as in the video lectures.

**Question Explanation:** Hint: for the lower bound, consider the special case in which `G` has only one edge.

## Question 3

This problem explores the relationship between two definitions about graph distances. In this problem, we consider only graphs that are undirected and connected. The diameter of a graph is the maximum, over all choices of vertices `s` and `t`, of the shortest-path distance between `s` and `t`. (Recall the shortest-path distance between `s` and `t` is the fewest number of edges in an `s-t` path.)

Next, for a vertex `s`, let `l(s)` denote the maximum, over all vertices `t`, of the shortest-path distance between `s` and `t`. The radius of a graph is the minimum of `l(s)` over all choices of the vertex `s`.

Which of the following inequalities always hold (i.e., in every undirected connected graph) for the radius r and the diameter `d`? [Select all that apply.]

- [ ] `r≥d` (A path is a counterexample.)
- [X] `r≥d/2` (Let `c` minimize `l(s)` over all vertices `s`. Since every pair of vertices `s` and `t` have paths to `c` with at most `r` edges, stitching these paths together yields an `s-t` with only `2.r` edges; of course, the shortest `s-t` path is only shorter.)
- [X] `r≤d` (By the definitions, `l(s)≤d` for every single choice of `s`.)
- [ ] `r≤d/2` (The triangle is a counterexample.)

## Question 4

Consider our algorithm for computing a topological ordering that is based on depth-first search (i.e., NOT the "straightforward solution"). Suppose we run this algorithm on a graph `G` that is NOT directed acyclic. Obviously it won't compute a topological order (since none exist). Does it compute an ordering that minimizes the number of edges that go backward? For example, consider the four-node graph with the six directed edges `(s,v)`, `(s,w)`, `(v,w)`, `(v,t)`, `(w,t)`, `(t,s)`. Suppose the vertices are ordered `s`, `v`, `w`, `t`. Then there is one backwards arc, the `(t,s)` arc. No ordering of the vertices has zero backwards arcs, and some have more than one.

**Answer:** Sometimes yes, sometimes no

## Question 5

On adding one extra edge to a directed graph `G`, the number of strongly connected components...?

**Answer:** ...might remain the same

**Explanation:** For example, the graph might already be strongly connected.

## Programming Question

Download the text file [here](http://spark-public.s3.amazonaws.com/algo1/programming_prob/SCC.txt). Zipped version [here](http://spark-public.s3.amazonaws.com/algo1/programming_prob/SCC.zip).

The file contains the edges of a directed graph. Vertices are labeled as positive integers from `1` to `875714`. Every row indicates an edge, the vertex label in first column is the tail and the vertex label in second column is the head (recall the graph is directed, and the edges are directed from the first column vertex to the second column vertex). So for example, the `11-th` row looks like : "`2 47646`". This just means that the vertex with label `2` has an outgoing edge to the vertex with label `47646`.

Your task is to code up the algorithm from the video lectures for computing strongly connected components (**SCCs**), and to run this algorithm on the given graph.

**Output Format:** You should output the sizes of the `5` largest **SCCs** in the given graph, in decreasing order of sizes, separated by commas (avoid any spaces). So if your algorithm computes the sizes of the five largest **SCCs** to be `500`, `400`, `300`, `200` and `100`, then your answer should be "`500,400,300,200,100`". If your algorithm finds less than `5` **SCCs**, then write `0` for the remaining terms. Thus, if your algorithm computes only `3` **SCCs** whose sizes are `400`, `300`, and `100`, then your answer should be "`400,300,100,0,0`".

**Answer:** `434821,968,459,313,211`
