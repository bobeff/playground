# Problem Set 6

## Question 1

We are given as input a set of `n` requests (e.g., for the use of an auditorium), with a known start time `si` and finish time `ti` for each request `i`. Two requests conflict if they overlap in time --- if one of them starts strictly between the start and finish times of the other. Our goal is to select a maximum-size subset of the given requests that contains no conflicts. We aim to design a greedy algorithm for this problem with the following form: At each iteration we select a new request `i`, including it in the solution-so-far and deleting from future consideration all requests that conflict with `i`. Which of the following greedy rules is guaranteed to always compute an optimal solution?

**Answer:** At each iteration, pick the remaining request with the earliest finish time (breaking ties arbitrarily).

**Explanation:** Let `Rj` denote the requests with the `j` earliest finish times. Prove by induction on `j` that this greedy algorithm selects the maximum-number of non-conflicting requests from `Sj`.

## Question 2

We are given as input a set of `n` jobs, where job `j` has a processing time `pj` and a deadline `dj`. Recall the definition of completion times `Cj` from the video lectures. Given a schedule (i.e., an ordering of the jobs), we define the lateness `lj` of job `j` as the amount of time `Cj−dj` after its deadline that the job completes, or as `0` if `Cj≤dj`. Our goal is to minimize the maximum lateness, `maxj(lj)`. Which of the following greedy rules produces an ordering that minimizes the maximum lateness? You can assume that all processing times and deadlines are distinct.

**Answer:** Schedule the requests in increasing order of deadline `dj`

**Explanation:** Proof by an exchange argument, analogous to minimizing the weighted sum of completion times.

## Question 3

Consider an undirected graph `G=(V,E)` where every edge `e∈E` has a given cost `ce`. Assume that all edge costs are positive and distinct. Let `T` be a minimum spanning tree of `G` and `P` a shortest path from the vertex `s` to the vertex `t`. Now suppose that the cost of every edge `e` of `G` is increased by `1` and becomes `ce+1`. Call this new graph `G'`. Which of the following is true about `G'`?

**Answer:** `T` must be a minimum spanning tree but `P` may not be a shortest `s-t` path.

**Explanation:** The positive statement has many proofs (e.g., via the Cut Property). For the negative statement, think about two different paths from `s` to `t` that contain a different number of edges.

## Question 4

Suppose `T` is a minimum spanning tree of the graph `G`. Let `H` be an induced subgraph of `G`. (I.e., `H` is obtained from `G` by taking some subset `S⊆V` of vertices, and taking all edges of `E` that have both endpoints in `S`.) Which of the following is true about the edges of `T` that lie in `H`? You can assume that edge costs are distinct, if you wish.

**Answer:** They are always contained in some minimum spanning tree of `H`.

**Explanation:** Proof via the **Cut Property** (cuts in `G` correspond to cuts in `H` with only fewer crossing edges).

## Question 5

Consider an undirected graph `G=(V,E)` where edge `e∈E` has cost `ce`. A minimum bottleneck spanning tree `T` is a spanning tree that minimizes the maximum edge cost `max e∈T(ce)`. Which of the following statements is true? Assume that the edge costs are distinct.

**Answer:** A minimum bottleneck spanning tree is not always a minimum spanning tree, but a minimum spanning tree is always a minimum bottleneck spanning tree.

**Explanation:** For the positive statement, recall the following (from correctness of **Prim's algorithm**): for every edge `e` of the `MST`, there is a cut `(A,B)` for which `e` is the cheapest one crossing it. This implies that every other spanning tree has maximum edge cost at least as large. For the negative statement, use a triangle with one extra high-cost edge attached.

## Programming Question 1

In this programming problem and the next you'll code up the greedy algorithms from lecture for minimizing the weighted sum of completion times. Download the text file [here](http://spark-public.s3.amazonaws.com/algo2/datasets/jobs.txt). This file describes a set of jobs with positive and integral weights and lengths. It has the format

```
[number_of_jobs]
[job_1_weight] [job_1_length]
[job_2_weight] [job_2_length]
...
```

For example, the third line of the file is "`74 59`", indicating that the second job has weight `74` and length `59`. You should NOT assume that edge weights or lengths are distinct.

Your task in this problem is to run the greedy algorithm that schedules jobs in decreasing order of the difference (`weight - length`). Recall from lecture that this algorithm is not always optimal.

**IMPORTANT:** if two jobs have equal difference (`weight - length`), you should schedule the job with higher weight first. Beware: if you break ties in a different way, you are likely to get the wrong answer. You should report the sum of weighted completion times of the resulting schedule --- a positive integer.

**ADVICE:** If you get the wrong answer, try out some small test cases to debug your algorithm.

**Answer:** `69119377652`

## Programming Question 2

For this problem, use the same data set as in the previous problem. Your task now is to run the greedy algorithm that schedules jobs (optimally) in decreasing order of the ratio (`weight/length`). In this algorithm, it does not matter how you break ties. You should report the sum of weighted completion times of the resulting schedule --- a positive integer.

**Answer:** `67311454237`

## Programming Question 3

In this programming problem you'll code up Prim's minimum spanning tree algorithm. Download the text file [here](http://spark-public.s3.amazonaws.com/algo2/datasets/edges.txt). This file describes an undirected graph with integer edge costs. It has the format

```
[number_of_nodes] [number_of_edges]
[one_node_of_edge_1] [other_node_of_edge_1] [edge_1_cost]
[one_node_of_edge_2] [other_node_of_edge_2] [edge_2_cost]
...
```

For example, the third line of the file is "`2 3 -8874`", indicating that there is an edge connecting vertex `#2` and vertex `#3` that has cost `-8874`. You should NOT assume that edge costs are positive, nor should you assume that they are distinct.

Your task is to run **Prim's minimum spanning tree algorithm** on this graph. You should report the overall cost of a minimum spanning tree --- an integer, which may or may not be negative.

**IMPLEMENTATION NOTES:** This graph is small enough that the straightforward `O(m.n)` time implementation of **Prim's algorithm** should work fine.

**OPTIONAL:** For those of you seeking an additional challenge, try implementing a heap-based version. The simpler approach, which should already give you a healthy speed-up, is to maintain relevant edges in a heap (with keys = edge costs). The superior approach stores the unprocessed vertices in the heap, as described in lecture. Note this requires a heap that supports deletions, and you'll probably need to maintain some kind of mapping between vertices and their positions in the heap.

**Answer:** `-3612829`
