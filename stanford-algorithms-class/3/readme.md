# Problem Set 3

## Question 1

How many min cuts are there in a tree with `n` nodes (ie. `n−1` edges) ?

**Answer:** `n - 1`

**Explanation:** Each edge defines a distinct minimum cut (with one crossing edge).

## Question 2

Let "output" denote the cut output by Karger's min cut algorithm, and let `p=1/(n 2)` (*`n` over `two`*). Which of the following statements are true ? Hint: if you're having trouble with this question, you might want to watch the short optional video on "Counting Minimum Cuts".

- [ ] For every graph `G` with `n` nodes, there exists a min cut `(A,B)` such that `Pr[out=(A,B)]≤p`. (Recall the first problem of this problem set.)
- [ ] For every graph `G` with `n` nodes and every min cut `(A,B)`,` Pr[out=(A,B)]≤p`. (Recall the first problem of this problem set.)
- [X] For every graph `G` with `n` nodes, there exists a min cut `(A,B)` of `G` such that `Pr[out=(A,B)]≥p`. (This is even weaker than what we proved in the lecture video.)
- [X] For every graph `G` with `n` nodes and every min cut (`A,B)` of `G`, `Pr[out=(A,B)]≥p`. (This is exactly what we proved in the lecture video.)
- [X] There exists a graph `G` with `n` nodes and a min cut `(A,B)` of `G` such that `Pr[out=(A,B)]≤p`. (An `n`-cycle works.)

## Question 3

Let `0.5<α<1` be some constant. Suppose you are looking for the median element in an array using **RANDOMIZED SELECT** (as explained in lectures). What is the probability that after the first iteration the size of the sub-array in which the element you are looking for lies is `≤α` times the size of the original array?

**Answer:**

## Question 4

Let `0<α<1` be a constant, independent of `n`. Consider an execution of **RSelect** in which you always manage to throw out at least a `1−α` fraction of the remaining elements before you recurse. What is the maximum number of recursive calls you'll make before terminating?

**Answer:** `−log(n)/log(α)`

**Explanation:** 	Equivalently, `log` base `1/alpha` of `n`.

## Question 5

The minimum `s-t` cut problem is the following. The input is an undirected graph, and two distinct vertices of the graph are labelled "`s`" and "`t`". The goal is to compute the minimum cut (i.e., fewest number of crossing edges) that satisfies the property that `s` and `t` are on different sides of the cut.

Suppose someone gives you a subroutine for this `s-t` minimum cut problem via an API. Your job is to solve the original minimum cut problem (the one discussed in the lectures), when all you can do is invoke the given min `s-t` cut subroutine. (That is, the goal is to reduce the min cut problem to the min `s-t` cut problem.)

Now suppose you are given an instance of the minimum cut problem -- that is, you are given an undirected graph (with no specially labelled vertices) and need to compute the minimum cut. What is the minimum number of times that you need to call the given min `s-t` cut subroutine to guarantee that you'll find a min cut of the given graph?

**Answer:** `n - 1`

**Explanation:** Call an arbitrary vertex `s`, let `t` range over all other `n-1` vertices, and return the best of the `s-t` min cuts founds.

## Programming Question

The file contains the adjacency list representation of a simple undirected graph. There are `40` vertices labeled `1` to `40`. The first column in the file represents the vertex label, and the particular row (other entries except the first column) tells all the vertices that the vertex is adjacent to. So for example, the `6-th` row looks like : "`6 29 32 37 27 16`". This just means that the vertex with label `6` is adjacent to (i.e., shares an edge with) the vertices with labels `29`, `32`, `37`, `27` and `16`.

Your task is to code up and run the randomized contraction algorithm for the min cut problem and use it on the above graph to compute the min cut.

**HINT:** Note that you'll have to figure out an implementation of edge contractions. Initially, you might want to do this naively, creating a new graph from the old every time there's an edge contraction. But you also think about more efficient implementations.

**WARNING:** As per the video lectures, please make sure to run the algorithm many times with different random seeds, and remember the smallest cut that you ever find).

**Answer:** `3`
