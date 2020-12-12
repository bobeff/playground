# Problem Set 8

## Question 1

Which of the following is true for our dynamic programming algorithm for computing a maximum-weight independent set of a path graph? (Assume there are no ties.)

**Answer:** If a vertex is excluded from the optimal solution of two consecutive sub-problems, then it is excluded from the optimal solutions of all bigger sub-problems.

**Explanation:** By induction, since the optimal solution to a subproblem depends only on the solutions of the previous two sub-problems.

## Question 2

Consider a variation of the **Knapsack** problem where we have two knapsacks, with integer capacities `W1` and `W2`. As usual, we are given `n` items with positive values and positive integer weights. We want to pick subsets `S1`, `S2` with maximum total value (i.e., `∑i∈S1(vi)+∑i∈S2(vi)`) such that the total weights of `S1` and `S2` are at most `W1` and `W2`, respectively. Assume that every item fits in either knapsack (i.e., `wi≤min{W1,W2}` for every item `i`). Consider the following two algorithmic approaches.

* (1) Use the algorithm from lecture to pick a a max-value feasible solution `S1` for the first knapsack, and then run it again on the remaining items to pick a max-value feasible solution `S2` for the second knapsack.
* (2) Use the algorithm from lecture to pick a max-value feasible solution for a knapsack with capacity `W1+W2`, and then split the chosen items into two sets `S1`, `S2` that have size at most `W1` and `W2`, respectively. Which of the following statements is true?

**Answer:** Neither algorithm is guaranteed to produce an optimal feasible solution to the original problem.

**Explanation:** Indeed. Can you devise from scratch a dynamic programming algorithm that correctly solves the problem?

## Question 3

Recall our dynamic programming algorithm for computing the maximum-weight independent set of a path graph. Consider the following proposed extension to more general graphs. Consider an undirected graph with positive vertex weights. For a vertex `v`, obtain the graph `G'(v)` by deleting `v` and its incident edges from `G`, and obtain the graph `G''(v)` from `G` by deleting `v`, its neighbors, and all of the corresponding incident edges from `G`. Let `OPT(H)` denote the value of a maximum-weight independent set of a graph `H`. Consider the formula `OPT(G)=max{OPT(G'(v)),wv+OPT(G''(v))}`, where `v` is an arbitrary vertex of `G` of weight `wv`. Which of the following statements is true?

**Answer:** The formula is always correct in trees, and it leads to an efficient dynamic programming algorithm.

**Explanation:** Indeed. What running time can you get?

## Question 4

Recall the dynamic programming algorithms from lecture for the **Knapsack** and sequence alignment problems. Both fill in a two-dimensional table using a double-for loop. Suppose we reverse the order of the two for loops. Are the resulting algorithms still well defined and correct?

**Answer:** Both algorithms remain well defined and correct after reversing the order of the for loops.	

**Explanation:** The necessary subproblem solutions are still available for constant-time lookup.

## Question 5

Consider an instance of the optimal binary search tree problem with `7` keys (say `1`, `2`, `3`, `4`, `5`, `6`, `7` in sorted order) and frequencies `w1=0.05`, `w2=0.4`, `w3=0.08`, `w4=0.04`, `w5=0.1`, `w6=0.1`, `w7=0.23`. What is the minimum-possible average search time of a binary search tree with these keys?

**Answer:** 2.18

## Programming Question 1

In this programming problem and the next you'll code up the **knapsack** algorithm from lecture. Let's start with a warm-up. Download the text file [here](http://spark-public.s3.amazonaws.com/algo2/datasets/knapsack1.txt). This file describes a **knapsack** instance, and it has the following format:

```
[knapsack_size][number_of_items]
[value_1] [weight_1]
[value_2] [weight_2]
...
```

For example, the third line of the file is "`50074 659`", indicating that the second item has value `50074` and size `659`, respectively.

You can assume that all numbers are positive. You should assume that item weights and the knapsack capacity are integers.

In the box below, type in the value of the optimal solution.

**ADVICE:** If you're not getting the correct answer, try debugging your algorithm using some small test cases.

**Answer:** `2493893`

## Programming Question 2

This problem also asks you to solve a knapsack instance, but a much bigger one.
Download the text file [here](http://spark-public.s3.amazonaws.com/algo2/datasets/knapsack2.txt). This file describes a knapsack instance, and it has the following format:

```
[knapsack_size][number_of_items]
[value_1] [weight_1]
[value_2] [weight_2]
...
```

For example, the third line of the file is "`50074 834558`", indicating that the second item has value `50074` and size `834558`, respectively. As before, you should assume that item weights and the knapsack capacity are integers.

This instance is so big that the straightforward iterative implementation uses an infeasible amount of time and space. So you will have to be creative to compute an optimal solution. One idea is to go back to a recursive implementation, solving sub-problems --- and, of course, caching the results to avoid redundant work --- only on an "as needed" basis. Also, be sure to think about appropriate data structures for storing and looking up solutions to sub-problems.

In the box below, type in the value of the optimal solution.

**ADVICE:** If you're not getting the correct answer, try debugging your algorithm using some small test cases.

**Answer:** `2595819`
