# Problem Set 5

## Question 1

Consider a directed graph with distinct and nonnegative edge lengths and a source vertex `s`. Fix a destination vertex `t`, and assume that the graph contains at least one `s-t` path. Which of the following statements are true?

* [ ] The shortest `s-t` path must include the minimum-length edge of `G`.
* [X] The shortest (i.e., minimum-length) `s-t` path might have as many as `n−1` edges, where `n` is the number of vertices.
* [X] There is a shortest `s-t` path with no repeated vertices (i.e., a "simple" or "loopless" such path).
* [ ] The shortest `s-t` path must exclude the maximum-length edge of `G`.

## Question 2

Consider a directed graph `G=(V,E)` and a source vertex `s` with the following properties: edges that leave the source vertex `s` have arbitrary (possibly negative) lengths; all other edge lengths are nonnegative; and there are no edges from any other vertex to the source `s`. Does Dijkstra's shortest-path algorithm correctly compute shortest-path distances (from s) in this graph?

**Answer:** Always

**Explanation:** One approach is to see that the proof of correctness from the videos still works. A slicker solution is to notice that adding a positive constant `M` to all edges incident to `s` increases the length of every `s-v` path by exactly `M`, and thus preserves the shortest path.

## Question 3

Suppose we use a hash function `h` to hash `n` distinct keys into an array `T` of length `m`. Assuming simple uniform hashing --- that is, with each key mapped independently and uniformly to a random bucket --- what is the expected number of keys that get mapped to the first bucket? More precisely, what is the expected cardinality of the set `{k:h(k)=1}`.

**Answer:** `n/m`

**Explanation:** Use linearity of expectation, with one indicator variable for each key. The probability that one key hashes to the first bucket is `1/m`, and by linearity of expectation the total expected number of keys that hash to the first bucket is just `n/m`.

## Question 4

Suppose you implement the functionality of a priority queue using a sorted array (e.g., from biggest to smallest). What is the worst-case running time of **Insert** and **Extract-Min**, respectively? (Assume that you have a large enough array to accommodate the **Insertions** that you face.)

**Answer:** `Θ(n)` and `Θ(1)`

## Question 5

Suppose you implement the functionality of a priority queue using an unsorted array. What is the worst-case running time of **Insert** and **Extract-Min**, respectively? (Assume that you have a large enough array to accommodate the **Insertions** that you face.)

**Answer:** `Θ(1)` and `Θ(n)`

## Question 6

You are given a heap with `n` elements that supports **Insert** and **Extract-Min**. Which of the following tasks can you achieve in **O(log(n))** time?

**Answer:** Find the fifth-smallest element stored in the heap.

## Programming Question

Download the text file [here](http://spark-public.s3.amazonaws.com/algo1/programming_prob/HashInt.txt).

The file contains `100,000` integers all randomly chosen between `1` and `1,000,000` (there might be some repetitions). This is your array of integers: the `i-th` row of the file gives you the `i-th` entry of the array.

Here are `9` "target sums", in increasing order: `231552`, `234756`, `596873`, `648219`, `726312`, `981237`, `988331`, `1277361`, `1283379`. Your task is to implement the hash table-based algorithm explained in the video lectures and determine, for each of the `9` target sums `x`, whether or not `x` can be formed as the sum of two entries in the given array.

Your answer should be in the form of a `9`-bit string, with a `1` indicating "**yes**" for the corresponding target sum and `0` indicating "**no**". For example, if you discover that all of the target sums except for the `5-th` and the `7-th` one (i.e., except for `726312` and `988331`) can be formed from pairs from the input file, then your answer should be "`111101011`" (without the quotes). We reiterate that the answer should be in the same order as the target sums listed above (i.e., in increasing order of the target).

**Answer:** `101110100`
