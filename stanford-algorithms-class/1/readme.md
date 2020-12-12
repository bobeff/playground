# Problem Set 1

## Question 1

**3-way-mergesort**:  Suppose instead of dividing in half at each step of the merge sort, you divide into thirds, sort each third, and finally combine all of them using a three way merge. What is the overall running time of this algorithm ? (Hint - Note that the merge step can still be implemented in `O(n)` time.)

**Answer:** `n.log(n)`

**Explanation:** That's correct! There is still a logarithmic number of levels, and the overall amount of work at each level is still linear.

## Question 2

You are given functions `f` and `g` such that `f(n)=O(g(n))`. Is `f(n).log2(f(n)^c)=O(g(n).log2(g(n)))` ? (Here `c` is some constant `>0`. You can assume that `f` and `g` are always bigger than `1`.)

**Answer:** yes

**Explanation:** Roughly, because the constant `c` in the exponent is inside a logarithm, it becomes part of the leading constant and gets suppressed by the big-Oh notation.

## Question 3

Assume again two (positive) functions `f` and `g` such that `f(n)=O(g(n))`. Is `2^f(n)=O(2^g(n))` ? (Multiple answers may be correct.)

- [x] Yes if `f(n) <= g(n)` for all sufficiently large n.
- [x] Sometimes
- [ ] Never (For example, what if `f(n)=g(n)`?)
- [ ] Always (What if `f(n) = 2.n` and `g(n) = n`?)

## Question 4

**k-way-mergesort**. Suppose you are given `k` sorted arrays, each with `n` elements, and you want to combine them into a single array of `k.n` elements. Consider the following approach. Using the merge subroutine taught in lecture, you merge the first 2 arrays, then merge the 3rd given array with this merged version of the first two arrays, and so on until you merge in the final (`k-th`) input array. What is the time taken for this strategy, as a function of `k` and `n` ? (Optional: can you think of a faster way to do the k-way merge procedure ?)

**Answer:** `θ(n.k^2)`

**Explanation:** For the upper bound, the merged list size is always `O(k.n)`, merging is linear in the size of the larger array, and there are `k` iterations. For the lower bound, each of the last `k/2` merges takes `θ(k.n)` time.

## Question 5

Arrange the following functions in increasing order of growth rate. ie. if `g(n)` follows `f(n)` in your list, then `f(n)` is necessarily `O(g(n))`.

- `sqrt(n)`
- `10^n`
- `n^1.5`
- `2^sqrt(log(n))`
- `n^(5/3)`

**Answer:** `2^sqrt(log(n))`, `sqrt(n)`, `n^1.5`, `n^(5/3)`, `10^n`

**Explanation:** One approach is to graph these functions for large values of `n`. Once in a while this can be misleading, however. Another useful trick is to take logarithms and see what happens (though again be careful, as in Question 3).

## Programming Question

Download the text file [here](http://spark-public.s3.amazonaws.com/algo1/programming_prob/IntegerArray.txt).

The file contains all the `100,000` integers between `1` and `100,000` (including both) in some random order (no integer is repeated).

Your task is to find the number of inversions in the file given (every row has a single integer between `1` and `100,000`). Assume your array is from `1` to `100,000` and `i-th` row of the file gives you the `i-th` entry of the array.
Write a program and run over the file given.

**Answer:** `2407905288`
