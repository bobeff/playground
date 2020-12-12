# Problem Set 2

## Question 1

This question will give you further practice with the **Master Method**. Suppose the running time of an algorithm is governed by the recurrence `T(n)=9∗T(n/3)+n^2`,. What's the overall asymptotic running time (i.e., the value of `T(n)`)?

**Answer:** `θ((n^2).log(n))`

**Explanation:** `a = b^d = 9`, so this is case `1` of the **Master Method**.

## Question 2

Consider the following pseudo-code for calculating `a^b` (where `a` and `b` are positive integers)

```
FastPower(a,b):
  if b = 1
    return a
  otherwise
    c := a*a
    ans := FastPower(c,[b/2])
  if b is odd
    return a*ans
  otherwise return ans
end
```

Here `[x]` denotes the floor function, that is, the largest integer less than or equal to `x`.

Now assuming that you use a calculator that supports multiplication and division (i.e., you can do multiplications and divisions in constant time), what would be the overall running time of the above algorithm (as a function of `b`)?

**Answer:** `log(b)`

**Explanation:** Constant work per digit in the binary expansion of `b`.

**Question Explanation:** This gives you a nice way of raising a number to the power in multiplications much less than `b`. You can get the answer by looking at the binary expression for `b`.

## Question 3

Let `0<α<0.5` be some constant. What is the probability that on a random input array, **PARTITION** (for quick sort as explained in lectures) produces a split in which the size of the smaller sub-array after the **PARTITION** subroutine is `≥α` times the size of the original array?

**Answer:** `α`

**Explanation:** What is the probability that a randomly chosen pivot lies between the `α.n` and `(1−α).n` positions in the sorted version of the input array (where `n` is the array length)?

## Question 4

Now assume that you achieve the approximately balanced splits above in every recursive call --- that is, every recursive call is passed a sub-array that contains between an `α` and `1−α` fraction of the elements of the given array (for some constant `0<α<0.5`). What is the order of depth `d` in the recursive tree (i.e., the range of the number of successive recursive calls before hitting the base case)?

**Answer:** `−log(n)/log(α)≤d≤−log(n)/log(1−α)`

**Question Explanation:** Recall the recursion tree method taught in **MergeSort** and **Master Method** lectures.

## Question 5

Define the recursion depth of **QuickSort** to be the maximum number of successive recursive calls before it hits the base case --- equivalently, the number of the last level of the corresponding recursion tree. Note that the recursion depth is a random variable, which depends on which pivots get chosen. What is the recursion depth of **QuickSort** (both for a best-case choice of pivots, and for a worst-case choice of pivots)?

**Answer:** best `log(n)`, worst `n`

**Explanation:** The best case is when the algorithm always picks the median as a pivot, in which case the recursion is essentially identical to that in **MergeSort**. In the worst case the min or the max is always chosen as the pivot, resulting in linear depth.

## Programming Question 1

Download the text file [here](http://spark-public.s3.amazonaws.com/algo1/programming_prob/QuickSort.txt).

The file contains all of the integers between `1` and `10,000` (inclusive) in unsorted order (with no integer repeated). The integer in the `i-th` row of the file gives you the `i-th` entry of an input array.

Your task is to compute the total number of comparisons used to sort the given input file by **QuickSort**. As you know, the number of comparisons depends on which elements are chosen as pivots, so we'll ask you to explore three different pivoting rules.

You should not count comparisons one-by-one. Rather, when there is a recursive call on a sub-array of length `m`, you should simply add `m−1` to your running total of comparisons. (This is because the pivot element will be compared to each of the other `m−1` elements in the sub-array in this recursive call.)

**WARNING:** The **Partition** subroutine can be implemented in several different ways, and different implementations can give you differing numbers of comparisons. For this problem, you should implement the **Partition** subroutine as it is described in the video lectures (otherwise you might get the wrong answer).

**DIRECTIONS FOR THIS PROBLEM:**

For the first part of the programming assignment, you should always use the first element of the array as the pivot element.

**Answer:** `162085`

## Programming Question 2

Compute the number of comparisons (as in **Programming Question 1**), always using the final element of the given array as the pivot element. Again, be sure to implement the **Partition** subroutine as it is described in the video lectures. Recall from the lectures that, just before the main **Partition** subroutine, you should exchange the pivot element (i.e., the last element) with the first element.

**Answer:** `164123`

## Programming Question 3

Compute the number of comparisons (as in **Programming Question 1**), using the **"median-of-three"** pivot rule. [This primary motivation behind this rule is to do a little bit of extra work to get much better performance on input arrays that are already sorted.] In more detail, you should choose the pivot as follows. Consider the first, middle, and final elements of the given array. (If the array has odd length it should be clear what the "middle" element is; for an array with even length `2k`, use the `k-th` element as the "middle" element. So for the array: `4 5 6 7`, the "middle" element is the second one ---- `5` and not `6`!) Identify which of these three elements is the median (i.e., the one whose value is in between the other two), and use this as your pivot. As discussed in the first and second parts of this programming assignment, be sure to implement **Partition** as described in the video lectures (including exchanging the pivot element with the first element just before the main **Partition** subroutine).

**SUBTLE POINT:** A careful analysis would keep track of the comparisons made in identifying the median of the three elements. You should NOT do this. That is, as in the previous two problems, you should simply add `m−1` to your running total of comparisons every time you recurse on a sub-array with length `m`.

**Answer:** `138382`
