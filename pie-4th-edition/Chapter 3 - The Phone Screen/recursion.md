# PROBLEM: Why is recursion bad?

In general case it is not bad and it can lead to very elegant code, but it have
a few tradeoffs:

- The code with recursion sometimes is harder to understand.

- The recursive function calls pay the price for function call and can lead to
slower code compared to single function call with maintained by the programmer
stack.

- The saving of entire stack frame to the thread's stack on each recursive call
can lead to more stack memory usage than saving only the relevant variables
when not using recursion with maintained by the programmer stack to simulate it.
