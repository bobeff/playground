# Repeat Collatz conjecture exercise so your program asks a user for a starting
# number. Print the resulting sequence.

import strUtils

proc calculateCollatzSequence(startNumber: uint64): seq[uint64] =
  result.add startNumber
  while result[^1] != 1:
    if result[^1] mod 2 == 0:
      result.add result[^1] div 2
    else:
      result.add result[^1] * 3 + 1

stdout.write("Enter a Collatz's sequence starting number: ")
let n = stdin.readLine().parseBiggestUInt()

stdout.write("Collatz(", n, ") = ", calculateCollatzSequence(n))
