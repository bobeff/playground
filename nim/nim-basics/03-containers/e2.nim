# Re-do the Collatz conjecture exercise, but this time instead of printing each
# step, add it to a sequence.
#   1. Pick a starting number. Interesting choices, among others, are 9, 19, 25 
#      and 27.
#   2. Create a sequence whose only member is that starting number.
#   3. Using the same logic as before, keep adding elements to the sequence
#      until you reach 1
#   4. Print the length of the sequence, and the sequence itself.

proc calculateCollatzSequence(startNumber: uint64): seq[uint64] =
  result.add startNumber
  while result[^1] != 1:
    if result[^1] mod 2 == 0:
      result.add result[^1] div 2
    else:
      result.add result[^1] * 3 + 1

const collatz9 = calculateCollatzSequence(9)
const collatz19 = calculateCollatzSequence(19)
const collatz25 = calculateCollatzSequence(25)
const collatz27 = calculateCollatzSequence(27)

proc printCollatz(startNumber: uint64, collatz: seq[uint64]) =
  stdout.write(
    "Collatz(", startNumber, ") has length ", collatz.len, " and it is: ")
  for n in collatz:
    stdout.write(n, ", ")
  stdout.writeLine("\n")

printCollatz(9, collatz9)
printCollatz(19, collatz19)
printCollatz(25, collatz25)
printCollatz(27, collatz27)
