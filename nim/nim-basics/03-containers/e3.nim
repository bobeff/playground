# Find the number in a range from 2 to 100 which will produce the longest 
# Collatz sequence.
#   1. For each number in the given range calculate its Collatz sequence.
#   2. If the length of current sequence is longer than the previous record,
#      save the current length and the starting number as a new record (you can
#      use the tuple (longestLength, startingNumber) or two separate variables).
#   3. Print the starting number which gives the longest sequence, and its 
#      length.

proc calculateCollatzSequence(startNumber: uint64): seq[uint64] =
  result.add startNumber
  while result[^1] != 1:
    if result[^1] mod 2 == 0:
      result.add result[^1] div 2
    else:
      result.add result[^1] * 3 + 1

var bestSequence = calculateCollatzSequence(1)
for n in 2u64 .. 100u64:
  var currentSequence = calculateCollatzSequence(n)
  if currentSequence.len > bestSequence.len:
    bestSequence = currentSequence

stdout.write("The longest Collatz's sequence between 1 and 100 is Collatz(",
  bestSequence[0], ") with length ", bestSequence.len, " and it is: ")
for n in bestSequence:
  stdout.write(n, ", ")
stdout.writeLine("")
