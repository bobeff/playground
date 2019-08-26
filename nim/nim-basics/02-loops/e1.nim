# Collatz conjecture is a popular mathematical problem with simple rules. First
# pick a number. If it is odd, multiply it by three and add one; if it is even,
# divide it by two. Repeat this procedure until you arrive at one. E.g.
# 5 → odd → 3*5 + 1 = 16 → even → 16 / 2 = 8 → even → 4 → 2 → 1 → end!
# Pick an integer (as a mutable variable) and create a loop which will print
# every step of the Collatz conjecture. (Hint: use div for division)

import parseUtils, strformat

write(stdout, "Enter a number: ")
let input = readLine(stdin)
var number: uint64
discard input.parseBiggestUInt(number)

writeLine(stdout, fmt"The Collatz's sequence for {number} is:")

while number != 1:
  write(stdout, fmt"{number} -> ")
  if number mod 2 == 0:
    number = number div 2
  else:
    number = number * 3 + 1

writeLine(stdout, number)
