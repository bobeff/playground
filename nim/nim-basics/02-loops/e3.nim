# Fizz buzz is a kids game sometimes used to test basic programming knowledge.
# We count numbers from one upwards. If a number is divisible by 3 replace it
# with fizz, if it is divisible by 5 replace it with buzz, and if a number is
# divisible by 15 (both 3 and 5) replace it with fizzbuzz. First few rounds
# would look like this: 1, 2, fizz, 4, buzz, fizz, 7, …​
# Create a program which will print first 30 rounds of Fizz buzz.
# (Hint: beware of the order of divisibility tests)

for round in 1 .. 30:
  if round mod 3 == 0 and round mod 5 == 0:
    write(stdout, "fizzbuzz, ")
  elif round mod 3 == 0:
    write(stdout, "fizz, ")
  elif round mod 5 == 0:
    write(stdout, "buzz, ")
  else:
    write(stdout, round, ", ")

writeLine(stdout, "")
