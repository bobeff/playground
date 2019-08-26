# Create an empty array which can contain ten integers.
#   1. Fill that array with numbers 10, 20, …​, 100. (Hint: use loops)
#   2. Print only the elements of that array that are on odd indices (values 20,
#      40, …​).
#   3. Multiply elements on even indices by 5. Print the modified array.

var a: array[1 .. 10, int]

for i in a.low .. a.high:
  a[i] = i * 10
  if i mod 2 == 0:
    stdout.writeLine("a[", i, "] = ", a[i])
    a[i] *= 5

stdout.writeLine("")

for i in a.low .. a.high:
  stdout.writeLine("a[", i, "] = ", a[i])
