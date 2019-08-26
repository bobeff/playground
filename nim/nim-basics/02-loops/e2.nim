# Create an immutable variable containing your full name. Write a for-loop which
# will iterate through that string and print only the vowels (a, e, i, o, u).
# (Hint: use case statement with multiple values per branch)

let myName = "Ivan Petrov Bobev"

for c in myName:
  if c in {'a', 'e', 'i', 'o', 'u'}:
    write(stdout, c)

writeLine(stdout, "")

for c in myName:
  case c
    of 'a', 'e', 'i', 'o', 'u': write(stdout, c)
    else: discard

writeLine(stdout, "")
