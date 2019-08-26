# Ask a user for a string they want to have reversed. Create a procedure which
# takes a string and returns a reversed version. For example, if user types
# Nim-lang, the procedure should return gnal-miN. (Hint: use indexing and
# countdown)

proc reverse(s: string): string =
  result.setLen s.len
  for i, c in s:
    result[s.len - i - 1] = c

while true:
  stdout.write("Enter string (empty for exit): ")
  let s = stdin.readLine()
  if s.len == 0:
    break
  stdout.writeLine("The reversed string is: ", s.reverse)
