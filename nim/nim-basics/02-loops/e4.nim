# In the previous exercises you have converted inches to centimeters, and vice
# versa. Create a conversion table with multiple values. For example, the table
# might look like this:

#   in | cm
#   ----------------
#   1  | 2.54
#   4  | 10.16
#   7  | 17.78
#   10 | 25.4
#   13 | 33.02
#   16 | 40.64
#   19 | 48.26

import strformat

const centimetersPerInch = 2.54

stdout.writeLine("-----------------")
stdout.writeLine(fmt"""| {"in":>2} | {"cm":>5.2} |""")
stdout.writeLine("-----------------")
for inches in 1 .. 39:
  let centimeters = float(inches) * centimetersPerInch
  stdout.writeLine(fmt"""| {inches:>2} | {centimeters:>5.2f} |""")
stdout.writeLine("-----------------")
