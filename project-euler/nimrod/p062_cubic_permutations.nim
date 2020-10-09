import tables, strutils, algorithm, sugar, sequtils

var numbers: Table[string, int]
var counts: Table[string, uint8]
var answer: int

for i in 1 .. int.high:
  let cube = i * i * i
  let key = cube.intToStr.sorted.foldl(a & b, "")
  if not numbers.hasKey(key):
    numbers[key] = cube
    counts[key] = 1
  else:
    counts[key].inc
    if counts[key] == 5:
      answer = numbers[key]
      break

dump(answer)
