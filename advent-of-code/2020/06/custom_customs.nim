import sets, strutils, sequtils

const
  inputFileName = "input.txt"

proc readInput(fileName: string): auto =
  fileName.readFile.split("\n\n").mapIt(it.split('\n').mapIt(it.toHashSet))

template solve(op: untyped): int =
  data.mapIt(it.foldl(op)).mapIt(it.card).foldl(a + b)

let data = inputFileName.readInput
echo solve(a + b)
echo solve(a * b)
