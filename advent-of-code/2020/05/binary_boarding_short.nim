import math, algorithm, sequtils, strutils

const
  inputFileName = "input.txt"

proc toNumber(line: string): uint =
  for i, c in line:
    if c == 'B' or c == 'R':
      result += uint(2 ^ (10 - i - 1))

proc readInput(fileName: string): seq[uint] =
  fileName.readFile.split('\n').mapIt(it.toNumber).sorted

let numbers = readInput(inputFileName)
echo numbers[^1]
for i in 0 ..< numbers.len:
  if numbers[i + 1] - numbers[i] == 2:
    echo numbers[i] + 1
    break
