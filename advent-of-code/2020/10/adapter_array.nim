import algorithm, strutils, sequtils

proc readInput(fileName: string): seq[uint] =
  result.add 0
  result.add fileName.readFile.split('\n').mapIt(it.parseUInt).sorted
  result.add result[^1] + 3

proc solvePartOne(joltages: seq[uint]): uint =
  var
    oneDiffs = 0'u
    threeDiffs = 0'u
  for i in 1 ..< joltages.len:
    let diff = joltages[i] - joltages[i - 1]
    if diff == 1:
      oneDiffs.inc
    elif diff == 3:
      threeDiffs.inc
  oneDiffs * threeDiffs

proc solvePartTwo(joltages: seq[uint]): uint64 =
  var pathsCount = newSeqWith(joltages.len, 0'u64)
  pathsCount[0] = 1
  for i in 0 ..< joltages.len - 1:
    for j in i + 1 ..< i + 4:
      if joltages[j] - joltages[i] <= 3:
        pathsCount[j] += pathsCount[i]
      else:
        break
  pathsCount[^1]

let joltages = readInput("input.txt")
echo solvePartOne(joltages)
echo solvePartTwo(joltages)
