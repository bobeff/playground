import sets, strutils, sequtils

proc readInput(fileName: string): seq[uint64] =
  fileName.readFile.split('\n').mapIt(it.parseBiggestUInt)

proc solvePartOne(numbers: seq[uint64], preambleSize: int): uint64 =
  for x in 0 ..< numbers.len - preambleSize - 1:
    var sums: HashSet[uint64]
    let upperBound = x + preambleSize
    for y in x ..< upperBound - 1:
      for z in y + 1 ..< upperBound:
        sums.incl(numbers[y] + numbers[z])
    if numbers[upperBound] notin sums:
      return numbers[upperBound]

proc findSmallestAndLargestNumbersSum(
    numbers: seq[uint64], startIndex, endIndex: int): uint64 =
  var
    smallest = numbers[startIndex]
    largest = numbers[startIndex]
  for i in startIndex .. endIndex:
    if numbers[i] < smallest:
      smallest = numbers[i]
    if numbers[i] > largest:
      largest = numbers[i]
  return smallest + largest

proc solvePartTwo(numbers: seq[uint64], sum: uint64): uint64 =
  for x in 0 ..< numbers.len - 1:
    var currentSum = numbers[x]
    for y in x + 1 ..< numbers.len:
      currentSum += numbers[y]
      if currentSum == sum:
        return findSmallestAndLargestNumbersSum(numbers, x, y)
      elif currentSum > sum:
        break

let numbers = readInput("input.txt")
let number = solvePartOne(numbers, 25)
echo number
echo solvePartTwo(numbers, number)
