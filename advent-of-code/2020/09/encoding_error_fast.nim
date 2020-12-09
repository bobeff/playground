import sets, strutils, sequtils

proc readInput(fileName: string): seq[uint64] =
  fileName.readFile.split('\n').mapIt(it.parseBiggestUInt)

proc hasTwoSum(numbers: seq[uint64], startIndex, endIndex: int,
            target: uint64): bool =
  var numbersSet: HashSet[uint64]
  for i in startIndex .. endIndex:
    numbersSet.incl numbers[i]
  for number in numbersSet:
    let difference = target - number
    if difference in numbersSet:
      return true

proc solvePartOne(numbers: seq[uint64], preambleSize: int): uint64 =
  for x in 0 ..< numbers.len - preambleSize - 1:
    let upperBound = x + preambleSize
    if not hasTwoSum(numbers, x, upperBound - 1, numbers[upperBound]):
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
  var
    l = 0
    r = 0
    currentSum = numbers[l]
  while currentSum != sum:
    if currentSum > sum:
      currentSum -= numbers[l]
      l.inc
    else:
      r.inc
      currentSum += numbers[r]
  return findSmallestAndLargestNumbersSum(numbers, l, r)

let numbers = readInput("input.txt")
let number = solvePartOne(numbers, 25)
echo number
echo solvePartTwo(numbers, number)
