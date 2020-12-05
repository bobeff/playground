import sets, strutils

const
  inputFileName = "input.txt"
  target = 2020

# All input numbers are unique and because of this HashSet works.

proc readInput(inputFile: string): HashSet[uint] =
  for line in inputFile.lines:
    result.incl line.parseUInt

proc twoSum(numbers: HashSet[uint], skipNumber, target: uint): uint =
  ## O(n) complexity
  for number in numbers:
    if number == skipNumber:
      continue
    let difference = target - number
    if difference in numbers:
      return number * difference

proc threeSum(numbers: HashSet[uint]): uint =
  ## O(n^2) complexity
  for x in numbers:
    let twoSumTarget = target - x
    let y = twoSum(numbers, x.uint, twoSumTarget)
    if y != 0:
      return x * y

let numbers = readInput(inputFileName)
echo twoSum(numbers, 0, target)
echo threeSum(numbers)
