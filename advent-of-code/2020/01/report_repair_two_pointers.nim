import strutils, algorithm

const
  inputFileName = "input.txt"
  target = 2020

proc readInput(inputFile: string): seq[uint] =
  for line in inputFile.lines:
    result.add line.parseUInt

proc twoSum(numbers: seq[uint], startIndex, target: uint): uint =
  ## O(n) complexity
  var
    i = startIndex
    j = numbers.len - 1
  while i < j.uint:
    let sum = numbers[i] + numbers[j]
    if sum == target:
      return numbers[i] * numbers[j]
    elif sum < target:
      inc(i)
    else:
      dec(j)

proc threeSum(numbers: seq[uint]): uint =
  ## O(n^2) complexity
  for i in 0 ..< numbers.len - 2:
    let twoSumTarget = target - numbers[i]
    let twoSumProduct = twoSum(numbers, i.succ.uint, twoSumTarget)
    if twoSumProduct != 0:
      return twoSumProduct * numbers[i]

var numbers = readInput(inputFileName)
numbers.sort                    # O(n * log(n)) complexity
echo twoSum(numbers, 0, target) # O(n * log(n)) sum complexity
echo threeSum(numbers)          # O(n^2) sum complexity
