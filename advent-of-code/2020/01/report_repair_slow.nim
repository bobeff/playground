import strutils

const
  inputFileName = "input.txt"
  target = 2020

proc readInput(inputFile: string): seq[uint] =
  for line in inputFile.lines:
    result.add line.parseUInt

proc twoSum(data: seq[uint]): uint =
  ## O(n^2) complexity
  for i in 0 ..< data.len - 1:
    for j in i + 1 ..< data.len:
      if data[i] + data[j] == target:
        return data[i] * data[j]
        
proc threeSum(data: seq[uint]): uint =
  ## O(n^3) complexity
  for i in 0 ..< data.len - 2:
    for j in i + 1 ..< data.len - 1:
      for k in j + 1 ..< data.len:
        if data[i] + data[j] + data[k] == target:
          return data[i] * data[j] * data[k]

let data = readInput(inputFileName)
echo data.twoSum
echo data.threeSum
