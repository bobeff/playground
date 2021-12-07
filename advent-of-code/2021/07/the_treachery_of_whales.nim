import strutils, sequtils

type
  PartOne = object
  PartTwo = object

proc solve[T: PartOne | PartTwo](
    Part: type T, numbers: seq[int], maxPosition: int): int =
  result = int.high
  for position in 0 .. maxPosition:
    var candidateAnswer = 0
    for number in numbers:
      let distance = abs(number - position)
      when Part is PartOne:
        candidateAnswer += distance
      elif Part is PartTwo:
        candidateAnswer += distance * (distance + 1) div 2
      else:
        {.fatal: &"Invalid problem part: " & $type(Part).}
    if candidateAnswer < result:
      result = candidateAnswer

let numbers = "input.txt".readFile.strip.split(',').mapIt(it.parseInt)
let maxPosition = max(numbers)
echo solve(PartOne, numbers, maxPosition)
echo solve(PartTwo, numbers, maxPosition)
