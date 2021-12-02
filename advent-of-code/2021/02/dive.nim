import sequtils, strutils

type Data = seq[(string, int)]

proc readInput(fileName: string): Data =
  fileName.lines.toSeq.mapIt(it.split).mapIt((it[0], it[1].parseInt))

proc solvePartOne(data: Data): int =
  var horizontalPosition = 0
  var verticalPosition = 0
  for (command, value) in data:
    case command
    of "forward": horizontalPosition += value
    of "down": verticalPosition += value
    of "up": verticalPosition -= value
  horizontalPosition * verticalPosition

proc solvePartTwo(data: Data): int =
  var aim = 0
  var horizontalPosition = 0
  var verticalPosition = 0
  for (command, value) in data:
    case command
    of "forward":
      horizontalPosition += value
      verticalPosition += aim * value
    of "down": aim += value
    of "up": aim -= value
  horizontalPosition * verticalPosition

let data = readInput("input.txt")
echo solvePartOne(data)
echo solvePartTwo(data)
