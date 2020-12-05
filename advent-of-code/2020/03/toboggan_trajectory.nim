import strutils

const
  inputFileName = "input.txt"

proc readInput(inputFile: string): seq[string] =
  for line in inputFile.lines:
    result.add line.strip

proc solve(slope: seq[string], slopeX, slopeY: int): uint =
  let
    width = slope[0].len
    height = slope.len
  var currentX, currentY = 0
  while currentY < height:
    if slope[currentY][currentX] == '#':
      result.inc
    currentX = (currentX + slopeX) mod width
    currentY += slopeY

let input = readInput(inputFileName)
let slope1x1 = solve(input, 1, 1)
let slope3x1 = solve(input, 3, 1)
let slope5x1 = solve(input, 5, 1)
let slope7x1 = solve(input, 7, 1)
let slope1x2 = solve(input, 1, 2)

echo slope3x1
echo slope1x1 * slope3x1 * slope5x1 * slope7x1 * slope1x2
