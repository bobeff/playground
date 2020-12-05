import math, strformat, unittest

const
  inputFileName = "input.txt"
  rowsCount = 128u
  columnsCount = 8u

proc readInput(fileName: string): seq[string] =
  for line in fileName.lines:
    result.add line

template average(x, y: uint): float =
  float(x + y) / 2.0

template averageFloor(x, y: uint): uint =
  uint(floor(average(x, y)))

template averageCeil(x, y: uint): uint =
  uint(ceil(average(x, y)))

proc decode(boardingPass: string): uint =
  var
    rowMin = 0u
    rowMax = rowsCount - 1
    columnMin = 0u
    columnMax = columnsCount - 1

  for c in boardingPass:
    case c
      of 'F': rowMax = averageFloor(rowMin, rowMax)
      of 'B': rowMin = averageCeil(rowMin, rowMax)
      of 'L': columnMax = averageFloor(columnMin, columnMax)
      of 'R': columnMin = averageCeil(columnMin, columnMax)
      else:
        raise newException(ValueError,
          "Invalid boarding pass character: " & c)

  if rowMin != rowMax or columnMin != columnMax:
    raise newException(ValueError,
      &"The data in the boarding pass '{boardingPass}' " &
       "is not sufficient to produce an answer.")

  return rowMin * columnsCount + columnMin

test "decode":
  check "FBFBBFFRLR".decode == 357
  check "BFFFBBFRRR".decode == 567
  check "FFFBBBFRRR".decode == 119
  check "BBFFBBFRLL".decode == 820
  expect ValueError: discard "FBFBBFMRLR".decode
  expect ValueError: discard "BBFFBBFRL".decode

proc solvePart1(boardingPasses: seq[string]): uint =
  for boardingPass in boardingPasses:
    let seatId = boardingPass.decode
    if seatId > result:
      result = seatId

proc solvePart2(boardingPasses: seq[string]): uint =
  var seats: array[rowsCount * columnsCount, bool]

  for boardingPass in boardingPasses:
    let seatId = boardingPass.decode
    seats[seatId] = true

  for seatId in 1 ..< rowsCount * columnsCount - 1:
    if seats[seatId - 1] and (not seats[seatId]) and seats[seatId + 1]:
      return uint(seatId)

let boardingPasses = readInput(inputFileName)
echo solvePart1(boardingPasses)
echo solvePart2(boardingPasses)
