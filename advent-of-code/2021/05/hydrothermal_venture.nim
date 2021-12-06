import strutils, sequtils

type
  Point = tuple[x, y: int]
  Line = tuple[p1, p2: Point]

proc parsePoint(s: string): Point =
  let coords = s.split(",").mapIt(it.parseInt)
  result.x = coords[0]
  result.y = coords[1]

proc readInput(fileName: string):
    tuple[lines: seq[Line], maxPoint: Point] =
  for line in fileName.lines:
    let points = line.split(" -> ")
    var line: Line
    line.p1 = points[0].parsePoint
    line.p2 = points[1].parsePoint
    result.lines.add line
    if line.p1.x > result.maxPoint.x:
      result.maxPoint.x = line.p1.x
    if line.p2.x > result.maxPoint.x:
      result.maxPoint.x = line.p2.x
    if line.p1.y > result.maxPoint.y:
      result.maxPoint.y = line.p1.y
    if line.p2.y > result.maxPoint.y:
      result.maxPoint.y = line.p2.y

proc drawLine(field: var seq[seq[int]], line: Line) =
  var x0 = line.p1.x
  var y0 = line.p1.y
  let x1 = line.p2.x
  let y1 = line.p2.y
  let dx = abs(x1 - x0)
  let sx = if x0 < x1: 1 else: -1
  let dy = -abs(y1 - y0)
  let sy = if y0 < y1: 1 else: -1
  var error = dx + dy
  while true:
    field[y0][x0].inc
    if x0 == x1 and y0 == y1:
      break
    let error2 = 2 * error
    if error2 >= dy:
      error += dy
      x0 += sx
    if error2 <= dx:
      error += dx
      y0 += sy

proc pointsWithOverlappingLines(lines: seq[Line], maxPoint: Point): int =
  var field = newSeqOfCap[seq[int]](maxPoint.x + 1)
  for i in 0 .. maxPoint.x:
    field.add newSeq[int](maxPoint.y + 1)
  for line in lines:
    field.drawLine(line)
  for line in field:
    for number in line:
      if number > 1:
        result.inc

let (lines, maxPoint) = "input.txt".readInput
echo pointsWithOverlappingLines(lines.filterIt(
  it.p1.x == it.p2.x or it.p1.y == it.p2.y), maxPoint)
echo pointsWithOverlappingLines(lines, maxPoint)
