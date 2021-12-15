import strutils, sequtils, strformat, sets

type
  Command = object
    splitType: char
    value: int

  Point = tuple[x, y: int]

proc readInput(fileName: string):
    tuple[points: seq[Point], commands: seq[Command]] =
  let data = fileName.readFile.split("\n\n").mapIt(it.strip.split("\n"))
  result.points = data[0].mapIt(
    it.split(',').mapIt(it.parseInt)).mapIt((it[0], it[1]))
  result.commands = data[1].mapIt(
    Command(splitType: it[11], value: parseInt(it[13 .. ^1])))

proc applyCommands(points: seq[Point], commands: seq[Command]): HashSet[Point] =
  var points = points.toHashSet
  for command in commands:
    var newPoints: HashSet[Point]
    case command.splitType:
    of 'x':
      for point in points:
        if point.x < command.value:
          newPoints.incl point
        elif point.x > command.value:
          newPoints.incl (x: 2 * command.value - point.x, y: point.y)
    of 'y':
      for point in points:
        if point.y < command.value:
          newPoints.incl point
        elif point.y > command.value:
          newPoints.incl (x: point.x, y: 2 * command.value - point.y)
    else: raiseAssert(&"Invalid split type: {command.splitType}.")
    points = newPoints
  return points

proc solvePartOne(points: seq[Point], command: Command): int =
  applyCommands(points, @[command]).len

proc solvePartTwo(points: seq[Point], commands: seq[Command]) =
  let points = applyCommands(points, commands)

  var maxX, maxY: int
  for point in points:
    if point.x > maxX:
      maxX = point.x
    if point.y > maxY:
      maxY = point.y

  for r in 0 .. maxY:
    for c in 0 .. maxX:
      stdout.write(if (c, r) in points: "# " else: ". ")
    stdout.write('\n')

let (points, commands) = readInput("input.txt")
echo solvePartOne(points, commands[0])
solvePartTwo(points, commands)
