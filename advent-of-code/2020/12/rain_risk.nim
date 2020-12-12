import strutils, strformat, tables, math

type
  Command = tuple[id: char, arg: int]
  Commands = seq[Command]

  Orientation = enum
    north, south, east, west

  Point = tuple[x, y: int]

  Ferry = object
    orientation: Orientation
    position: Point

# In the input turn degrees are always 90, 180 or 270

const
  turnLeftTable = {
    north: {90: west, 180: south, 270: east}.toTable,
    south: {90: east, 180: north, 270: west}.toTable,
    east: {90: north, 180: west, 270: south}.toTable,
    west: {90: south, 180: east, 270: north}.toTable}.toTable
  
  turnRightTable = {
    north: {90: east, 180: south, 270: west}.toTable,
    south: {90: west, 180: north, 270: east}.toTable,
    east: {90: south, 180: west, 270: north}.toTable,
    west: {90: north, 180: east, 270: south}.toTable}.toTable

proc readInput(fileName: string): Commands =
  for line in fileName.lines:
    let command = (line[0], parseInt(line[1 .. ^1]))
    result.add command

proc moveNorth(position: var Point, distance: int) =
  position.y += distance

proc moveSouth(position: var Point, distance: int) =
  position.y -= distance

proc moveEast(position: var Point, distance: int) =
  position.x += distance

proc moveWest(position: var Point, distance: int) =
  position.x -= distance

proc turnLeft(ferry: var Ferry, degrees: int) =
  ferry.orientation = turnLeftTable[ferry.orientation][degrees]

proc turnRight(ferry: var Ferry, degrees: int) =
  ferry.orientation = turnRightTable[ferry.orientation][degrees]

proc moveForward(ferry: var Ferry, distance: int) =
  case ferry.orientation
    of north: ferry.position.moveNorth(distance)
    of south: ferry.position.moveSouth(distance)
    of east: ferry.position.moveEast(distance)
    of west: ferry.position.moveWest(distance)

proc executeCommandsPartOne(ferry: var Ferry, commands: Commands) =
  for command in commands:
    case command.id
      of 'N': ferry.position.moveNorth(command.arg)
      of 'S': ferry.position.moveSouth(command.arg)
      of 'E': ferry.position.moveEast(command.arg)
      of 'W': ferry.position.moveWest(command.arg)
      of 'L': ferry.turnLeft(command.arg)
      of 'R': ferry.turnRight(command.arg)
      of 'F': ferry.moveForward(command.arg)
      else: raiseAssert(&"Invalid command '{command.id}'.")

proc rotate(wayPoint: var Point, shipPosition: Point, degrees: int) =
  let
    radians = degToRad(float(degrees))
    sinAngle = sin(radians)
    cosAngle = cos(radians)
    deltaX = float(wayPoint.x - shipPosition.x)
    deltaY = float(wayPoint.y - shipPosition.y)
  wayPoint.x = int(round(cosAngle * deltaX - sinAngle * deltaY)) + shipPosition.x
  wayPoint.y = int(round(sinAngle * deltaX + cosAngle * deltaY)) + shipPosition.y

proc moveToWayPoint(ferryPosition, wayPoint: var Point, times: int) =
  let
    deltaX = times * (wayPoint.x - ferryPosition.x)
    deltaY = times * (wayPoint.y - ferryPosition.y)
  ferryPosition.x += deltaX
  ferryPosition.y += deltaY
  wayPoint.x += deltaX
  wayPoint.y += deltaY

proc executeCommandsPartTwo(ferry: var Ferry, wayPoint: var Point,
                            commands: Commands) =
  for command in commands:
    case command.id
      of 'N': wayPoint.moveNorth(command.arg)
      of 'S': wayPoint.moveSouth(command.arg)
      of 'E': wayPoint.moveEast(command.arg)
      of 'W': wayPoint.moveWest(command.arg)
      of 'L': wayPoint.rotate(ferry.position, command.arg)
      of 'R': wayPoint.rotate(ferry.position, 360 - command.arg)
      of 'F': ferry.position.moveToWayPoint(wayPoint, command.arg)
      else: raiseAssert(&"Invalid command '{command.id}'.")

proc manhattanDistance(ferry: Ferry): int =
  abs(ferry.position.x) + abs(ferry.position.y)

let commands = readInput("input.txt")
var ferry = Ferry(orientation: east, position: (x: 0, y: 0))
executeCommandsPartOne(ferry, commands)
echo manhattanDistance(ferry)

ferry = Ferry(orientation: east, position: (x: 0, y: 0))
var wayPoint = (x: 10, y: 1)
executeCommandsPartTwo(ferry, wayPoint, commands)
echo manhattanDistance(ferry)
