import strutils

type
  Point = tuple[x, y, z: int]
  Vector {.borrow: `.`.} = distinct Point

proc initVector(x, y, z: int): Vector =
  result.x = x
  result.y = y
  result.z = z

proc `+`(p: Point, v: Vector): Point =
  result.x = p.x + v.x
  result.y = p.y + v.y
  result.z = p.z + v.z

proc initShifts(): seq[Vector] =
  for x in -1 .. 1:
    for y in -1 .. 1:
      for z in -1 .. 1:
        if x == 0 and y == 0 and z == 0:
          continue
        result.add(initVector(x, y, z))

const
  fieldSize = 22
  shifts = initShifts()

type
  Grid = array[fieldSize, array[fieldSize, array[fieldSize, char]]]
  Field = object
    cells: Grid
    min, max: Point

proc initGrid(cells: var Grid) =
  for z in 0 ..< fieldSize:
    for y in 0 ..< fieldSize:
      for x in 0 ..< fieldSize:
        cells[z][y][x] = '.'

proc initField(initialState: seq[string]): Field =
  initGrid(result.cells)
  let n = initialState.len

  result.min.x = fieldSize div 2 - n div 2
  result.min.y = result.min.x
  result.min.z = fieldSize div 2

  result.max.x = fieldSize div 2 + n div 2 - 1 + n mod 2
  result.max.y = result.max.x
  result.max.z = result.min.z

  for y in result.min.y .. result.max.y:
    for x in result.min.x .. result.max.x:
      result.cells[result.min.z][y][x] =
        initialState[y - result.min.y][x - result.min.x]

proc readInput(fileName: string): seq[string] =
  fileName.readFile.split('\n')

proc getActiveNeighbours(field: Field, point: Point): uint =
  for shift in shifts:
    if field.cells[point.z + shift.z][point.y + shift.y][
                   point.x + shift.x] == '#':
      result.inc

proc enlarge(field: Field): Field =
  initGrid(result.cells)
  result.min = field.min + initVector(-1, -1, -1)
  result.max = field.max + initVector(1, 1, 1)

proc solve(field: var Field, steps: int): uint =
  for step in 0 ..< steps:
    var newField = enlarge(field)
    for z in newField.min.z .. newField.max.z:
      for y in newField.min.y .. newField.max.y:
        for x in newField.min.x .. newField.max.x:
          let activeNeighbours = getActiveNeighbours(field, (x, y, z))
          if field.cells[z][y][x] == '#' and
            (activeNeighbours == 2 or activeNeighbours == 3):
            newField.cells[z][y][x] = '#'
          if field.cells[z][y][x] == '.' and activeNeighbours == 3:
            newField.cells[z][y][x] = '#'
    field = newField

  for z in field.min.z .. field.max.z:
    for y in field.min.y .. field.max.y:
      for x in field.min.x .. field.max.x:
        if field.cells[z][y][x] == '#':
          result.inc

let input = readInput("input.txt")
var field = initField(input)
echo solve(field, 6)
