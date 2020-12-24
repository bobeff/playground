import strutils, sets

const
  eastShift      = ( 1,  0)
  southEastShift = ( 0,  1)
  southWestShift = (-1,  1)
  westShift      = (-1,  0)
  northWestShift = ( 0, -1)
  northEastShift = ( 1, -1)

  shifts = [eastShift, southEastShift, southWestShift,
            westShift, northWestShift, northEastShift]

type
  Direction = enum
    east, southEast, southWest, west, northWest, northEast

  HexCoords = tuple[x, y: int]

  TilesSet = HashSet[HexCoords]

proc `+`(lhs, rhs: HexCoords): HexCoords =
  result = (lhs.x + rhs.x, lhs.y + rhs.y)

proc `+=`(lhs: var HexCoords, rhs: HexCoords) =
  lhs = lhs + rhs

proc readInput(fileName: string): seq[seq[Direction]] =
  for line in fileName.lines:
    var line = line
    var tilePath = newSeqOfCap[Direction](line.len)
    while line.len > 0:
      if line.startsWith("se"):
        tilePath.add southEast
      elif line.startsWith("sw"):
        tilePath.add southWest
      elif line.startsWith("nw"):
        tilePath.add northWest
      elif line.startsWith("ne"):
        tilePath.add northEast
      elif line.startsWith("e"):
        tilePath.add east
      elif line.startsWith("w"):
        tilePath.add west
      else:
        raiseAssert("Invalid direction.")

      if tilePath[^1] == east or tilePath[^1] == west:
        line = line[1 .. ^1]
      else:
        line = line[2 .. ^1]

    result.add tilePath

proc solvePartOne(tiles: seq[seq[Direction]]): TilesSet =
  for tilePath in tiles:
    var tileCoords = (0, 0)
    for nextTile in tilePath:
      case nextTile:
        of east:
          tileCoords += eastShift
        of southEast:
          tileCoords += southEastShift
        of southWest:
          tileCoords += southWestShift
        of west:
          tileCoords += westShift
        of northWest:
          tileCoords += northWestShift
        of northEast:
          tileCoords += northEastShift
    if tileCoords in result:
      result.excl tileCoords
    else:
      result.incl tileCoords

proc adjacentBlackTilesCount(tileToCheck: HexCoords,
                             blackTiles: TilesSet): uint =
  for shift in shifts:
    var tile = tileToCheck + shift
    if tile in blackTiles:
      result.inc

proc flipToWhite(tileToCheck: HexCoords, blackTiles: TilesSet): bool =
  assert tileToCheck in blackTiles
  let blackTilesCount = adjacentBlackTilesCount(tileToCheck, blackTiles)
  blackTilesCount == 0 or blackTilesCount > 2

proc flipToBlack(tileToCheck: HexCoords, blackTiles: TilesSet): bool =
  assert tileToCheck notin blackTiles
  adjacentBlackTilesCount(tileToCheck, blackTiles) == 2

proc solvePartTwo(blackTiles: TilesSet): int =
  var blackTiles = blackTiles
  for day in 0 ..< 100:
    var checkedTiles, newBlackTiles: TilesSet
    for tile in blackTiles:
      var tilesToCheck = @[tile]
      for shift in shifts:
        tilesToCheck.add tile + shift

      for tileToCheck in tilesToCheck:
        if tileToCheck in checkedTiles:
          continue

        if tileToCheck in blackTiles and
           not flipToWhite(tileToCheck, blackTiles):
          newBlackTiles.incl tileToCheck

        if tileToCheck notin blackTiles and
           flipToBlack(tileToCheck, blackTiles):
          newBlackTiles.incl tileToCheck
        
        checkedTiles.incl tileToCheck
    blackTiles = newBlackTiles
  blackTiles.card

let tiles = readInput("input.txt")
let blackTileCoords = solvePartOne(tiles)
echo blackTileCoords.card
echo solvePartTwo(blackTileCoords)
