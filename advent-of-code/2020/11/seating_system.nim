import strutils, sequtils

type
  Seats = seq[string]
  CheckProc = proc (seat: char, seats: Seats, r, c: int): bool

const shifts = @[
  (-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

proc readInput(fileName: string): Seats =
  fileName.readFile.split('\n')

proc validRowCol(r, c: int, seats: Seats): bool =
  c >= 0 and c < seats[0].len and r >= 0 and r < seats.len

proc adjacentOccupiedSeatsCount(seats: Seats, r, c: int): uint =
  for shift in shifts:
    let
      cc = c + shift[1]
      rr = r + shift[0]
    if validRowCol(rr, cc, seats) and seats[rr][cc] == '#':
      result.inc

proc visibleOccupiedSeatsCount(seats: Seats, r, c: int): uint =
  for shift in shifts:
    var
      cc = c + shift[1]
      rr = r + shift[0]
    while validRowCol(rr, cc, seats) and seats[rr][cc] == '.':
      cc = cc + shift[1]
      rr = rr + shift[0]
    if validRowCol(rr, cc, seats) and seats[rr][cc] == '#':
      result.inc

proc makeSeatOccupiedPartOne(seat: char, seats: Seats, r, c: int): bool =
  seat == 'L' and adjacentOccupiedSeatsCount(seats, r, c) == 0

proc makeSeatEmptyPartOne(seat: char, seats: Seats, r, c: int): bool =
  seat == '#' and adjacentOccupiedSeatsCount(seats, r, c) >= 4

proc makeSeatOccupiedPartTwo(seat: char, seats: Seats, r, c: int): bool =
   seat == 'L' and visibleOccupiedSeatsCount(seats, r, c) == 0

proc makeSeatEmptyPartTwo(seat: char, seats: Seats, r, c: int): bool =
   seat == '#' and visibleOccupiedSeatsCount(seats, r, c) >= 5

proc countOccupiedSeats(seats: Seats): uint =
  for row in seats:
    for col in row:
      if col == '#':
        result.inc

proc solve(seats: Seats, makeSeatOccupied, makeSeatEmpty: CheckProc): uint =
  var
    seats = seats
    newSeats = newSeqWith(seats.len, '.'.repeat(seats[0].len))
  while true:
    for r in 0 ..< seats.len:
      for c in 0 ..< seats[r].len:
        if makeSeatOccupied(seats[r][c], seats, r, c):
          newSeats[r][c] = '#'
        elif makeSeatEmpty(seats[r][c], seats, r, c):
          newSeats[r][c] = 'L'
        else:
          newSeats[r][c] = seats[r][c]
    if seats == newSeats:
      break
    else:
      seats = newSeats
  return countOccupiedSeats(seats)

proc solvePartOne(seats: Seats): uint =
  solve(seats, makeSeatOccupiedPartOne, makeSeatEmptyPartOne)

proc solvePartTwo(seats: Seats): uint =
  solve(seats, makeSeatOccupiedPartTwo, makeSeatEmptyPartTwo)

let seats = readInput("input.txt")
echo solvePartOne(seats)
echo solvePartTwo(seats)
