import sequtils

const
  boardSide = 10
  shifts = [
    (r: -1, c: -1),
    (r: -1, c:  0),
    (r: -1, c:  1),
    (r:  0, c: -1),
    (r:  0, c:  1),
    (r:  1, c: -1),
    (r:  1, c:  0),
    (r:  1, c:  1)]

proc solve(octopuses: seq[seq[int]], steps: int): int =
  var octopuses = octopuses
  var allFlashes = 0
  for step in 0 ..< steps:
    for r in 0 ..< boardSide:
      for c in 0 ..< boardSide:
        octopuses[r][c].inc
    var used: array[10, array[10, bool]]
    var hasFlashes = true
    var stepFlashes = 0
    while hasFlashes:
      hasFlashes = false
      for r in 0 ..< boardSide:
        for c in 0 ..< boardSide:
          if octopuses[r][c] == 10:
            octopuses[r][c] = 0
            hasFlashes = true
            stepFlashes.inc
            allFlashes.inc
      if hasFlashes:
        for r in 0 ..< boardSide:
          for c in 0 ..< boardSide:
            if octopuses[r][c] == 0 and not used[r][c]:
              for shift in shifts:
                let rr = r + shift.r
                let cc = c + shift.c
                if rr >= 0 and rr < boardSide and cc >= 0 and cc < boardSide:
                  if octopuses[rr][cc] != 0 and octopuses[rr][cc] != 10:
                    octopuses[rr][cc].inc
              used[r][c] = true
    if stepFlashes == boardSide * boardSide:
      return step + 1
  return allFlashes

let octopuses = "input.txt".lines.toSeq.mapIt(it.mapIt(it.ord - '0'.ord))
echo solve(octopuses, 100)
echo solve(octopuses, int.high)
