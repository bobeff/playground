## To compile this use `nim c -d:danger --gc:none din.nim` command line.

import strutils, sequtils, algorithm, math, sets

type
  Triple = array[3, uint32]
  Move = array[2, uint8]

proc readInput: Triple =
  let inputSeq = stdin.readLine.split(' ').mapIt(it.parseUint)
  return [uint32(inputSeq[0]), uint32(inputSeq[1]), uint32(inputSeq[2])]

proc sorted(triple: Triple): Triple =
  result = triple
  if result[0] > result[2]:
    swap(result[0], result[2])
  if result[0] > result[1]:
    swap(result[0], result[1])
  if result[1] > result[2]:
    swap(result[1], result[2])

proc solve(input: Triple): seq[Move] =
  let capacity = 3 ^ input[2].float.log(2).ceil.int
  var
    i = 0'u32
    triples = newSeqOfCap[Triple](capacity)
    moves = newSeqOfCap[Move](capacity)
    cameFrom = newSeqOfCap[uint32](capacity)
    alreadyVisited: HashSet[Triple]

  proc constructMove(a, b: uint8): bool =
    var triple = triples[i]
    let (a, b) = if triple[a] > triple[b]: (b, a) else: (a, b)
    triple[b] -= triple[a]
    triple[a] = triple[a] shl 1
    let sortedTriple = triple.sorted
    if sortedTriple in alreadyVisited:
      return false
    triples.add triple
    moves.add [b, a]
    cameFrom.add i
    alreadyVisited.incl sortedTriple
    sortedTriple[0] == 0

  proc moveSequence: seq[Move] =
    result = newSeqOfCap[Move](moves.len.float.log(3).ceil.int)
    var i = uint32(moves.len - 1)
    while i > 0:
      result.add moves[i]
      i = cameFrom[i]
    result.reverse

  triples.add input
  moves.add [0'u8, 0'u8]
  cameFrom.add 0
  alreadyVisited.incl input

  while true:
    if constructMove(1, 2):
      return moveSequence()
    if constructMove(0, 2):
      return moveSequence()
    if constructMove(0, 1):
      return moveSequence()
    i.inc

proc printOutput(moves: seq[Move]) =
  for move in moves:
    echo chr(ord('A') + move[0]), "->", chr(ord('A') + move[1])

readInput().solve.printOutput
