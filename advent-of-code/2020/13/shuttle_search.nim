import strutils

var
  timeStamp: uint
  busIds: seq[uint]
  minutesAfter: seq[uint]

proc readInput(fileName: string) =
  let fileLines = fileName.readFile.split('\n')
  timeStamp = fileLines[0].parseUInt
  let tokens = fileLines[1].split(',')
  for i, token in tokens:
    if token != "x":
      busIds.add parseUInt(token)
      minutesAfter.add i.uint

proc waitTime(timeStamp, busId: uint): uint =
  return
    if timeStamp mod busId == 0:
      0'u
    else:
      timeStamp div busId * busId + busId - timeStamp

proc solvePartOne(): uint =
  var
    minWait = uint.high
    bestId = 0'u
  for busId in busIds:
    let waitForBus = waitTime(timeStamp, busId)
    if waitForBus < minWait:
      minWait = waitForBus
      bestId = busId
  minWait * bestId

proc mod_power(base, exponent, module: uint): uint =
  ## Fast Modular Exponentiation
  var
    base = base mod module
    exponent = exponent

  if base == 0:
      return 0
  
  result = 1
  while exponent > 0: 
    if exponent mod 2 == 1:
      result = result * base mod module 
    exponent = exponent div 2
    base = base * base mod module 

proc modularInverse(value, module: uint): uint =
  mod_power(value, module - 2, module)

proc solvePartTwo(): uint =
  ## Uses Chinese Reminder Theorem
  let n = busIds.len

  for i in 0 ..< n:
    minutesAfter[i] = busIds[i] - minutesAfter[i]

  var
    N = newSeqOfCap[uint](n)
    NN = 1u

  for i in 0 ..< n:
    N.add 1
    NN *= busIds[i]
    for j in 0 ..< n:
      if j != i:
        N[i] *= busIds[j]

  var X = newSeqOfCap[uint](n)
  for i in 0 ..< n:
    X.add modularInverse(N[i], busIds[i])

  for i in 0 ..< n:
    result += minutesAfter[i] * N[i] * X[i]
  
  result mod NN

readInput("input.txt")
echo solvePartOne()
echo solvePartTwo()
