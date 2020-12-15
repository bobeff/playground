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

proc solvePartTwo(): uint =
  discard

readInput("sample_input_1.txt")
echo solvePartOne()
echo solvePartTwo() 
