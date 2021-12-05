import sequtils

proc readInput(fileName: string): seq[string] =
  fileName.lines.toSeq

proc calculatePowerConsumption(data: seq[string]): int =
  var gammaRate = 0
  var epsilonRate = 0
  for i in 0 ..< data[0].len:
    var oneBitsCount = 0
    for j in 0 ..< data.len:
      if data[j][i] == '1':
        oneBitsCount.inc
    let value = 1 shl (data[0].len - i - 1)
    if oneBitsCount > data.len div 2:
      gammaRate += value
    else:
      epsilonRate += value
  gammaRate * epsilonRate

proc binaryStringToDecimal(s: string): int = 
  var powerOfTwo = 1
  for i in countdown(s.len - 1, 0):
    result += (ord(s[i]) - ord('0')) * powerOfTwo
    powerOfTwo = powerOfTwo shl 1

proc calculateRating(data: seq[string],
    getTargetBit: proc (oneBitsCount, zeroBitsCount: int): char): int =
  var data = data
  for i in 0 ..< data[0].len:
    var zeroBitsCount = 0
    var oneBitsCount = 0
    for j in 0 ..< data.len:
      if data[j][i] == '0':
        zeroBitsCount.inc
      else:
        oneBitsCount.inc
    let targetBit = getTargetBit(oneBitsCount, zeroBitsCount)
    var newData = newSeqOfCap[string](data.len)
    for j in 0 ..< data.len:
      if data[j][i] == targetBit:
        newData.add data[j]
    data = newData
    if data.len == 1:
      return binaryStringToDecimal(data[0])

proc calculateOxygenGeneratorRating(data: seq[string]): int =
  calculateRating(data, proc (oneBitsCount, zeroBitsCount: int): char =
    if oneBitsCount >= zeroBitsCount: '1' else: '0')

proc calculateCO2ScrubberRating(data: seq[string]): int =
  calculateRating(data, proc (oneBitsCount, zeroBitsCount: int): char =
    if zeroBitsCount <= oneBitsCount: '0' else: '1')

proc calculateLifeSupportRating(data: seq[string]): int =
  calculateOxygenGeneratorRating(data) * calculateCO2ScrubberRating(data)

let data = "input.txt".readInput
echo data.calculatePowerConsumption
echo data.calculateLifeSupportRating
