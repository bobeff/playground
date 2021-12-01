import sequtils, strutils

proc countIncreases(depths: seq[uint], window: int): uint =
  var previousSum = uint.high
  for i in 0 .. depths.len - window:
    var sum = 0'u
    for j in i ..< i + window:
      sum += depths[j]
    if previousSum < sum:
      result.inc
    previousSum = sum

let depths = "input.txt".lines.toSeq.mapIt(it.parseUInt)
echo countIncreases(depths, 1)
echo countIncreases(depths, 3)
