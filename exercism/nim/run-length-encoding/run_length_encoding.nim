import std/[strutils, parseutils]

proc encode*(s: string): string =
  var count = 1
  for i in 0 ..< s.len:
    if i + 1 < s.len and s[i] == s[i + 1]:
      count.inc
    else:
      if count > 1:
        result &= $count
      result &= s[i]
      count = 1

proc decode*(s: string): string =
  var numStr = ""
  for i in 0 ..< s.len:
    if s[i].isDigit:
      numStr &= s[i] 
    else:
      if numStr == "":
        numStr = "1"
      var num: uint
      discard parseUInt(numStr, num)
      result &= repeat(s[i], num)
      numStr = ""
