import strutils

proc isIsogram*(word: string): bool =
  var letters: array[26, bool]
  for c in word.toLowerAscii:
    if not isAlphaAscii(c):
      continue
    let index = ord(c) - ord('a')
    if letters[index]:
      return false
    letters[index] = true
  return true
