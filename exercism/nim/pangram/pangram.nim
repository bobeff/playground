import strutils

proc isPangram*(sentence: string): bool =
  var usedChars: array[26, bool]
  for c in sentence.toLowerAscii:
    if c in {'a' .. 'z'}:
      usedChars[ord(c) - ord('a')] = true
  for used in usedChars:
    if not used:
      return false
  return true
