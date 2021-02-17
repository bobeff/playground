import strutils, sequtils, std/enumerate

proc abbreviate*(phrase: string): string =
  let words = phrase.split({' ', '_', '-'}).filterIt(it != "")
  result = newString(words.len)
  for i, word in enumerate(words):
    result[i] = words[i][0].toUpperAscii
