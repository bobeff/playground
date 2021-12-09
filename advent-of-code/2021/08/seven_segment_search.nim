import strutils, sequtils, algorithm, tables

type Entry = tuple[signalPatterns, digitsOutput: seq[string]]

proc readInput(fileName: string): seq[Entry] =
  fileName.readFile.strip.splitLines.mapIt(it.split('|')).mapIt(
    (it[0].strip.split, it[1].strip.split))

proc solvePartOne(data: seq[Entry]): int =
  const s = {2, 3, 4, 7}
  for entry in data:
    for i in 0 ..< 4:
      if entry.digitsOutput[i].len in s:
        result.inc

proc decode(pattern, perm: string): string =
  for i in 0 ..< pattern.len:
    result &= perm[ord(pattern[i]) - ord('a')]
  result.sort

proc solvePartTwo(data: seq[Entry]): int =
  const correctPatterns = {
    "abcefg": 0, "cf": 1, "acdeg": 2, "acdfg": 3, "bcdf": 4,
    "abdfg": 5, "abdefg": 6, "acf": 7, "abcdefg": 8, "abcdfg": 9}.toTable

  for entry in data:
    var letters = "abcdefg"
    while true:
      var usedNumbers: set[int8]
      for pattern in entry.signalPatterns:
        let decoded = decode(pattern, letters)
        if decoded in correctPatterns:
          usedNumbers.incl correctPatterns[decoded].int8
      if card(usedNumbers) == 10:
        var number = 0
        for digit in entry.digitsOutput:
          number = number * 10 + correctPatterns[decode(digit, letters)]
        result += number
        break
      if not letters.nextPermutation:
        break

let data = readInput("input.txt")
echo solvePartOne(data)
echo solvePartTwo(data)
