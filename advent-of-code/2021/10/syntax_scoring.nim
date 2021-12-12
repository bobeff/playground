import sequtils, strformat, algorithm

proc opposite(c: char): char =
  case c
  of ')': return '('
  of ']': return '['
  of '}': return '{'
  of '>': return '<'
  else: raiseAssert(&"Invalid character {c}.")

proc solvePartOne(lines: seq[string]): int =
  for line in lines:
    var stack: seq[char]
    for c in line:
      case c
      of '(', '[', '{', '<':
        stack.add c
      of ')', ']', '}', '>':
        if stack.len > 0 and stack[^1] == opposite(c):
          discard stack.pop
        else:
          case c
          of ')': result += 3
          of ']': result += 57
          of '}': result += 1197
          of '>': result += 25137
          else: raiseAssert(&"Invalid character {c}.")
          break
      else: raiseAssert(&"Invalid character {c}.")

proc solvePartTwo(lines: seq[string]): int =
  var scores: seq[int]
  for line in lines:
    var isLineCorrupted = false
    var stack: seq[char]
    for c in line:
      case c
      of '(', '[', '{', '<':
        stack.add c
      of ')', ']', '}', '>':
        if stack.len == 0 or stack[^1] != opposite(c):
          isLineCorrupted = true
          break
        else:
          discard stack.pop
      else: raiseAssert(&"Invalid character {c}.")
    if isLineCorrupted:
      continue
    var score = 0
    while stack.len > 0:
      let c = stack.pop
      score *= 5
      case c
      of '(': score += 1
      of '[': score += 2
      of '{': score += 3
      of '<': score += 4
      else: raiseAssert(&"Invalid character {c}.")
    scores.add score
  scores.sort
  scores[scores.len div 2]

let lines = "input.txt".lines.toSeq
echo solvePartOne(lines)
echo solvePartTwo(lines)
