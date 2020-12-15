import tables, strutils, sequtils

proc solve(fileName: string, finalTurn: int): int =
  var
    memory: Table[int, int]
    lastNumber, nextNumber: int

  let numbers = fileName.readFile.split(',').mapIt(it.parseInt)
  for i in 0 ..< numbers.len - 1:
    memory[numbers[i]] = i + 1
  lastNumber = numbers[^1]

  for currentTurn in memory.len + 2 .. finalTurn:
    if not memory.hasKey(lastNumber):
      nextNumber = 0
    else:
      nextNumber = (currentTurn - 1) - memory[lastNumber]
    memory[lastNumber] = currentTurn - 1
    lastNumber = nextNumber
  lastNumber

echo solve("input.txt", 2020)
echo solve("input.txt", 30_000_000)
