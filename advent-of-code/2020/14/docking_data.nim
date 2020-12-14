import strscans, strutils, parseutils, tables

const
  maskPrefix = "mask = "

var
  mask: string
  key, value: uint64
  memory: Table[uint64, uint64]

proc resetBit(value: uint64, pos: int): uint64 =
  var mask = 1'u64 shl pos
  mask = mask xor uint64.high
  value and mask

proc setBit(value: uint64, pos: int): uint64 =
  let mask = 1'u64 shl pos
  value or mask

proc applyMaskPartOne(value: uint64, mask: string): uint64 =
  result = value
  for i in 0 ..< mask.len:
    let x = mask[mask.len - i - 1]
    if x == '0':
      result = resetBit(result, i)
    elif x == '1':
      result = setBit(result, i)

template solve(fileName: string, body: untyped) =
  for line in fileName.lines:
    if line.startsWith(maskPrefix):
      mask = line[maskPrefix.len .. ^1]
    else:
      discard scanf(line, "mem[${parseBiggestUInt}] = ${parseBiggestUInt}",
        key, value)
      body
  for key, value in memory:
    result += value

proc solvePartOne(fileName: string): uint64 =
  solve(fileName):
    memory[key] = applyMaskPartOne(value, mask)

proc hasX(mask: string): bool =
  for x in mask:
    if x == 'X':
      return true

proc applyMaskPartTwo(value: uint64, mask: string): uint64 =
  result = value
  for i in 0 ..< mask.len:
    let x = mask[mask.len - i - 1]
    if x == '1':
      result = setBit(result, i)
    elif x == '2':
      result = resetBit(result, i)

proc solvePartTwo(j: int) =
  for i in j ..< mask.len:
    if mask[i] == 'X':
      mask[i] = '2'
      solvePartTwo(i + 1)
      mask[i] = '1'
      solvePartTwo(i + 1)
      mask[i] = 'X'
  if not hasX(mask):
    memory[applyMaskPartTwo(key, mask)] = value

proc solvePartTwo(fileName: string): uint64 =
  solve(fileName):
    solvePartTwo(0)

echo solvePartOne("input.txt")
memory.clear
echo solvePartTwo("input.txt")
