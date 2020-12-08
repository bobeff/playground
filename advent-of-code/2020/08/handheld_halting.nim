import strutils,sequtils

type
  Instruction = enum acc, jmp, nop

  Program = object
    instructions: seq[Instruction]
    arguments: seq[int]

proc readProgram(fileName: string): Program =
  for line in fileName.lines:
    let parts = line.split(' ')
    result.instructions.add case parts[0]
      of "acc": acc
      of "jmp": jmp
      of "nop": nop
      else: raiseAssert("Invalid instruction: " & parts[0])
    result.arguments.add parseInt(parts[1])

proc executeProgram(program: Program):
    tuple[hasInfiniteLoop: bool, accumulator: int] =
  let programLength = program.instructions.len
  var alreadyExecuted = newSeqWith(programLength, false)
  var ip = 0
  while ip < programLength and not alreadyExecuted[ip]:
    alreadyExecuted[ip] = true
    case program.instructions[ip]
      of acc:
        result.accumulator += program.arguments[ip]
        ip.inc
      of jmp:
        ip += program.arguments[ip]
      of nop:
        ip.inc
  if ip < programLength:
    result.hasInfiniteLoop = true

proc fixProgram(program: var Program): int =
  template executeFixedProgram() =
    let (hasInfiniteLoop, accumulator) = executeProgram(program)
    if not hasInfiniteLoop:
      result = accumulator
      break
    program.instructions[i] = op

  for i, op in program.instructions:
    case op
    of jmp:
      program.instructions[i] = nop
      executeFixedProgram()
    of nop:
      program.instructions[i] = jmp
      executeFixedProgram()
    of acc: discard

var program = readProgram("input.txt")
echo executeProgram(program).accumulator
echo fixProgram(program)
