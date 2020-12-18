import strutils, tables

const
  precedencePartOne = {'(': 0, '*': 1, '+': 1}.toTable
  precedencePartTwo = {'(': 0, '*': 1, '+': 2}.toTable

proc evaluate(values: var seq[int], op: char) =
  let y = values.pop
  let x = values.pop
  values.add case op
    of '+': x + y
    of '*': x * y
    else: raiseAssert("Invalid operation: " & op)

proc solve(fileName: string, precedence: Table[char, int]): int =
  for line in fileName.lines:
    var tokens = newSeqOfCap[char](line.len)
    for c in line:
      if c != ' ':
        tokens.add c
    var valuesStack = newSeqOfCap[int](tokens.len)
    var operatorsStack = newSeqOfCap[char](tokens.len)
    for token in tokens:
      if token in Digits:
        valuesStack.add ord(token) - ord('0')
      elif token == '(':
        operatorsStack.add token
      elif token == ')':
        var operator = operatorsStack.pop
        while operator != '(':
          evaluate(valuesStack, operator)
          operator = operatorsStack.pop
      else:
        while operatorsStack.len > 0 and
              precedence[operatorsStack[^1]] >= precedence[token]:
          evaluate(valuesStack, operatorsStack.pop)
        operatorsStack.add token
    while operatorsStack.len > 0:
      evaluate(valuesStack, operatorsStack.pop)
    assert valuesStack.len == 1
    result += valuesStack.pop

echo solve("input.txt", precedencePartOne)
echo solve("input.txt", precedencePartTwo)
