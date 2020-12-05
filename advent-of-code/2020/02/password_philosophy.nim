import strscans

const
  inputFileName = "input.txt"

proc isPartOnePasswordValid(
    password: string, minLetterCount, maxLetterCount: int, letter: char): bool =
  var letterCount = 0
  for c in password:
    if c == letter:
      letterCount.inc
  letterCount >= minLetterCount and letterCount <= maxLetterCount

proc isPartTwoPasswordValid(
    password: string, firstLetterIndex, secondLetterIndex: int,
    letter: char): bool =
  password[firstLetterIndex - 1] == letter xor
  password[secondLetterIndex - 1] == letter

var
  partOneValidPasswordsCount = 0
  partTwoValidPasswordsCount = 0

for line in inputFileName.lines:
  var
    first, second: int
    letter, password: string
  discard scanf(line, "$i-$i $w: $w", first, second, letter, password)
  if password.isPartOnePasswordValid(first, second, letter[0]):
    partOneValidPasswordsCount.inc
  if password.isPartTwoPasswordValid(first, second, letter[0]):
    partTwoValidPasswordsCount.inc

echo partOneValidPasswordsCount
echo partTwoValidPasswordsCount
