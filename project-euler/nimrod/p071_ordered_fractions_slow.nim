import sugar

const initialDenominator = 999_999
const upperBound = 3 / 7

var lowerBound = 2 / 5
var answer = 1

func gcd(x, y: int): int =
  var x = x
  var y = y
  while y != 0:
    let temp = y
    y = x mod y
    x = temp
  return x

for denominator in countdown(initialDenominator, 2):
  var count = 0
  for numerator in countdown(denominator div 2, 1):
    let number = numerator / denominator
    count.inc
    if number < upperBound:
      if number > lowerBound and gcd(numerator, denominator) == 1:
        lowerBound = number
        answer = numerator
      break

dump(answer)
