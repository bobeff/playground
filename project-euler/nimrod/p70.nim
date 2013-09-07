proc isPerm(x, y : natural) : bool =
  var xx = x
  var yy = y
  var dx, dy : array[0 .. 9, int8]
  while xx > 0 and yy > 0:
    inc(dx[xx mod 10])
    inc(dy[yy mod 10])
    xx = xx div 10
    yy = yy div 10
  if xx > 0 or yy > 0:
    return false
  for i in 0 .. 9:
    if dx[i] != dy[i]:
      return false
  return true

const MAX = 10_000_000
var totientSieve : array[1 .. MAX, natural]
for i in 1 .. MAX:
  totientSieve[i] = i
for i in 1 .. MAX:
  var j = 2 * i;
  while j <= MAX:
    totientSieve[j] -= totientSieve[i]
    j += i
var bestNumber : natural
var minRatio = toFloat(MAX)
for i in 2 .. MAX:
  if isPerm(i, totientSieve[i]):
    var ratio = toFloat(i) / toFloat(totientSieve[i])
    if (ratio < minRatio):
      minRatio = ratio
      bestNumber = i
echo(bestNumber)
