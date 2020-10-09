func sums(x: int, min = 1): int =
  for i in min .. x:
    let y = x - i
    if y == 0:
      result.inc
    else:
      result.inc sums(y, i)

echo sums(100) - 1
