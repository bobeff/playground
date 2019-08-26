# Create a procedure findMax3 which will return the largest of three values.

proc findMax3[T](x, y, z: T): T =
  if x >= y:
    if x >= z:
      return x
    else:
      return z
  elif y >= z:
    return y
  else:
    return z

doAssert findMax3(1, 1, 1) == 1
doAssert findMax3(1, 1, 0) == 1
doAssert findMax3(1, 0, 0) == 1
doAssert findMax3(1, 3, 2) == 3
doAssert findMax3(1, 3, 3) == 3
doAssert findMax3(1, 1, 2) == 2
doAssert findMax3(3, 2, 1) == 3
doAssert findMax3(2, 1, 2) == 2
doAssert findMax3(1, 2, 1) == 2

echo "All tests passed."
