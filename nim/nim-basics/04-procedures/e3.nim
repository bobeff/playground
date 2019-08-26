# Points in 2D plane can be represented as tuple[x, y: float]. Write a
# procedure which will receive two points and return a new point which is a sum # of those two points (add x’s and y’s separately).

type Point = tuple[x, y: float]

proc `+`(a, b: Point): Point =
  result.x = a.x + b.x
  result.y = a.y + b.y

doAssert (1.0, 2.0) + (3.0, 4.0) == (4.0, 6.0)
