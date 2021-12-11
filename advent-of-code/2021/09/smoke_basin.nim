import strutils, sequtils, algorithm

proc isLowPoint(hightmap: seq[string], r, c: int): bool =
  if r - 1 >= 0 and hightmap[r - 1][c] <= hightmap[r][c]:
    return false
  if r + 1 < hightmap.len and hightmap[r + 1][c] <= hightmap[r][c]:
    return false
  if c - 1 >= 0 and hightmap[r][c - 1] <= hightmap[r][c]:
    return false
  if c + 1 < hightmap[r].len and hightmap[r][c + 1] <= hightmap[r][c]:
    return false
  return true

proc solvePartOne(hightmap: seq[string]): int =
  var lowPoints: seq[int]
  for r in 0 ..< hightmap.len:
    for c in 0 ..< hightmap[r].len:
      if isLowPoint(hightmap, r, c):
        lowPoints.add ord(hightmap[r][c]) - ord('0')
  for lowPoint in lowPoints:
    result += 1 + lowPoint

proc dfs(hightmap: seq[string], visited: var seq[seq[bool]], r, c: int): int =
  if r < 0 or r >= hightmap.len or c < 0 or c >= hightmap[0].len or
     hightmap[r][c] == '9' or visited[r][c]:
    return 0
  visited[r][c] = true
  result = dfs(hightmap, visited, r - 1, c) +
           dfs(hightmap, visited, r + 1, c) +
           dfs(hightmap, visited, r, c - 1) +
           dfs(hightmap, visited, r, c + 1) + 1

proc solvePartTwo(hightmap: seq[string]): int =
  var visited = newSeq[seq[bool]](hightmap.len)
  for i in 0 ..< hightmap.len:
    visited[i].setLen(hightmap[i].len)
  var basins: seq[int]
  for r in 0 ..< hightmap.len:
    for c in 0 ..< hightmap[r].len:
      if hightmap[r][c] != '9' and not visited[r][c]:
        basins.add dfs(hightmap, visited, r, c)
  basins.sort
  basins[^1] * basins[^2] * basins[^3]

let hightmap = "input.txt".lines.toSeq
echo solvePartOne(hightmap)
echo solvePartTwo(hightmap)
