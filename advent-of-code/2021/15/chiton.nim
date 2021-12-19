import sequtils, heapqueue

type
  HqItem = object
    r, c: int
    dist: uint

  Map = seq[seq[uint8]]

proc `<`(lhs, rhs: HqItem): bool =
  lhs.dist < rhs.dist

proc readInput(fileName: string): Map =
  fileName.lines.toSeq.mapIt(it.mapIt((it.ord - '0'.ord).uint8))

proc dijkstra(map: Map): uint =
  let
    rs = map.len
    cs = map[0].len

  var
    visited = newSeqOfCap[seq[bool]](rs)
    dist = newSeqOfCap[seq[uint]](rs)
    priorityQueue: HeapQueue[HqItem]

  for r in 0 ..< rs:
    visited.add newSeqOfCap[bool](cs)
    dist.add newSeqOfCap[uint](cs)
    for c in 0 ..< cs:
      visited[r].add false
      dist[r].add uint.high

  dist[0][0] = 0
  priorityQueue.push(HqItem(r: 0, c: 0))

  while priorityQueue.len > 0:
    let
      item = priorityQueue.pop
      r = item.r
      c = item.c
      minDist = dist[r][c]

    if visited[r][c]:
      continue

    if r == rs - 1 and c == cs - 1:
      return minDist

    visited[r][c] = true

    if r - 1 >= 0 and not visited[r - 1][c]:
      dist[r - 1][c] = min(dist[r - 1][c], minDist + map[r - 1][c])
      priorityQueue.push(HqItem(r: r - 1, c: c, dist: dist[r - 1][c]))

    if r + 1 < rs and not visited[r + 1][c]:
      dist[r + 1][c] = min(dist[r + 1][c], minDist + map[r + 1][c])
      priorityQueue.push(HqItem(r: r + 1, c: c, dist: dist[r + 1][c]))

    if c - 1 >= 0 and not visited[r][c - 1]:
      dist[r][c - 1] = min(dist[r][c - 1], minDist + map[r][c - 1])
      priorityQueue.push(HqItem(r: r, c: c - 1, dist: dist[r][c - 1]))

    if c + 1 < cs and not visited[r][c + 1]:
      dist[r][c + 1] = min(dist[r][c + 1], minDist + map[r][c + 1])
      priorityQueue.push(HqItem(r: r, c: c + 1, dist: dist[r][c + 1]))

  dist[^1][^1]

proc extendMap(map: Map): Map =
  result = newSeqOfCap[seq[uint8]](map.len * 5)
  for r in 0 ..< map.len * 5:
    result.add newSeqOfCap[uint8](map[0].len * 5)
    for c in 0 ..< map[0].len * 5:
      if r < map.len and c < map[r].len:
        result[r].add map[r][c]
      elif r < map.len:
        result[r].add result[r][c - map[0].len] + 1
      else:
        result[r].add result[r - map.len][c] + 1
      if result[r][c] == 10:
        result[r][c] = 1

let map = readInput("input.txt")
echo dijkstra(map)
echo dijkstra(extendMap(map))
