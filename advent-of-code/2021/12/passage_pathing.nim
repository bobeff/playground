import sequtils, strutils, tables, sets

type Graph = Table[string, seq[string]]

proc buildGraph(edges: seq[seq[string]]): Graph =
  for edge in edges:
    if edge[0] notin result:
      result[edge[0]] = @[]
    if edge[1] notin result:
      result[edge[1]] = @[]
    result[edge[0]].add(edge[1])
    result[edge[1]].add(edge[0])

proc solvePartOne(graph: Graph, node: string): int =
  var visited {.global.}: HashSet[string]
  if node == "end":
    return 1
  if node[0].isLowerAscii:
    visited.incl node
  for v in graph[node]:
    if v notin visited:
      result += solvePartOne(graph, v)
  visited.excl node

proc solvePartTwo(graph: Graph, node: string): int =
  var visited {.global.}: HashSet[string]
  var doubleVisited {.global.}: string
  if node == "end":
    return 1
  if node in visited:
    doubleVisited = node
  elif node[0].isLowerAscii:
    visited.incl node
  for v in graph[node]:
    if v notin visited or doubleVisited == "" and v != "start":
      result += solvePartTwo(graph, v)
  if doubleVisited == node:
    doubleVisited = ""
  else:
    visited.excl node

let edges = "input.txt".lines.toSeq.mapIt(it.split('-'))
let graph = buildGraph(edges)
echo solvePartOne(graph, "start")
echo solvePartTwo(graph, "start")
