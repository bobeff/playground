import tables, strutils, sequtils

type
  Node = tuple[color: string, count: uint]
  Graph = Table[string, seq[Node]]

proc readInput(fileName: string): Graph =
  for line in fileName.lines:
    let tokens = line.split({',', '.', ' '}).filterIt(it != "")
    var parseNeighbours = false
    var startNode = ""
    for i, token in tokens:
      if token == "contain":
        parseNeighbours = true
      if token.startsWith("bag"):
        let node = tokens[i - 2] & " " & tokens[i - 1]
        if node == "no other":
          continue
        if not parseNeighbours:
          startNode = node
          result[startNode] = @[]
        else:
          let count = parseUInt(tokens[i - 3])
          result[startNode].add (node, count)

proc hasRoute(graph: Graph, startNode, endNode: string): bool =
  if startNode == endNode:
    return true
  for node in graph[startNode]:
    result = result or graph.hasRoute(node.color, endNode)

proc solvePartOne(graph: Graph, endNode: string): uint =
  for node, _ in graph:
    if node != endNode and graph.hasRoute(node, endNode):
      result.inc

proc solvePartTwo(graph: Graph, startNode: string): uint =
  for (color, count) in graph[startNode]:
    result += count + count * graph.solvePartTwo(color)

let graph = readInput("input.txt")
echo solvePartOne(graph, "shiny gold")
echo solvePartTwo(graph, "shiny gold")
