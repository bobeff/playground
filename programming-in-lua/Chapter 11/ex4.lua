--[[
    Assume the graph representation of the previous exercise, where the
label of each arc represents the distance between its end nodes. Write a
function to find the shortest path between two given nodes.
]]

local function name2node(g, name)
    local node = g[name]
    if not node then
        node = { name = name, adj = {} }
        g[name] = node
    end
    return node
end

function readGraph(fileName, isDirected)
    isDirected = isDirected or false
    local graph = {}
    for line in io.lines(fileName) do
        local nameFrom, nameTo, edgeLabel =
            string.match(line, "(%S+)%s+(%S+)%s+(%S+)")
        local nodeFrom = name2node(graph, nameFrom)
        local nodeTo = name2node(graph, nameTo)
        local weight = tonumber(edgeLabel)
        nodeFrom.adj[nodeTo] = weight
        if not isDirected then
            nodeTo.adj[nodeFrom] = weight
        end
    end
    return graph
end

function dijkstra(graph, source, destination)

    local nodesCount = 0
    local dist = {}
    local visited = {}

    for _, node in pairs(graph) do
        dist[node] = math.huge
        visited[node] = false
        nodesCount = nodesCount + 1
    end

    local parent = {}
    local node = graph[source]
    dist[node] = 0

    for n, w in pairs(node.adj) do
        dist[n] = w
        parent[n] = node
    end

    visited[node] = true
    for i = 1, nodesCount - 1 do
        local minDistNode
        local minDist = math.huge

        for n, v in pairs(visited) do
            local dist = dist[n]
            if not v and dist < minDist then
                minDist = dist
                minDistNode = n
            end
        end

        if minDistNode == graph[destination] then break end

        for n, w in pairs(minDistNode.adj) do
            if w + minDist < dist[n] then
                dist[n] = w + minDist
                parent[n] = minDistNode
            end
        end

        visited[minDistNode] = true
    end

    local pathLength = 0
    local path = {}
    local destNode = graph[destination]
    if dist[destNode] == math.huge then return nil end

    while destNode do
        table.insert(path, destNode)
        destNode = parent[destNode]
    end

    for i = 1, #path / 2 do
        path[i], path[#path - i + 1] = path[#path - i + 1], path[i]
    end

    return dist[graph[destination]], path
end

local function printPath(path)
    for i = 1, #path do
        io.write(path[i].name, " ")
    end
    io.write("\n")
end

local graph1 = readGraph("graph.txt")
local minDist1, path1 = dijkstra(graph1, "New_York", "Vienna")
io.write("Minimum distance between New York and Vienna is "
         .. minDist1 .. "\nAnd the path is: ")
printPath(path1)

print()

local graph2 = readGraph("graph2.txt", true)
local minDist2, path2 = dijkstra(graph2, "1", "4")
io.write("Minimum distance between 1 and 4 is "
         .. minDist2 .. "\n And the path is: ")
printPath(path2)
