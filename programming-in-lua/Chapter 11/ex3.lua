--[[
    Modify the graph structure so that it keeps a label for each arc. The
structure should represent each arc by an object, too, with two fields"
its label and the node it points to. Instead of an adjasent set, each
node keeps an incident set that contains the arcs that originate at the
node.
    Adapt the readgraph function to read two node names plus a label from
each line in the input file. (Assume that the label is a number.)
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

local function printGraph(g)
    for _, nodeFrom in pairs(g) do
        io.write(nodeFrom.name, " ")
        for nodeTo, weight in pairs(nodeFrom.adj) do
            io.write(string.format("(%s, %d) ", nodeTo.name, weight))
        end
        io.write("\n")
    end
end

local graph1 = readGraph("graph.txt")
printGraph(graph1)
print()
local graph2 = readGraph("graph2.txt", true)
printGraph(graph2)
