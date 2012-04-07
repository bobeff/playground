import std.stdio;
import std.math;
import std.array;
import std.conv;
import std.random;
import std.algorithm;

/// Graph reporesented like list of edges
struct Graph {
	uint verticesCount;
	uint[2][] edges;
}

uint findMinimalCut(ref Graph g) {
	uint randomContraction(Graph g) {
		while (g.verticesCount > 2) {
			uint edgeIndex = uniform(0, g.edges.length);
			auto edge = g.edges[edgeIndex];
			uint u = edge[0], v = edge[1];
			for (uint i = 0; i < g.edges.length; ++i) {
				if (g.edges[i][0] == v) g.edges[i][0] = u;
				if (g.edges[i][1] == v) g.edges[i][1] = u;
				if (g.edges[i][0] == g.edges[i][1]) {
					g.edges[i--] = g.edges[$ - 1];
					--g.edges.length;
				}
			}
			--g.verticesCount;
		}
		return g.edges.length;
	}
	uint bestResult = g.edges.length;
	foreach (i; 0 .. pow(g.verticesCount, 3)) {
		uint currentResult = randomContraction(Graph(g.verticesCount, g.edges.dup));
		if (currentResult < bestResult) bestResult = currentResult;
	}
	return bestResult;
}

void main() {
	Graph g;
	auto f = File("KargerAdj.txt");
	foreach (line; f.byLine()) {
		++g.verticesCount;
		auto vertices = map!(to!uint)(split(line));
		foreach (i; 1 .. vertices.length)
			if (vertices[0] < vertices[i])
				g.edges ~= [vertices[0], vertices[i]];
	}
	writeln(findMinimalCut(g));
}
