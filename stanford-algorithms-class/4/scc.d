// To avoid stack overflow compile program with incresed stack size
// dmd scc.d -O -release -L/STACK:1000000000

import std.stdio;
import std.array;
import std.conv;
import std.algorithm;

/// Graph represented with adjacency list
alias uint[][] Graph;

Graph reversed(const ref Graph g) {
	Graph g_rev;
	g_rev.length = g.length;
	foreach (node; 0 .. g.length)
		foreach (neighbour; g[node])
			g_rev[neighbour] ~= node;
	return g_rev;
}

unittest {
	Graph g = [[3], [7], [5], [6], [1], [8], [0], [4, 5], [2, 6]];
	Graph g_rev = reversed(g);
	assert(g_rev == [[6], [4], [8], [0], [7], [2, 7], [3, 8], [1], [5]]);
}

/// Finds strongly connected components by Kosaraju's algorithm
/// Returns the size of each component
uint[][] findSCC(const ref Graph g) {
	uint[] f = new uint[g.length]; // magic ordering for each node
	void DFS_Loop_1(const ref Graph g) {
		uint magic_number = 0;
		bool[] visited = new bool[g.length];
		void dfs(uint node) {
			visited[node] = true;
			foreach (neighbour; g[node])
				if (!visited[neighbour])
					dfs(neighbour);
			f[magic_number++] = node;
		}
		foreach (node; 0 .. g.length)
			if (!visited[node])
				dfs(node);
	}
	uint[][] components;
	void DFS_Loop_2(const ref Graph g) {
		uint start_node = 0;
		bool[] visited = new bool[g.length];
		void dfs(uint node) {
			visited[node] = true;
			components[$ - 1] ~= node;
			foreach (neighbour; g[node])
				if (!visited[neighbour])
					dfs(neighbour);
		}
		for (int i = f.length - 1; i >= 0; --i)
			if (!visited[f[i]]) {
				++components.length;
				dfs(f[i]);
			}
	}
	// construc reverse graph
	Graph g_rev = reversed(g);
	// compute magic ordering
	DFS_Loop_1(g_rev);
	// find SCC one by one
	DFS_Loop_2(g);
	return components;
}

unittest {
	Graph g1 = [[3], [7], [5], [6], [1], [8], [0], [4, 5], [2, 6]];
	uint[][] components = findSCC(g1);
	assert(components.length == 3);
	foreach (uint i; 0 .. 3) assert(components[i].length == 3);
	writeln(components);

	Graph g2 = [[1, 4],[2, 6],[7],[0],[3, 5, 8],[6, 9, 10],[2],[6],[5],[10],[8]];
	components = findSCC(g2);
	assert(components.length == 4);
	writeln(components);
}

void main() {
	Graph graph;
	auto finp = File("SCC.txt");
	foreach (line; finp.byLine()) {
		auto nodes = map!(to!uint)(split(line));
		if (nodes[0] > graph.length)
			graph.length = nodes[0];
		graph[nodes[0] - 1] ~= nodes[1] - 1;
	}
	auto components = findSCC(graph);
	writeln(components.length);
	uint[] componentSizes;
	foreach (ref comp; components)
		componentSizes ~= comp.length;
	sort!("a > b")(componentSizes);
	for (uint i = 0; i < 5; ++i)
		if (i < componentSizes.length)
			write(componentSizes[i], ",");
		else
			write("0,");
	writeln();
}
