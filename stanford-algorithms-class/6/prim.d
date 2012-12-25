import std.stdio;
import std.c.stdio;

uint n, m;
int[2][][] graph;

void main() {
	auto f = File("edges.txt");
	fscanf(f.getFP(), "%d%d", &n, &m);
	graph.length = n;
	foreach (i; 0 .. m) {
		int u, v, w;
		fscanf(f.getFP(), "%d%d%d", &u, &v, &w);
		graph[u - 1] ~= [v - 1, w];
		graph[v - 1] ~= [u - 1, w];
	}
	debug writeln(graph);

	// Prim's algorithm
	bool[] used = new bool[n];
	int[] priority = new int[n];
	priority[0 .. $] = int.max;
	int lastChoosed = 0;
	int sum = 0;
	foreach (i; 0 .. n - 1) {
		used[lastChoosed] = true;
		// update priorities
		foreach (edge; graph[lastChoosed]) {
			int vertex = edge[0];
			int weight = edge[1];
			if (weight < priority[vertex])
				priority[vertex] = weight;
		}
		int bestPriority = int.max;
		foreach (j, p; priority) {
			if (!used[j] && p < bestPriority) {
				lastChoosed = j;
				bestPriority = p;
			}
		}
		sum += bestPriority;
	}
	writeln(sum);
}
