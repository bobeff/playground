import std.stdio;
import std.c.stdio;
import std.array;
import std.algorithm;

class DisjointSets
{
public:
	this(uint size) {
		m_Size = m_SetsCount = size;
		m_Parent = uninitializedArray!(uint[])(size + 1);
		m_Rank = uninitializedArray!(uint[])(size + 1);
		foreach (i; 1 .. size + 1) {
			m_Parent[i] = i;
			m_Rank[i] = 0;
		}
	}

    uint find(uint x) {
		uint root = m_Parent[x];
		while (root != m_Parent[root])
			root = m_Parent[root];
		// path compression
		while (x != m_Parent[x]) {
			uint temp = m_Parent[x];
			m_Parent[x] = root;
			x = temp;
		}
		return root;
	}

	bool Union(uint x, uint y) {
		int xroot = find(x);
		int yroot = find(y);
		if (xroot == yroot) return false;
		// union by rank
		if (m_Rank[xroot] > m_Rank[yroot])
			m_Parent[yroot] = xroot;
		else
			m_Parent[xroot] = yroot;
		if (m_Rank[xroot] == m_Rank[yroot])
			m_Rank[yroot]++;
		--m_SetsCount;
		return true;
	}

	@property uint setsCount() const {
		return m_SetsCount;
	}

private:
	uint m_Size;
	uint m_SetsCount;
	uint[] m_Parent;
	uint[] m_Rank;
};

struct Edge {
	// node1, node2, weight
	ushort n1, n2, w;
}

alias Edge[] Graph;

void clustering1() {
	auto finp = File("clustering1.txt");
	uint n; // number of nodes
	fscanf(finp.getFP(), "%d", &n);
	uint m = n * (n - 1) / 2; // number of edges
	Graph g = uninitializedArray!(Graph)(m);
	foreach (i; 0 .. m)
		fscanf(finp.getFP(), "%d%d%d", &g[i].n1, &g[i].n2, &g[i].w);
	auto sets = new DisjointSets(n);
	sort!((x, y) { return x.w < y.w; })(g);
	uint edgeIndex;
	foreach (i, ref e; g) {
		sets.Union(e.n1, e.n2);
		if (sets.setsCount == 3) {
			edgeIndex = i;
			break;
		}
	}
	if (sets.setsCount == 3)
		writeln(g[edgeIndex].w);
	else
		writeln("There is no 4 clusters.");
}

ubyte hammingDistance(ubyte[] x, ubyte[] y) {
	ubyte ret = 0;
	foreach (i; 0 .. x.length)
		if (x[i] ^ y[i]) ++ret;
	return ret;
}

void clustering2() {
	auto finp = File("clustering2.txt");
	uint n, k; // number of nodes and number of bits per node
	fscanf(finp.getFP(), "%d%d", &n, &k);
	ubyte[][] points = uninitializedArray!(ubyte[][])(n, k);
	foreach (i; 0 .. n)
		foreach (j; 0 ..k)
			fscanf(finp.getFP(), "%d", &points[i][j]);
	uint m = n * (n - 1) / 2;
	auto g = uninitializedArray!(Graph)(m);
	uint nextEdge = 0;
	foreach (i; 0 .. n - 1)
		foreach (j; i + 1 .. n) {
			g[nextEdge].n1 = cast(ushort)(i + 1);
			g[nextEdge].n2 = cast(ushort)(j + 1);
			g[nextEdge++].w = hammingDistance(points[i], points[j]);
		}
	sort!((x, y) { return x.w < y.w; })(g);
	auto sets = new DisjointSets(n);
	foreach (ref e; g) {
		if (e.w >= 3) break;
		sets.Union(e.n1, e.n2);
	}
	writeln(sets.setsCount());
}

void main() {
	clustering1();
	clustering2();
}
