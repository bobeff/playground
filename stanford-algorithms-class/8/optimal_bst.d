import std.stdio;

immutable N = 7;

void printArray(ref float[N][N] a) {
	foreach (ref row; a) writeln(row);
}

void main() {
	float[N] f = [0.05, 0.4, 0.08, 0.04, 0.1, 0.1, 0.23];
	float[N][N] t;
	foreach (s; 0 .. N) {
		foreach (i; 0 .. N) {
			if (i + s >= N) break;
			float best = float.infinity;
			float sum = 0;
			foreach (r; i .. i + s + 1) sum += f[r];
			foreach (r; i .. i + s + 1) {
				float left = i > r - 1 ? 0 : t[i][r - 1];
				float right = r + 1 > i + s ? 0 : t[r + 1][i + s];
				float current = sum + left + right;
				if (current < best) best = current;
			}
			t[i][i + s] = best;
		}
		printArray(t);
		writeln();
	}
	writeln(t[0][N - 1]);
}
