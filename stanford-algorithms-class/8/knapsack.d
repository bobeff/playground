import std.stdio;
import std.c.stdio;
import std.array;
import std.algorithm;

int knapsackSize, itemsCount;
int[] itemValues, itemWeights;
int[][] f; // target function
int[int] cache;

void readInput() {
	scanf("%d%d", &knapsackSize, &itemsCount);
	itemValues = uninitializedArray!(int[])(itemsCount);
	itemWeights = uninitializedArray!(int[])(itemsCount);
	foreach (i; 0 .. itemsCount)
		scanf("%d%d", &itemValues[i], &itemWeights[i]);
}

void solveKnapsack1() {
	f = uninitializedArray!(int[][])(itemsCount + 1, knapsackSize + 1);
	foreach (i; 0 .. knapsackSize + 1) f[0][i] = 0;
	foreach (i; 0 .. itemsCount + 1) f[i][0] = 0;
	foreach (s; 1 .. knapsackSize + 1)
		foreach (i; 1 .. itemsCount + 1) {
			if (s - itemWeights[i - 1] < 0) {
				f[i][s] = f[i - 1][s];
				continue;
			}
			f[i][s] = max(f[i - 1][s],
				f[i - 1][s - itemWeights[i - 1]] + itemValues[i - 1]);
		}
	writeln(f[itemsCount][knapsackSize]);
}

int code(int n, int m) {
	return n * knapsackSize + m;
}

int solveKnapsack2(int n, int m) {
	if (n == 0 || m == 0) return 0;
	int c = code(n, m);
	if (c in cache) return cache[c];
	if (m - itemWeights[n - 1] < 0)
		return cache[c] = solveKnapsack2(n - 1, m);
	return cache[c] = max(
		solveKnapsack2(n - 1, m),
		solveKnapsack2(n - 1, m - itemWeights[n - 1]) + itemValues[n - 1]);
}

void main() {
	readInput();
	//solveKnapsack1();
	writeln(solveKnapsack2(itemsCount, knapsackSize));
}
