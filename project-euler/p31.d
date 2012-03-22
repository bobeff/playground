import std.stdio;

immutable SUM = 200;

ubyte[8] coins = [1, 2, 5, 10, 20, 50, 100, 200];
int[coins.length][SUM + 1] ways;

int Solve(int sum, int pos) {
	if (pos == coins.length) {
		return 0;
	}
	if (sum < 0) {
		return 0;
	}
	if (ways[sum][pos] != 0) {
		return ways[sum][pos];
	}
	if (sum == 0) {
		return ways[sum][pos] = 1;
	}
	return ways[sum][pos] = Solve(sum - coins[pos], pos) + Solve(sum, pos + 1);
}

void main() {
	writeln(Solve(SUM, 0));
}
