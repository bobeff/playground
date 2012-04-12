import std.stdio;
import std.conv;
import std.string;

immutable MAX_NUMBERS_COUNT = 100_000;
immutable MAX_VALUE = 1_000_000;

void main() {
	uint next = 0;
	auto values = new uint[MAX_NUMBERS_COUNT];
	auto present = new byte[MAX_VALUE];
	uint[] sums = [231552,234756,596873,648219,726312,981237,988331,1277361,1283379];
	auto f = File("HashInt.txt");
	foreach (line; f.byLine()) {
		auto value = to!uint(chomp(line));
		values[next++] = value;
		++present[value];
	}
outer:
	foreach (sum; sums) {
		foreach (value; values) {
			--present[value];
			auto diff = sum - value;
			if (diff >= 0 && diff < MAX_VALUE && present[diff] > 0) {
				write(1);
				++present[value];
				continue outer;
			}
			++present[value];
		}
		write(0);
	}
}
