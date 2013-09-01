import std.stdio;
import std.algorithm;

immutable MAX = 28123;

uint SumOfDivisors(uint n) {
	uint sum = 1;
	for (uint i = 2; i * i <= n; ++i)
		if (n % i == 0) {
			sum += i;
			if (i * i < n)
				sum += n / i;
		}
	return sum;
}

unittest {
	assert(SumOfDivisors(1) == 1);
	assert(SumOfDivisors(12) == 16);
	assert(SumOfDivisors(16) == 15);
	assert(SumOfDivisors(28) == 28);
}

uint[] FindAbundantNumbers(uint max) {
	uint[] abundants;
	foreach (i; 12 .. max + 1)
		if (SumOfDivisors(i) > i)
			abundants ~= i;
	return abundants;
}

unittest {
	assert(FindAbundantNumbers(100) ==
		[12, 18, 20, 24, 30, 36, 40, 42, 48, 54, 56, 60, 66, 70, 72, 78, 80, 84, 88, 90, 96, 100]);
}

auto FindNumbersWhichAreSumOfTwoAbundants(uint max) {
	bool[MAX + 1] result;
	uint[] abundants = FindAbundantNumbers(max);
	foreach (i; 0 .. abundants.length)
		foreach (j; i .. abundants.length) {
			uint sum = abundants[i] + abundants[j];
			if (sum <= max)
				result[sum] = true;
		}
	return result;
}

void main() {
	uint result;
	auto arr = FindNumbersWhichAreSumOfTwoAbundants(MAX);
	foreach (i; 0 .. arr.length)
		if (!arr[i])
			result += i;
	writeln(result);
}
