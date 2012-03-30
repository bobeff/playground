import std.stdio;
import std.conv;
import std.string;

// fast version O(n * log(n))
uint countInversions(uint[] numbers)
	out(count) {
		ulong n = numbers.length;
		assert(count <= n * (n + 1) / 2);
	}
body {
	uint countSplitInversions(uint[] left, uint[] right) {
		uint invCount = 0;
		uint i = 0, j = 0, k = 0;
		while (i < left.length && j < right.length) {
			if (left[i] <= right[j])
				numbers[k++] = left[i++];
			else {
				numbers[k++] = right[j++];
				invCount += left.length - i;
			}
		}
		while (i < left.length) numbers[k++] = left[i++];
		while (j < right.length) numbers[k++] = right[j++];
		return invCount;
	}

	if (numbers.length == 1) return 0;
	return countInversions(numbers[0 .. $ / 2]) +
	       countInversions(numbers[$ / 2 .. $]) +
	       countSplitInversions(numbers[0 .. $ / 2].dup,
								numbers[$ / 2 .. $].dup);
}

// slow version O(n * n)
uint countInversionsSlow(uint[] numbers) {
	uint count = 0;
	foreach (i; 0 .. numbers.length)
		foreach (j; i + 1 .. numbers.length)
			if (numbers[i] > numbers[j])
				count++;
	return count;
}

void main() {
	uint[] numbers;
	File finp = File("hw1.txt");
	foreach (line; finp.byLine())
		numbers ~= to!uint(chomp(line));
	writeln(countInversions(numbers));
}
