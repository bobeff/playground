import std.stdio;
import std.conv;
import std.string;
import std.algorithm;

/// Version of quick sort which returns number of comparisons used.
uint qsort(uint[] a, uint function(uint[]) choosePivot) {
	uint partition() {
		uint i = 1;
		foreach (j; 1 .. a.length)
			if (a[j] < a[0])
				swap(a[j], a[i++]);
		swap(a[0], a[i - 1]);
		return i - 1;
	}
	if (a.length <= 1) return 0;
	swap(a[0], a[choosePivot(a)]);
	uint i = partition();
	return a.length - 1 +
		qsort(a[0 .. i], choosePivot) +
		qsort(a[i + 1 .. $], choosePivot);
}

void main() {
	uint[] numbers;
	File finp = File("array0.txt");
	foreach (line; finp.byLine())
		numbers ~= to!uint(chop(line));
	writeln(qsort(numbers.dup, function uint(uint[] a) { return 0; }));
	writeln(qsort(numbers.dup, function uint(uint[] a) { return a.length - 1; }));
	writeln(qsort(numbers.dup, function uint(uint[] a) {
				uint m = a.length % 2 == 0 ? a.length / 2 - 1 : a.length / 2;
				if (a[0] < a[m] && a[m] < a[$ - 1]) return 0;
				if (a[m] < a[0] && a[0] < a[$ - 1]) return m;
				return a.length - 1;
			}));
}
