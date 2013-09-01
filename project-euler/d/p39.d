import std.stdio;
import std.math;

immutable MAX = 1000;

struct Solution
{
	uint count;
	uint[3][] triples;
}

Solution[MAX + 1] solutions;

void main() {
	foreach (a; 1 .. MAX / 3)
		foreach (b; a .. MAX / 2) {
			auto cr = sqrt(a * a + b * b);
			uint c = cast(uint)(cr);
			if (c == cr && a + b + c <= MAX) {
				uint p = a + b + c;
				++solutions[p].count;
				solutions[p].triples ~= [a, b, c];
			}
		}
	uint p = 0, max = 0;
	foreach (i, s; solutions)
		if (max < s.count) {
			max = s.count;
			p = i;
		}
	writeln("Max solutions ", max, " for perimeter ", p);
	foreach (triple; solutions[p].triples) {
		writeln(triple);
	}
}
