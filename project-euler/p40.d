import std.stdio;
import std.conv;

void main() {
	string s;
	for (uint i = 0; s.length <= 100_000; ++i)
		s ~= to!string(i);
	uint prod = 1;
	for (uint d = 1; d <= 100_000; d *= 10)
		prod *= s[d] - '0';
	writeln(prod);
}
