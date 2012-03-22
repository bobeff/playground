import std.stdio;
import std.conv;

bool IsPalindrom(string s) {
	for (auto i = 0, j = s.length - 1; i < j; ++i, --j)
		if (s[i] != s[j])
			return false;
	return true;
}

string ToBinary(uint num) {
	auto mask = 1;
	while (mask <= num)
		mask <<= 1;
	mask >>= 1;
	string result = "";
	while (mask > 0) {
		if ((num & mask) > 0)
			result ~= "1";
		else
			result ~= "0";
		mask >>= 1;
	}
	return result;
}

void main() {
	auto sum = 0;
	foreach (i; 1 .. 1000000)
		if (IsPalindrom(to!string(i)) && IsPalindrom(ToBinary(i)))
			sum += i;
	writeln(sum);
}
