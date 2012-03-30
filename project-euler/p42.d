import std.stdio;
import std.string;

uint getValue(string word) {
	uint result = 0;
	foreach (c; word)
		result += c - 'A' + 1;
	return result;
}

bool[] generateTriangulerNumbers(uint upperBound) {
	bool[] result = new bool[upperBound];
	uint n = 1, number = 1;
	while (upperBound > number) {
		result[number] = true;
		number = ++n * (n + 1) / 2;
	}
	return result;
}

bool isTriangular(uint number) {
	static bool[] triangularNumbers = generateTriangulerNumbers(200);
	return triangularNumbers[number];
}

void main() {
	auto finp = File("p42.txt");
	string[] words;
	foreach (s; finp.byLine())
		foreach (word; split(s, ","))
			words ~= cast(string)(word[1 .. $ - 1]);
	uint triangularWords = 0;
	foreach (word; words)
		if(isTriangular(getValue(word))) {
			writeln(word);
			++triangularWords;
		}
	writeln(triangularWords);
}
