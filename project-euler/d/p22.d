import std.stdio;
import std.string;
import std.algorithm;

uint GetValue(char[] word) {
	return reduce!((s, c) { return s + (c - 'A' + 1); } )(0, word);
}

void main() {
	auto fNames = File("names.txt", "r");
	char[] buffer;
	fNames.readln(buffer, 46447);
	auto words = split(buffer, ",");
	sort(words);
	uint sum;
	foreach (i, word; words)
		sum += (i + 1) * GetValue(word[1 .. $ - 1]);
	writeln(sum);
}
