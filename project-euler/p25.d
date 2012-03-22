import std.stdio;
import std.bigint;

void main()
{
	BigInt f0 = "1";
	BigInt f1 = "1";

	uint counter = 0;

	while (true)
	{
		bool halt = false;
		++counter;

		f0.toString((const(char)[] s) {
				if (s.length == 1000) {
					writeln(counter);
					halt = true;
				}
			}, "d");

		if (halt) break;

		BigInt temp = f1;
		f1 = f0 + f1;
		f0 = temp;
	}
}
