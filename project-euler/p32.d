import std.stdio;
import std.math;

immutable ubyte POWER = 5;

uint FindMaxNumber(uint n) {
	immutable p9n = pow(9, n);
	auto digitsCount = 2;
	auto maxNum = 99;
	while (digitsCount * p9n > maxNum) {
		++digitsCount;
		maxNum *= 10;
		maxNum += 9;
	}
	return digitsCount * p9n;
}

bool IsSumOfDigits(uint n, uint power) {
	uint sum = 0;
	uint num = n;
	while (num > 0) {
		auto digit = num % 10;
		sum += pow(digit, power);
		num /= 10;
	}
	return sum == n;
}

unittest {
	assert(IsSumOfDigits(1634, 4));
	assert(IsSumOfDigits(8208, 4));
	assert(!IsSumOfDigits(8218, 4));
	assert(IsSumOfDigits(9474, 4));
}

void main() {
	static immutable MAX_NUMBER = FindMaxNumber(POWER);
	auto result = 0;
	foreach (n; 10 .. MAX_NUMBER + 1) {
		if (IsSumOfDigits(n, POWER)) {
			result += n;
		}
	}
	writeln(result);
}
