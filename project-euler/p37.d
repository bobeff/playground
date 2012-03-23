import std.stdio;
import std.math;
import std.conv;
import primes;

immutable UPPER_BOUND = 1000000;
immutable prime = generatePrimesSieve(UPPER_BOUND);

bool checkRightTruncations(uint n) {
	n /= 10;
	while (n > 0) {
		if (!prime[n])
			return false;
		n /= 10;
	}
	return true;
}

bool checkLeftTruncations(uint n) {
	uint mod = pow(10, to!string(n).length - 1);
	n %= mod;
	while (n > 0) {
		if (!prime[n])
			return false;
		mod /= 10; 
		n %= mod;
	}
	return true;
}

unittest {
	assert(checkRightTruncations(3797));
	assert(checkLeftTruncations(3797));
}

void main() {
	uint count = 0;
	uint sum = 0;
	foreach (n; 11 .. UPPER_BOUND)
		if (prime[n] && checkRightTruncations(n) && checkLeftTruncations(n)) {
			writeln("Number ", count + 1, ":", n);
			sum += n;
			if (++count == 11) break;
		}
	assert(count == 11);
	writeln("Sum = ", sum);
}
