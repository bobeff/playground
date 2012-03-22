import std.stdio;

bool[] EratosthenesSieve(uint maxNumber) {
	assert(maxNumber >= 2);
	bool[] sieve = new bool[maxNumber];
	foreach (ref x; sieve) {
		x = true;
	}
	sieve[0] = false;
	sieve[1] = false;
	foreach (i; 2 .. maxNumber) {
		if (sieve[i]) {
			for (auto j = 2 * i; j < maxNumber; j += i) {
				sieve[j] = false;
			}
		}
	}
	return sieve;
}

unittest {
	static assert(
		EratosthenesSieve(10) ==
		[false, false, true, true, false, true, false, true, false, false]
	);
}

int Quadratic(uint n, int a, int b) {
	return n * n + a * n + b;
}

bool IsPrime(uint n) {
	static immutable MAX = 111000;
	static bool[] primes = EratosthenesSieve(MAX);
	assert(n < MAX);
	return primes[n];
}

unittest {
	assert(IsPrime(2));
	assert(IsPrime(3));
	assert(!IsPrime(4));
	assert(IsPrime(5));
	assert(!IsPrime(6));
	assert(IsPrime(13));
	assert(!IsPrime(100));

	foreach (n; 0 .. 40) {
		assert(IsPrime(Quadratic(n, 1, 41)));
	}

	assert(!IsPrime(Quadratic(40, 1, 41)));

	foreach (n; 0 .. 80) {
		assert(IsPrime(Quadratic(n, -79, 1601)));
	}

	assert(!IsPrime(Quadratic(80, -79, 1601)));
}

void main() {
	auto bestCount = 0;
	int product;
	foreach (int a; -999 .. 1000) {
		foreach (int b; -999 .. 1000) {
			auto n = 0;
			auto count = 0;
			auto number = Quadratic(n, a, b);
			while (number > 2 && IsPrime(number)) {
				++count;
				++n;
				number = Quadratic(n, a, b);
			}
			if (count > bestCount) {
				bestCount = count;
				product = a * b;
			}
		}
	}
	writeln(product);
}
