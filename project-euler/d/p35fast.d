import std.stdio;
import std.math;

immutable UPPER_BOUND = 1_000_000;

void main() {
    // sieve of Eratosthenes
    bool[UPPER_BOUND] primes;
    foreach (i; 2 .. UPPER_BOUND)
        if (!primes[i])
            for (auto j = 2 * i; j < UPPER_BOUND; j += i)
                primes[j] = true;

    auto count = 0;
    foreach (i; 2 .. UPPER_BOUND)
        if (!primes[i]) {
            auto prime = i;
            auto digitsCount = 0;
            while (prime > 0) {
                ++digitsCount;
                prime /= 10;
            }
            prime = i;
            bool isCircular = true;
            do {
                prime = prime % 10 * pow(10, digitsCount - 1) + (prime / 10);
                if (primes[prime]) {
                    isCircular = false;
                    break;
                }
            }
            while (prime != i);
            if (isCircular) {
                writeln(prime);
                ++count;
            }
        }
    writeln(count);
}
