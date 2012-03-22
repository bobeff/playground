import std.stdio;
import std.conv;
import primes;

uint circularPrimesCount(uint upperBound) {

    bool isCircular(uint prime)
    in {
        assert(isPrime(prime));
    }
    body {
        string sPrime = to!string(prime);
        foreach (i; 0 .. sPrime.length - 1) {
            string sNewNumber = sPrime[1 .. $] ~ sPrime[0 .. 1];
            uint newNumber = to!uint(sNewNumber);
            if (!isPrime(newNumber))
                return false;
            sPrime = sNewNumber;
        }
        return true;
    }

    uint[] primeNumbers = generatePrimes(upperBound);
    uint count = 0;
    foreach (i; 0 .. primeNumbers.length)
        if (isCircular(primeNumbers[i]))
            ++count;

    return count;
}

unittest {
    assert(circularPrimesCount(100) == 13);
}

void main() {
    writeln(circularPrimesCount(1000000));
}
