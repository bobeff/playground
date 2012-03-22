import std.stdio;

bool isPrime(T)(T n) {
    for (auto i = 2; i * i <= n; ++i)
        if (n % i == 0)
            return false;
    return true;
}

unittest {
    assert(isPrime(2));
    assert(isPrime(3));
    assert(!isPrime(4));
    assert(isPrime(13));
    assert(isPrime(101));
}

uint[] generatePrimes(uint upperBound)
in {
    assert(upperBound >= 2);
}
body {
    bool[] sieve = new bool[upperBound / 2 - 1];
    for (auto i = 3; i < upperBound; i += 2)
        if (!sieve[i / 2 - 1])
            for (auto j = i + 2 * i; j < upperBound; j += 2 * i)
                sieve[j / 2 - 1] = true;
    uint[] result = [ 2 ];
    foreach (i; 0 .. upperBound / 2 - 1)
        if (!sieve[i])
            result ~= i * 2 + 3;
    return result;
}

unittest {
    assert(generatePrimes(10) == [2, 3, 5, 7]);
    assert(generatePrimes(101) ==
           [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41,
            43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97 ]);
}
