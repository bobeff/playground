import std.stdio;

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

const uint[] primes = generatePrimes(10_000);

uint distinctPrimeFactors(uint x)
{
  uint primeFactorsCount;
  uint[primes.length] usedPrimes;
  
  foreach (i, p; primes) {
    if (x % p == 0) {
      if (usedPrimes[i] == 0)
        ++primeFactorsCount;
      ++usedPrimes[i];
    }
  }

  return primeFactorsCount;
}

void main()
{
  uint consCount, x = 467;
  
  while (true)
  {
    if (distinctPrimeFactors(x) >= 4)
      ++consCount;
    else
      consCount = 0;
    if (consCount == 4)
      break;
    ++x;
  }

  writeln(x - 3);
}
