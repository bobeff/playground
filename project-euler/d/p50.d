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

immutable UPPER_BOUND = 1_000_000;

void main()
{
  uint[] primes = generatePrimes(UPPER_BOUND);
  uint[UPPER_BOUND] sums;
  foreach (i; 0 .. primes.length) {
    uint sum = 0;
    uint sumCount = 0;
    foreach (j; i .. primes.length) {
      sum += primes[j];
      ++sumCount;
      if (sum >= UPPER_BOUND) break;
      if (sums[sum] < sumCount) sums[sum] = sumCount;
    }
  }

 uint bestPrime, longestSum;
 for (int i = primes.length - 1; i >= 0; --i)
   if (sums[primes[i]] > longestSum) {
     bestPrime = primes[i];
     longestSum = sums[bestPrime];
   }

 writeln(bestPrime, " ", longestSum);
}
