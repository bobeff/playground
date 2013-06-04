import std.stdio;

uint[] generatePrimes(uint upperBound)
{
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

uint[] primes = generatePrimes(10_000);

uint digitsCount(uint n)
{
  uint res;
  while (n != 0) { n /= 10; ++res; }
  return res;
}

bool areSameDigits(uint x, uint y)
{
  ubyte[10] dx, dy;
  while (x != 0) { ++dx[x % 10]; x /= 10; }
  while (y != 0) { ++dy[y % 10]; y /= 10; }
  foreach (i; 0 .. 10)
    if (dx[i] != dy[i])
      return false;
  return true;
}

void main()
{
  int i = 0;
  while (digitsCount(primes[i]) < 4) ++i;
  foreach (j; i .. primes.length - 2)
    foreach (k; j + 1 .. primes.length - 1)
      if (areSameDigits(primes[j], primes[k])) {
        uint diff = primes[k] - primes[j];
        foreach (l; k + 1 .. primes.length)
          if (areSameDigits(primes[k], primes[l]) && diff == primes[l] - primes[k]) {
            writeln(primes[j], primes[k], primes[l]);
          }
      }
}
