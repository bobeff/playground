import std.stdio;

bool[] generatePrimesSieve(uint upperBound)
{
  bool[] primes = new bool[upperBound];
  foreach (i; 2 .. upperBound)
    primes[i] = true;
  foreach (i; 2 .. upperBound)
    if (primes[i])
      for (auto j = 2 * i; j < upperBound; j += i)
        primes[j] = false;
  return primes;
}

const bool[] isPrime = generatePrimesSieve(10_000);

bool areSameDigits(uint x, uint y, uint z)
{
  ubyte[10] dx, dy, dz;
  while (x != 0) { ++dx[x % 10]; x /= 10; }
  while (y != 0) { ++dy[y % 10]; y /= 10; }
  while (z != 0) { ++dz[z % 10]; z /= 10; }
  foreach (i; 0 .. 10)
    if (dx[i] != dy[i] || dy[i] != dz[i])
      return false;
  return true;
}

void main()
{
  foreach (uint i; 1001 .. 10000 - 6660) {
    uint j = i + 3330;
    uint k = j + 3330;
    if (isPrime[i] && isPrime[j] && isPrime[k] && areSameDigits(i, j, k))
      writeln(i, j, k);
  }
}
