import std.stdio;
import std.math;

bool isPrime(T)(T n) {
  for (auto i = 2; i * i <= n; ++i)
    if (n % i == 0)
      return false;
  return true;
}

void main()
{
  uint[] primes = [3, 5, 7];
  uint x = 9;

outer:
  while (true)
  {
    x += 2;

    if (isPrime(x)) {
      primes ~= x;
      continue;
    }

    for (int i = primes.length - 1; i >= 0; --i) {
      float squareRoot = sqrt(cast(float)(x - primes[i]) / 2);
      if (squareRoot == floor(squareRoot))
        continue outer;
    }

    break;
  }

  writeln(x);
}
