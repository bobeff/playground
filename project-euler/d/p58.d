import std.stdio;

bool isPrime(T)(T n) {
  for (auto i = 2; i * i <= n; ++i)
    if (n % i == 0)
      return false;
  return true;
}

void main()
{
  uint n = 1;
  uint step = 2;
  float primesCount = 0;
  float ratio = 1;
  while (ratio >= 0.1) {
    foreach (i; 0 .. 4) {
      n += step;
      if (isPrime(n))
        ++primesCount;
    }
    ratio = primesCount / (2 * (step + 1) - 1);
    step += 2;
  }
  writeln(step - 1);
}
