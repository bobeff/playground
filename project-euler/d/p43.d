import std.stdio;
import std.algorithm;

ulong getNum(uint[] a)
{
  ulong res = 0;
  foreach (i; 0 .. a.length)
    res = res * 10 + a[i];
  return res;
}

void main()
{
  ulong sum;
  ubyte[7] primes = [2, 3, 5, 7, 11, 13, 17];
  uint[] a = [1, 0, 2, 3, 4, 5, 6, 7, 8, 9];

  do
  {
    uint i, x;
    for (i = 1; i < 8; ++i)
    {
      x = a[i] * 100 + a[i + 1] * 10 + a[i + 2];
      if (x % primes[i - 1] != 0) break;
    }
    if (i == 8) {
      ulong num = getNum(a);
      writeln(num);
      sum += num;
    }
  }
  while (nextPermutation(a));

  writeln("Sum = ", sum);
}
