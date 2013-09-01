import std.stdio;
import std.bigint;
import std.algorithm;

uint digitsCount(BigInt n)
{
  uint res;
  do { ++res; n /= 10; } while (n != 0);
  return res;
}

void main()
{
  uint res;
  BigInt num = 1;
  BigInt denom = 1;
  foreach (i; 0 .. 1001) {
    num += denom;
    swap(num, denom);
    num += denom;
    if (digitsCount(num) > digitsCount(denom))
      ++res;
  }
  writeln(res);
}
