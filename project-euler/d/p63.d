import std.stdio;
import std.bigint;

ubyte digitsCount(BigInt n)
{
  ubyte res;
  do { ++res; n /= 10; } while (n != 0);
  return res;
}

void main()
{
  uint dc, count, n = 1;
  bool flag = true;
  while (flag) {
    flag = false;
    BigInt x = 1;
    do {
      BigInt p = x ^^ n;
      dc = digitsCount(p);
      if (dc == n) {
        ++count;
        flag = true;
        writeln(p, " = ", x, " ^^ ", n);
      }
      ++x;
    } while (dc <= n);
    ++n;
  }
  writeln(count);
}
