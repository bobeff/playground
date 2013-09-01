import std.stdio;
import std.conv;

bool isPalindrome(ulong n)
{
  ulong d = 1;
  ulong nn = n;
  
  while (nn / 10 != 0)
  {
    d *= 10;
    nn /= 10;
  }
  
  while (d > 1 && n / d == n % 10)
  {
    n /= 10;
    d /= 10;
    n %= d;
    d /= 10;
  }

  return d <= 1;
}

unittest
{
  assert(isPalindrome(0));
  assert(isPalindrome(1));
  assert(isPalindrome(22));
  assert(isPalindrome(202));
  assert(!isPalindrome(21));
  assert(isPalindrome(212));
  assert(!isPalindrome(2121));
  assert(isPalindrome(2112));
  assert(isPalindrome(21512));
}

ulong reverse(ulong n)
{
  ulong res;
  
  while (n != 0)
  {
    res *= 10;
    res += n % 10;
    n /= 10;
  }

  return res;
}

unittest
{
  assert(reverse(0) == 0);
  assert(reverse(5) == 5);
  assert(reverse(12) == 21);
  assert(reverse(321) == 123);
}

bool isLychrel(ulong n)
{
  foreach (i; 0 .. 50)
  {
    ulong nn = n + reverse(n);
    if (isPalindrome(nn))
      return false;
    n = nn;
  }
  return true;
}

unittest
{
  assert(!isLychrel(47));
  assert(!isLychrel(349));
  assert(isLychrel(196));
  assert(isLychrel(4994));
}

void main()
{
  uint num;
  foreach (i; 0 .. 10_000)
  {
    num += to!uint(isLychrel(i));
  }
  writeln(num);
}
