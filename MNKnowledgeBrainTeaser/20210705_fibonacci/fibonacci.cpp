#include <iostream>
#include <iomanip>
#include <cstring>

using namespace std;

constexpr uint16_t MAX_NUMBER_LENGTH = 10000;
constexpr uint32_t LAST_9_DIGITS_REPEAT_CYCLE = 1'500'000'000;
constexpr uint32_t LAST_9_DIGITS_DIVIDER = 1'000'000'000;

uint64_t mod(const string& n)
{
  uint64_t result = 0;
  for (size_t i = 0; i < n.length(); ++i)
  {
    uint8_t digit = n[i] - '0';
    result = result * 10 + digit;
    result %= LAST_9_DIGITS_REPEAT_CYCLE;
  }
  return result;
}

void multiply(uint64_t f[2][2], uint64_t m[2][2])
{
  uint64_t x = (f[0][0] * m[0][0] + f[0][1] * m[1][0]) % LAST_9_DIGITS_DIVIDER;
  uint64_t y = (f[0][0] * m[0][1] + f[0][1] * m[1][1]) % LAST_9_DIGITS_DIVIDER;
  uint64_t z = (f[1][0] * m[0][0] + f[1][1] * m[1][0]) % LAST_9_DIGITS_DIVIDER;
  uint64_t w = (f[1][0] * m[0][1] + f[1][1] * m[1][1]) % LAST_9_DIGITS_DIVIDER;

  f[0][0] = x;
  f[0][1] = y;
  f[1][0] = z;
  f[1][1] = w;
}

void power(uint64_t f[2][2], uint32_t n)
{
  uint64_t result[2][2] = { { 1, 1 }, { 1, 0 } };
  while (n > 0)
  {
    if (n % 2 == 1)
    {
      multiply(result, f);
    }
    multiply(f, f);
    n >>= 1;
  }
  memcpy(f, result, sizeof(result));
}

uint64_t fibonacci(uint32_t n)
{
  if (n == 0) return 0;
  if (n == 1) return 1;
  uint64_t f[2][2] = { {1, 1}, {1, 0} };
  power(f, n - 2);
  return f[0][0];
}

int main()
{
  ios_base::sync_with_stdio(false);
  cin.tie(nullptr);

  string n;
  n.reserve(MAX_NUMBER_LENGTH);
  cin >> n;

  cout << setw(9) << setfill('0') << fibonacci(mod(n)) << "\n";

  return 0;
}
