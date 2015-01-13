#include <iostream>
#include <unordered_set>
#include <vector>
#include <map>
#include <functional>
#include <type_traits>
#include <cassert>
#include <cmath>

using namespace std;

const size_t SEARCHED_PRIMES_COUNT = 8;

auto startNumber = 100000u;
uint8_t numberLength;
unordered_set<unsigned> primes;

template <typename T>
bool isPrime(T n)
{
  static_assert(is_integral<T>::value, "Only integral types are supported.");
  assert(n >= 0 && "Expected positive number.");

  if (n == 0 || n == 1) return false;
  if (n == 2) return true;
  for (auto i = T(2); i * i <= n; ++i)
    if (n % i == 0)
      return false;
  return true;
}

template <typename T>
uint8_t length(T n)
{
  static_assert(is_integral<T>::value, "Only integral types are supported.");

  if (0 == n) return 1;

  uint8_t digitsCount = 0;
  while (n != 0)
  {
    n /= 10;
    ++digitsCount;
  }
  return digitsCount;
}

/*
 * Example:
 * Positions: 987654321
 * Number:    283042342
 */
template <typename T>
T replaceDigit(T n, uint8_t digitPos, uint8_t digit)
{
  static_assert(is_integral<T>::value, "Only integral types are supported.");
  assert(digit < 10 && "Expected number between 0 and 9.");

  T div = pow(10, digitPos);
  T mod = div / 10;
  return (n / div * 10 + digit) * mod + n % mod;
}

typedef vector<uint8_t> Combination;

template <typename Func>
void combinations(uint8_t n, uint8_t k, Func f)
{
  assert(n > 0);
  assert(k <= n);

  vector<bool> used(n, false);
  Combination result;
  result.reserve(k);

  const function<void(uint8_t)> comb = [&](uint8_t i)
  {
    if (result.size() == k)
    {
      f(result);
    }

    for (auto j = i; j < n; ++j)
    {
      if (!used[j])
      {
        used[j] = true;
        result.emplace_back(j);
        comb(j + 1);
        result.pop_back();
        used[j] = false;
      }
    }
  };

  comb(0);
}

vector<Combination> combinations(uint8_t n, uint8_t k)
{
  vector<Combination> result;
  auto store = [&result](Combination& c)
  {
    result.emplace_back(c);
  };
  combinations(n, k, store);
  return result;
}

const vector<Combination>& memoizedCombinations(uint8_t n, uint8_t k)
{
  typedef pair<uint8_t, uint8_t> KeyType;
  typedef vector<Combination> ValueType;

  static map<KeyType, ValueType> cache;

  auto key = make_pair(n, k);
  auto it = cache.find(key);

  if (it == cache.end())
  {
    it = cache.emplace(key, combinations(n, k)).first;
  }

  return it->second;
}

void reportResult()
{
  for (auto prime : primes)
  {
    cout << prime << " ";
  }

  cout << endl;

  exit(0);
}

int main()
{
  do
  {
    numberLength = length(startNumber);

    for (uint8_t i = 1; i <= numberLength; ++i)
    {
      const auto& combs = memoizedCombinations(numberLength, i);

      for (const auto& comb : combs)
      {
        for (uint8_t digit = 0; digit < 10; ++digit)
        {
          auto newNumber = startNumber;

          for (int j = comb.size() - 1; j >= 0; --j)
          {
            if (comb[j] + 1 == numberLength && digit == 0)
            {
              goto continue_outer_loop;
            }

            newNumber = replaceDigit(newNumber, comb[j] + 1, digit);
          }

          if(isPrime(newNumber))
          {
            primes.insert(newNumber);
          }
continue_outer_loop: ;
        }

        if (primes.size() >= SEARCHED_PRIMES_COUNT)
        {
          reportResult();
        }
        else
        {
          primes.clear();
        }
      }
    }

    ++startNumber;
  }
  while (true);

  return 0;
}
