// PROBLEM: Write a program that prints the numbers from 1 to 100. If a number
// is divisible by 3, print “Fizz” instead of the number, if a number is
// divisible by  5, print “Buzz” instead of the number, and if a number is
// divisible by both 3 and 5, print “FizzBuzz” instead of the number.

#include <iostream>
#include <string>

#define DOCTEST_CONFIG_IMPLEMENT
#include <doctest/doctest.h>

using namespace std;

std::string fizzBuzz(int x)
{
  bool divisibleBy3 = x % 3 == 0;
  bool divisibleBy5 = x % 5 == 0;

  if (divisibleBy3 && divisibleBy5) return "fizzbuzz";
  else if (divisibleBy3) return "fizz";
  else if (divisibleBy5) return "buzz";
  else return to_string(x);
}

TEST_CASE("fizzBuzz")
{
  CHECK(fizzBuzz(1) == "1");
  CHECK(fizzBuzz(3) == "fizz");
  CHECK(fizzBuzz(5) == "buzz");
  CHECK(fizzBuzz(7) == "7");
  CHECK(fizzBuzz(15) == "fizzbuzz");
}

int main()
{
  doctest::Context context;
  int res = context.run();
  if(context.shouldExit())
      return res;

  for (int i = 1; i <= 100; ++i)
    cout << fizzBuzz(i) << "\n";

  return 0;
}
