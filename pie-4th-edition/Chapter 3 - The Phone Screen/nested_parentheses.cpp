// ROBLEM: You are given a string that contains left and right parenthesis
// char-acters. Write code to determine whether the parentheses are correctly
// nested. For example, the strings "(())" and "()()" are correctly nested but
// "(()()" and ")(" are not.

#include <exception>
#include <string>

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

using namespace std;

bool checkParenthesesNesting(const string& s)
{
  int counter = 0;
  for (int i = 0; i < s.length(); ++i)
  {
    if (s[i] == '(')
      ++counter;
    else if (s[i] == ')')
      --counter;
    else
      throw runtime_error(
        string("Invalid character \"") + s[i] + string("\"."));

    if (counter < 0)
      return false;
  }

  return counter == 0;
}

TEST_CASE("correctly nested parentheses")
{
  CHECK(checkParenthesesNesting("(())"));
  CHECK(checkParenthesesNesting("()()"));
  CHECK(!checkParenthesesNesting("(()()"));
  CHECK(!checkParenthesesNesting(")("));
  CHECK(checkParenthesesNesting("(()((()()())))"));
  CHECK(!checkParenthesesNesting("(()())()))"));
  CHECK_THROWS_WITH_AS(checkParenthesesNesting("(aa)"),
    "Invalid character \"a\".", runtime_error);
}
