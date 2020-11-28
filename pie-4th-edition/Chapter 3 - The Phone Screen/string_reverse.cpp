// PROBLEM: Write a function that reverses a string without using any library
// functions.

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

using namespace std;

string reverse(const string& s)
{
  string result;
  result.resize(s.length());
  for (int i = 0, j = s.length() - 1; i <= j; ++i, --j)
  {
    result[i] = s[j];
    result[j] = s[i];
  }
  return result;
}

TEST_CASE("reverse")
{
  CHECK(reverse("") == "");
  CHECK(reverse("a") == "a");
  CHECK(reverse("ab") == "ba");
  CHECK(reverse("abc") == "cba");
  CHECK(reverse("Hello, World!") == "!dlroW ,olleH");
}

void reverseInPlace(string& s)
{
  for (int i = 0, j = s.length() - 1; i <= j; ++i, --j)
  {
    char temp = s[i];
    s[i] = s[j];
    s[j] = temp;
  }
}

TEST_CASE("reverseInPlace")
{
  string s1 = "";
  string s2 = "a";
  string s3 = "ab";
  string s4 = "abc";
  string s5 = "Hello, World!";

  reverseInPlace(s1);
  reverseInPlace(s2);
  reverseInPlace(s3);
  reverseInPlace(s4);
  reverseInPlace(s5);

  CHECK(s1 == "");
  CHECK(s2 == "a");
  CHECK(s3 == "ba");
  CHECK(s4 == "cba");
  CHECK(s5 == "!dlroW ,olleH");
}
