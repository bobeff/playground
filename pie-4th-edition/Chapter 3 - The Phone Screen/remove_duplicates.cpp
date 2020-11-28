// PROBLEM: Given an unsorted list of integers, write a function that returns a
// new list with all duplicate values removed.

#include <list>
#include <unordered_set>

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

using namespace std;

list<int> removeDuplicates(const list<int>& l)
{
  list<int> result;
  unordered_set<int> alreadyFound;
  for (auto x: l)
  {
    auto it = alreadyFound.find(x);
    if (it != alreadyFound.end())
      continue;
    alreadyFound.insert(x);
    result.push_back(x);
  }
  return result;
}

TEST_CASE("remove duplicates")
{
  CHECK(removeDuplicates(list<int>{}) == list<int>{});
  CHECK(removeDuplicates(list<int>{0}) == list<int>{0});
  CHECK(removeDuplicates(list<int>{-1, -1}) == list<int>{-1});
  CHECK(removeDuplicates(list<int>{0, 0, 1, 0}) == list<int>{0, 1});
  CHECK(removeDuplicates(list<int>{4, 6, 3, 4, 3, -8, 6, 1, 1}) == 
        list<int>{4, 6, 3, -8, 1});
}
