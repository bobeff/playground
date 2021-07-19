#include <iostream>

using namespace std;

int main() {
  ios_base::sync_with_stdio(false);
  cin.tie(nullptr);
  int x, result = 0;
  while (cin >> x) result ^= x;
  cout << result << '\n';
  return 0;
}
