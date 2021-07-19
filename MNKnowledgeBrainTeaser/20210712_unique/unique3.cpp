#include <iostream>

using namespace std;

int main() {
  ios_base::sync_with_stdio(false);
  cin.tie(nullptr);
  int x, oneRepeat = 0, twoRepeats = 0;
  while (cin >> x) {
    twoRepeats |= oneRepeat & x;
    oneRepeat ^= x;
    int threeRepeatsReversed = ~(oneRepeat & twoRepeats);
    oneRepeat &= threeRepeatsReversed;
    twoRepeats &= threeRepeatsReversed;
  }
  cout << oneRepeat << '\n';
  return 0;
}
