#include <iostream>
#include <cstdint>

using namespace std;

const int MAX = 128;
uint32_t t[MAX][MAX];

int main() {
	t[0][0] = 1;
	for (int n = 1; n <= 100; ++n) {
		t[n][0] = 1;
		for (int r = 1; r <= n; ++r) {
			t[n][r] = t[n - 1][r - 1] + t[n - 1][r]; 
		}
	}
	int count = 0;
	for (int n = 1; n <= 100; ++n) {
		for (int r = 1; r <= n; ++r) {
			if (t[n][r] > 1000000) {
				++count;
			}
		}
	}
	cout << count << endl;
	return 0;
}
