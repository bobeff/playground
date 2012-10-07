#include <iostream>
#include <cstdint>

using namespace std;

int main() {
	uint64_t x = 28433;
	for (int i = 0; i < 7830457; ++i) {
		x <<= 1;
		x %= 10000000000;
	}
	++x;
	cout << x << endl;
	return 0;
}
