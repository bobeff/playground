#include <iostream>
#include <algorithm>

using namespace std;

const int MAX = 32;
char s[5][MAX];

int intToStr(int n, char* s) {
	int len = 0;
	do {
		s[len++] = n % 10 + '0';
		n /= 10;
	} while (n);
	reverse(s, s + len);
	s[len] = '\0';
	return len;
}

int main() {
	for (int i = 1; ; ++i) {
		for (int j = 0; j < 5; ++j) {
			int len = intToStr(i * (j + 2), s[j]);
			sort(s[j], s[j] + len);
		}
		if (!strcmp(s[0], s[1]) &&
			!strcmp(s[1], s[2]) &&
			!strcmp(s[2], s[3]) &&
			!strcmp(s[3], s[4]))
		{
			cout << i << endl;
			break;
		}
	}
	return 0;
}
