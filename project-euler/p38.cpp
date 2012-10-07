#include <iostream>
#include <algorithm>

using namespace std;

template <typename T> T strToNum(const char* s) {
	T n(0);
	for (const char* p = s; *p != '\0'; ++p) {
		n *= 10;
		n += *p - '0';
	}
	return n;
}

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

bool isPrefix(const char* p, const char* s) {
	while (*p++ == *s++ && *p && *s) ;
	return *p == *s;
}

bool isPandigital(const char* digits) {
	int number = strToNum<int>(digits);
	for (size_t i = 1; i < 8; ++i) {
		char prefix[16] = {0};
		strncpy(prefix, digits, i);
		int first = strToNum<int>(prefix);
		long long currentNumber = 0;
		for (int j = 2; currentNumber < number; ++j) {
			int next = first * j;
			char nexts[16];
			intToStr(next, nexts);
			strcat(prefix, nexts);
			currentNumber = strToNum<long long>(prefix);
			if (currentNumber == number) {
				cout << "prefix = " << first << endl;
				return true;
			} else if (!isPrefix(prefix, digits)) {
				break;
			}
		}
	}
	return false;
}

int main() {
	char digits[10] = "987654321";
	do {
		if (isPandigital(digits)) {
			cout << digits << endl;
			break;
		}
	} while (prev_permutation(digits, digits + 9));
}
