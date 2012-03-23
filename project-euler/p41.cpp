#include <iostream>
#include <algorithm>

using namespace std;

bool isPrime(int n) {
	for (int i = 2; i * i <= n; ++i)
        if (n % i == 0)
            return false;
    return true;
}

int main() {
	char number[10];
	for (int i = 9; i > 0; --i) {
		for (int j = i; j > 0; --j)
			number[i - j] = j + '0';
		number[i] = '\0';
		do {
			if (isPrime(atoi(number))) goto out;
		} while (prev_permutation(number, number + i));
	}
out:
	cout << number << endl;
}
