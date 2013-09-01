#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

int main()
{
	string digits = "0123456789";

	for (int i = 0; i < 999999; ++i)
		next_permutation(digits.begin(), digits.end());

	cout << digits << endl;

	return 0;
}
