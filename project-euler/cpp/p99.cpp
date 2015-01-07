#include <iostream>
#include <fstream>
#include <cmath>

using namespace std;

int main()
{
    ifstream f("p099_base_exp.txt");

    float maxValue = 0;
    int maxValueLine = 0;
    int lineNumber = 0;

    string line;
    while (f >> line)
    {
        ++lineNumber;

        auto index = line.find_first_of(",");
        auto base = stoi(line.substr(0, index));
        auto exp = stoi(line.substr(index + 1));
        auto value = exp * log(base);

        if (value > maxValue)
        {
            maxValue = value;
            maxValueLine = lineNumber;
        }
    }

    cout << maxValueLine << endl;

    return 0;
}
