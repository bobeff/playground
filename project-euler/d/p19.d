import std.stdio;

ubyte[12][2] daysPerMonth = [
	[31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],
	[31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31], // leap year
];

bool IsLeapYear(uint year) {
	return year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
}

uint GetSundays(uint finalYear) {
	uint sundays = 0;
	uint day = 1;
	foreach (year; 1900 .. finalYear)
		foreach (month; 0 .. 12) {
			day += daysPerMonth[cast(int)(IsLeapYear(year))][month];
			if (day % 7 == 6)
				++sundays;
		}
	return sundays;
}

void main() {
	writeln(GetSundays(2001) - GetSundays(1901));
}
