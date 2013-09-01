import std.stdio;
import std.algorithm;
import std.ctype;

string GetAsWord(uint x)
{
	string[uint] numbers =
	[
		1	:	"one"		,
		2	:	"two"		,
		3	:	"three"		,
		4	:	"four"		,
		5	:	"five"		,
		6	:	"six"		,
		7	:	"seven"		,
		8	:	"eight"		,
		9	:	"nine"		,
		10	:	"ten"		,
		11	:	"eleven"	,
		12	:	"twelve"	,
		13	:	"thirteen"	,
		14	:	"fourteen"	,
		15	:	"fifteen"	,
		16	:	"sixteen"	,
		17	:	"seventeen"	,
		18	:	"eighteen"	,
		19	:	"nineteen"	,
		20	:	"twenty"	,
		30	:	"thirty"	,
		40	:	"forty"		,
		50	:	"fifty"		,
		60	:	"sixty"		,
		70	:	"seventy"	,
		80	:	"eighty"	,
		90	:	"ninety"	,
	];

	if (x in numbers)
	{
		return numbers[x];
	}

	if (x > 20 && x < 100)
	{
		return numbers[x / 10 * 10] ~ "-" ~ numbers[x % 10];
	}

	if (x >= 100 && x < 1000)
	{
		string result = numbers[x / 100] ~ " hundred";
		
		if (x % 100 != 0)
		{
			result ~= " and " ~ GetAsWord(x % 100);
		}

		return result;
	}

	if (x == 1000)
	{
		return "one thousand";
	}

	assert(0);

	return "";
}

uint GetCount(string number)
{
	return reduce!((count, character) { return isalpha(character) ? count + 1 : count; })(0, number);
}

uint GetCharactersCount()
{
	uint charactersCount = 0;
	
	foreach(i; 1 .. 1001)
	{
		charactersCount += GetCount(GetAsWord(i));
	}

	return charactersCount;
}

void main()
{
	writeln(GetCharactersCount());
}

