import std.stdio;

uint SumOfFactors(uint n)
{
	uint sum = 1;
	for (uint i = 2; i * i <= n; ++i)
		if (n % i == 0)
		{
			sum += i;
			if (i != n / i) sum += n / i;
		}
	return sum;
}

void main()
{	
	uint sum;
	foreach (a; 1 .. 10000)
	{
		uint b = SumOfFactors(a);
		if (a == b) continue;
		uint c = SumOfFactors(b);
		if (a == c) sum += a;
	}
	writeln(sum);
}
