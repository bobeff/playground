import std.stdio;

immutable MAX = 10_000_000;
ubyte sequenceEnd[7 * 81 + 1];

uint sumDigitsSquare(uint x)
{
  uint sum;
  do {
    auto digit = x % 10;
    sum += digit * digit;
    x /= 10;
  } while (x > 0);
  return sum;
}

ubyte computeSequenceEnd(uint n)
{
  auto sds = sumDigitsSquare(n);
  if (sequenceEnd[sds] != 0) return sequenceEnd[sds];
  return sequenceEnd[sds] = computeSequenceEnd(sds);
}

void main()
{
  uint count;
  sequenceEnd[1] = 1;
  sequenceEnd[89] = 89;
  foreach (i; 1 .. MAX)
    if (computeSequenceEnd(i) == 89)
      ++count;
  writeln(count);
}
