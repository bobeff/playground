import std.stdio;

immutable LENGTH = 150_000;

void main()
{
  uint[LENGTH] composite;
	foreach (i; 2 .. LENGTH)
		if (!composite[i])
			for (auto j = 2 * i; j < LENGTH; j += i)
				++composite[j];
	foreach (i; 2 .. LENGTH) {
    uint count;
    foreach (j; 0 .. 4)
      if (composite[i + j] == 4)
        ++count;
      else
        break;
    if (count == 4) {
      writeln(i);
      break;
    }
  }
}
