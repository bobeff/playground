import std.stdio;
import std.algorithm;
import std.typecons;

alias Tuple!(ubyte, ubyte, ubyte) Triple;

uint indexOf(ubyte digit, ubyte[] digits)
{
  foreach (i; 0 .. digits.length)
    if (digits[i] == digit)
      return i;
  writefln("Digit %s not found in %s", digit, digits);
  return -1;
}

ubyte get(char c) { return cast(ubyte)(c - '0'); }

void main()
{
  auto file = File("keylog.txt");
  bool[10] isDigitUsed;
  Triple[] triples;
  // process the input
  foreach (line; file.byLine()) {
    auto t = Triple(get(line[0]), get(line[1]), get(line[2]));
    triples ~= t;
    isDigitUsed[t[0]] = true;
    isDigitUsed[t[1]] = true;
    isDigitUsed[t[2]] = true;
  }
  // find used digits
  ubyte[] digits;
  foreach (ubyte i; 0 .. 10)
    if (isDigitUsed[i])
      digits ~= i;
  // for every possible passcode
  outer: do {
    // check it with the input
    foreach (ref t; triples) {
      if (indexOf(t[0], digits) > indexOf(t[1], digits)) continue outer;
      if (indexOf(t[0], digits) > indexOf(t[2], digits)) continue outer;
      if (indexOf(t[1], digits) > indexOf(t[2], digits)) continue outer;
    }
    writeln(digits);
    return;
  } while(nextPermutation(digits));
}
