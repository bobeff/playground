import std.stdio;
import std.array;
import std.conv;
import std.string;
import std.ascii;

immutable string[10] words = [ // 10 most common in English
  "the", "be", "to", "of", "and", "a", "in", "that", "have", "i" ];

uint decript(char[] text, char[3] cipher)
{
  foreach (i; 0 .. text.length)
    text[i] = text[i] ^ cipher[i % 3];

  string word;
  string[] textWords;
  foreach (i; 0 .. text.length)
    if (isAlpha(text[i]))
      word ~= text[i];
    else if (word != "") {
      textWords ~= toLower(word);
      word = "";
    }

  uint score;
  foreach (w1; textWords)
    foreach (w2; words)
      if (w1 == w2) {
        ++score;
        break;
      }

  return score;
}

void main()
{
  auto f = File("cipher1.txt");
  string line;
  f.readln(line);

  auto characters = line.split(",");
  characters[$ - 1] = chop(characters[$ - 1]);
  
  char[] text;
  text.length = characters.length;
  foreach (i, c; characters)
    text[i] = cast(char)(to!uint(c));

  uint bestScore;
  char[3] cipher, bestCipher;
  foreach (char c1; 'a' .. 'z') {
    cipher[0] = c1;
    foreach (char c2; 'a' .. 'z') {
      cipher[1] = c2;
      foreach (char c3; 'a' .. 'z') {
        cipher[2] = c3;
        auto score = decript(text.dup, cipher);
        if (score > bestScore) {
          bestScore = score;
          bestCipher = cipher;
        }
      }
    }
  }

  foreach (i; 0 .. text.length)
    text[i] = text[i] ^ bestCipher[i % 3];

  writeln(text);
  writeln();
  writeln("The cipher is: ", bestCipher);

  uint sum;
  foreach (i; 0 .. text.length)
    sum += text[i];

  writeln("The answor is: ", sum);
}
