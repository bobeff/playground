import std.stdio;
import std.conv;
import std.array;

immutable SIZE = 80;

void main()
{
  uint row;
  uint[SIZE][SIZE] matrix;
  auto f = File("matrix.txt");
  foreach (line; f.byLine()) {
    auto numbers = line.split(",");
    foreach (col, n; numbers)
      matrix[row][col] = to!uint(n);
    ++row;
  }

  foreach (r; 1 .. SIZE)
    matrix[r][0] += matrix[r - 1][0];
  foreach (c; 1 .. SIZE)
    matrix[0][c] += matrix[0][c - 1];
  foreach (r; 1 .. SIZE)
    foreach (c; 1 .. SIZE)
      if (matrix[r - 1][c] < matrix[r][c - 1])
        matrix[r][c] += matrix[r - 1][c];
      else
        matrix[r][c] += matrix[r][c - 1];
  writeln(matrix[SIZE-1][SIZE-1]);
}
