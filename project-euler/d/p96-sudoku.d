module sudoku;

import std.stdio;

unittest {
	ubyte[9][9] sudoku = [
		[0, 0, 3, 0, 2, 0, 6, 0, 0],
		[9, 0, 0, 3, 0, 5, 0, 0, 1],
		[0, 0, 1, 8, 0, 6, 4, 0, 0],
		[0, 0, 8, 1, 0, 2, 9, 0, 0],
		[7, 0, 0, 0, 0, 0, 0, 0, 8],
		[0, 0, 6, 7, 0, 8, 2, 0, 0],
		[0, 0, 2, 6, 0, 9, 5, 0, 0],
		[8, 0, 0, 2, 0, 3, 0, 0, 9],
		[0, 0, 5, 0, 1, 0, 3, 0, 0],
	];

	ubyte[9][9] expected = [
		[4, 8, 3, 9, 2, 1, 6, 5, 7],
		[9, 6, 7, 3, 4, 5, 8, 2, 1],
		[2, 5, 1, 8, 7, 6, 4, 9, 3],
		[5, 4, 8, 1, 3, 2, 9, 7, 6],
		[7, 2, 9, 5, 6, 4, 1, 3, 8],
		[1, 3, 6, 7, 9, 8, 2, 4, 5],
		[3, 7, 2, 6, 8, 9, 5, 1, 4],
		[8, 1, 4, 2, 5, 3, 7, 6, 9],
		[6, 9, 5, 4, 1, 7, 3, 8, 2],
	];

	SolveSudoku(sudoku);
	assert(sudoku == expected);
}

void SolveSudoku(ref ubyte[9][9] sudoku) {
	bool found = false;

	bool CheckPosition(ubyte x, ubyte y) {

		bool Check(ref ubyte[9] arr) {
			foreach (i; 0 .. 9)
				if (arr[i] > 1)
					return false;
			return true;
		}

		// check row
		ubyte[9] row;
		foreach (i; 0 .. 9)
			if (sudoku[x][i] != 0)
				++row[sudoku[x][i] - 1];
		if (!Check(row))
			return false;

		// check column
		ubyte[9] col;
		foreach (i; 0 .. 9)
			if (sudoku[i][y] != 0)
				++col[sudoku[i][y] - 1];
		if (!Check(col))
			return false;

		// check square
		x = x / 3 * 3;
		y = y / 3 * 3;

		ubyte[9] sqr;
		foreach (i; x .. x + 3)
			foreach (j; y .. y + 3)
				if (sudoku[i][j] != 0)
					++sqr[sudoku[i][j] - 1];
		if (!Check(sqr))
			return false;

		return true;
	}

	void Solve(ubyte index) {
		if (index >= 81) {
			found = true;
			return;
		}

		ubyte x = cast(ubyte)(index / 9);
		ubyte y = cast(ubyte)(index % 9);

		if (sudoku[x][y] != 0) {
			Solve(cast(ubyte)(index + 1));
			return;
		}

		foreach (ubyte i; 1 .. 10) {
			sudoku[x][y] = i;
			if (CheckPosition(x, y)) {
				Solve(cast(ubyte)(index + 1));
				if (found) return;
			}
		}

		sudoku[x][y] = 0;
	}

	Solve(0);
}

void main() {
	auto input = File("sudoku.txt", "r");
	auto output = File("output.txt", "w");

	char[] buffer;
	uint sum = 0;

	while (input.readln(buffer)) {
		output.write(buffer);

		ubyte sudoku[9][9];

		foreach (i; 0 .. 9) {
			input.readln(buffer);
			foreach (j; 0 .. 9)
				sudoku[i][j] = cast(ubyte)(buffer[j] - '0');
		}

		SolveSudoku(sudoku);
		sum += sudoku[0][0] * 100 + sudoku[0][1] * 10 + sudoku[0][2];

		foreach (i; 0 .. 9) {
			foreach (j; 0 .. 9)
				output.write(sudoku[i][j]);
			output.writeln();
		}
	}

	writeln(sum);
}
