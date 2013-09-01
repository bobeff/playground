import std.stdio;

immutable N = 1001;

uint[2][4] shift = [
	[ 0,  1],
	[ 1,  0],
	[ 0, -1],
	[-1,  0],
];

int[N][N] spiral;

void main() {
	uint step = 1;
	uint currentLng = 0;
	uint lng = 1;
	int direction = -1;
	int x = N / 2;
	int y = N / 2;
	spiral[x][y] = 1;
	while (true) {
		++step;
		++currentLng;
		if (currentLng > lng / 2) {
			++lng;
			++direction;
			currentLng = 1;
		}
		x += shift[direction % 4][0];
		y += shift[direction % 4][1];
		if (x < 0 || x >= N || y < 0 || y >= N)
			break;
		spiral[x][y] = step;
	}

	/*
	foreach (i; 0 .. N) {
		foreach (j; 0 .. N)
			writef("%3d", spiral[i][j]);
		writeln();
	}
	*/

	uint sum = 0;
	foreach (i; 0 .. N) {
		sum += spiral[i][i];
		sum += spiral[i][N - i - 1];
	}

	writeln(sum - spiral[N / 2][N / 2]);
}
