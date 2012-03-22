import std.stdio;
import std.string;
import std.conv;
import std.algorithm;

ubyte[][] input;
uint[][] paths;

void ReadInput(string inputFileName)
{
	auto inputStream = File(inputFileName);
	
	foreach (string line; lines(inputStream))
	{
		auto numbers = split(line);
		++input.length;
		
		foreach (n; numbers)
		{
			input[$ - 1] ~= to!ubyte(n);
		}
	}
}

void FillPathsTable()
{
	// resize paths table 
	paths.length = input.length;
	foreach (i, ref row; paths)
	{
		row.length = input[i].length;
	}
	
	paths[0][0] = input[0][0];
	
	foreach (i; 0 .. paths.length - 1)
	{
		foreach (j; 0 .. paths[i].length)
		{
			paths[i+1][j] = max(paths[i][j]+input[i+1][j], paths[i+1][j]);
			paths[i+1][j+1] = max(paths[i][j]+input[i+1][j+1], paths[i+1][j+1]);
		}
	}
}

int main(string[] args)
{
	if (args.length < 2)
	{
		writeln("Use ", args[0], " <input_file_name>");
		return -1;
	}
	
	ReadInput(args[1]);
	FillPathsTable();

	writeln(reduce!(max)(paths[$-1]));
	
	return 0;
}
