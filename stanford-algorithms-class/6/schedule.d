import std.stdio;
import std.c.stdio;
import std.array;
import std.algorithm;

struct Job {
	uint weight;
	uint length;
	int diff;
	float ratio;
}

Job[] jobs;

ulong getValue() {
	ulong ret, time;
	foreach (ref j; jobs) {
		time += j.length;
		ret += j.weight * time;
	}
	return ret;
}

void main() {
	auto f = File("jobs.txt");
	uint jobsCount;
	fscanf(f.getFP(), "%d", &jobsCount);
	jobs = uninitializedArray!(Job[])(jobsCount);
	foreach (ref j; jobs) {
		fscanf(f.getFP(), "%d%d", &j.weight, &j.length);
		j.diff = j.weight - j.length;
		j.ratio = cast(float)j.weight / j.length;
	}
	debug writeln(jobs);
	sort!((a, b) { return a.diff > b.diff ||
		a.diff == b.diff && a.weight > b.weight; })(jobs);
	debug writeln(jobs);
	writeln("Greedy difference: ", getValue());
	sort!((a, b) { return a.ratio > b.ratio; })(jobs);
	debug writeln(jobs);
	writeln("Greedy ratio: ", getValue());
}
