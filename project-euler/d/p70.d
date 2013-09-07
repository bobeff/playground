import std.stdio;
import std.array;

immutable MAX = 10_000_000;

bool isPerm(uint x, uint y)
{
    uint[10] dx, dy;
    while (x > 0 && y > 0) {
        ++dx[x % 10];
        ++dy[y % 10];
        x /= 10;
        y /= 10;
    }
    if (x > 0 || y > 0)
        return false;
    foreach (i; 0 .. 10)
        if (dx[i] != dy[i])
            return false;
    return true;
}

void main()
{
    uint[] totientSieve = uninitializedArray!(uint[])(MAX);
    foreach (i; 1 .. MAX)
        totientSieve[i] = i;
    foreach (i; 1 .. MAX)
        for (uint j = 2 * i; j < MAX; j += i)
            totientSieve[j] -= totientSieve[i];
    uint bestNumber;
    float minRatio = float.max;
    foreach (i; 2 .. MAX)
        if (isPerm(i, totientSieve[i])) {
            float ratio = cast(float)(i) / totientSieve[i];
            if (ratio < minRatio) {
                minRatio = ratio;
                bestNumber = i;
            }
        }
    writeln(bestNumber);
}
