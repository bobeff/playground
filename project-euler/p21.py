def SumOfDivisors(x):
    return sum([y for y in xrange(1, x / 2 + 1) if x % y == 0])

MAX = 10000
result = 0

for i in xrange(1, MAX):
    s1 = SumOfDivisors(i)
    if i == s1: continue;
    s2 = SumOfDivisors(s1)
    if i == s2: result += i

print result
