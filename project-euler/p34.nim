proc fact(n : int) : int =
    if n == 0: return 1
    else: return fact(n - 1) * n

proc precompute(n : int) : array[0 .. 9, int] =
    for i in 0 .. 9:
        result[i] = fact(i)
    
const f : array[0 .. 9, int] = precompute(9)
    
proc findUpperBound() : int =
    result = 2 * f[9]
    var x = 99
    while result >= x:
        inc(result, f[9])
        x = x * 10 + 9
    
proc isCurious(x : int) : bool =
    var s = $x
    var sum = 0
    for i in 0 .. len(s) - 1:
        inc(sum, f[ord(s[i]) - ord('0')])
    return sum == x

var upperBound = findUpperBound()
var sum = 0
for i in 10 .. upperBound:
    if isCurious(i):
        inc(sum, i)
echo(sum)
