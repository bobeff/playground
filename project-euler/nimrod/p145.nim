import unsigned

proc reverse(nn : uint32) : uint32 =
  var n = nn
  if n mod 10 == 0: return 0
  while n > 0u:
    result *= 10
    result += n mod 10
    n = n div 10
    
proc areDigitsOdd(nn : uint32) : bool =
  var n = nn
  while n > 0u:
    if (n mod 10) mod 2 == 0:
      return false
    n = n div 10
  return true
  
proc countReversables(n : uint32) : uint32 =
  for i in 1 .. n:
    if areDigitsOdd(i + reverse(i)):
      inc(result)
      
echo(countReversables(1_000_000_000))
