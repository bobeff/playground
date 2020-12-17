proc distance*(s1, s2: string): int =
  if s1.len != s2.len:
    raise newException(ValueError, "The strings are of different length.")
  for i in 0 ..< s1.len:
    if s1[i] != s2[i]:
      result.inc
