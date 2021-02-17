proc reverse*(s: string): string =
  result = newString(s.len)
  for i in 0 ..< s.len:
    result[s.len - i - 1] = s[i]
