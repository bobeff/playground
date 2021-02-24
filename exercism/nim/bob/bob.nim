import std/[strutils]

func isAllCapital(message: string): bool =
  var hasCapital = false
  for c in message:
    if c.isAlphaAscii:
      if c notin {'A' .. 'Z'}:
        return false
      else:
        hasCapital = true
  return hasCapital

func hey*(message: string): string =
  let message = message.strip
  if message.len == 0:
    "Fine. Be that way!"
  elif message[message.len - 1] == '?':
    if message.isAllCapital:
      "Calm down, I know what I'm doing!"
    else:
      "Sure."
  elif message.isAllCapital:
    "Whoa, chill out!"
  else:
    "Whatever."
