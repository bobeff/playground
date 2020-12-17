import strformat

proc twoFer*(name = ""): string =
  let name = if name.len > 0: name else: "you"
  return &"One for {name}, one for me."
