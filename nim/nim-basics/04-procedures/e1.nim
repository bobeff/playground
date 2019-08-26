# Create a procedure which will greet a person (print "Hello <name>") based on
# the provided name. Create a sequence of names. Greet each person using the
# created procedure.

proc swear(name: string) =
  echo "Fuck you ", name, "!"

let names = @["Ivan", "Stoyan", "Dragan", "Stavri"]

for name in names:
  swear(name)
