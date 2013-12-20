local intarray = require"intarray"

print(pcall(intarray.from_sequence, {}))
print(intarray.from_sequence{13})
print(intarray.from_sequence{1, 5, 6, 4, 3})
print(#intarray.from_sequence{1, 2, 3, 5})

local a = intarray.new(10)
print(a)
for i = 10, 1, -1 do a[i] = 10 - i + 1 end
print(a)
for i = 1, 10 do io.write(a[i], " ") end
print()
