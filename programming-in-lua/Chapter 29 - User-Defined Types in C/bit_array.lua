local bitarray = require"bitarray"

local a = bitarray.new(20)
print(a)
for i = 6, 15 do a[i] = true end
for i = 6, 15 do io.write(tostring(a[i]), " ") end
print()
print("len(a) = ", #a)
print()

local a1 = bitarray.from_string("01101")
local a2 = bitarray.from_string("10101")
print("a1 = \t", a1)
print("a2 = \t", a2)
print("a1 * a2 = ", a1 * a2);
print("a1 + a2 = ", a1 + a2);
print()

a1 = bitarray.from_string("0110001110")
a2 = bitarray.from_string("11101");
print("a1 = \t", a1)
print("a2 = \t", a2)
print("a1 + a2 = ", bitarray.union(a1, a2))
print("a1 * a2 = ", bitarray.intersect(a2, a1))
print()

print(bitarray.from_string(00011100))

print(pcall(bitarray.from_string, "213"))
print(pcall(bitarray.from_string, ""))
a[5] = nil
