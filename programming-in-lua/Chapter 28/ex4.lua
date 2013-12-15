local ex4mod = require"ex4mod"

local function printTable(t)
    for k, v in pairs(t) do
        io.write(string.format("t['%s'] = %s\n", tostring(k), tostring(v)))
    end
    io.write("\n")
end

ex4mod.settrans({ a = "b", l = "m", b = "a", [" "] = false })
printTable(ex4mod.gettrans())
print(ex4mod.transliterate("ala bala"))
print();

ex4mod.settrans({ a = false, l = false, b = false, [" "] = "*" })
printTable(ex4mod.gettrans())
print(ex4mod.transliterate("ala bala"))
print()

ex4mod.settrans({ " ", " ", " " })
printTable(ex4mod.gettrans())
print(pcall(ex4mod.transliterate, "ala2bala"))
print(pcall(ex4mod.settrans, 9))
print()

ex4mod.settrans({})
printTable(ex4mod.gettrans())
print(ex4mod.transliterate("ala bala"))
