local ex5mod = require"ex5mod"

local function printTable(t)
    for k, v in pairs(t) do
        io.write(string.format("t['%s'] = %s\n", tostring(k), tostring(v)))
    end
    io.write("\n")
end

ex5mod.settrans({ a = "b", l = "m", b = "a", [" "] = false })
printTable(ex5mod.gettrans())
print(ex5mod.transliterate("ala bala"))
print();

ex5mod.settrans({ a = false, l = false, b = false, [" "] = "*" })
printTable(ex5mod.gettrans())
print(ex5mod.transliterate("ala bala"))
print()

ex5mod.settrans({ " ", " ", " " })
printTable(ex5mod.gettrans())
print(pcall(ex5mod.transliterate, "ala2bala"))
print(pcall(ex5mod.settrans, 9))
print()

ex5mod.settrans({})
printTable(ex5mod.gettrans())
print(ex5mod.transliterate("ala bala"))
