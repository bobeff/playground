local ex28mod = require"ex28mod"

local function printTable(t)
    for _, v in ipairs(t) do
        io.write(v, " ");
    end
    io.write("\n");
end

printTable(ex28mod.filter({}, function (x) return x > 0 end))
printTable(ex28mod.filter({1, 3, 20, -4, 5}, function (x) return x < 5 end))
print(pcall(ex28mod.filter, 1, 2))
print()

printTable(ex28mod.split("", ":"))
printTable(ex28mod.split("hi:ho:there", "ya"))
printTable(ex28mod.split("hi:ho:there", ":o"))
printTable(ex28mod.split("h\0i:h\0o:t\0h\0e\0r\0e", ":"))
printTable(ex28mod.split("\0\0\0\0", "\0"))
print(#ex28mod.split("\0\0\0\0", "\0"))
print(pcall(ex28mod.split, {}, {}))
print();

print(ex28mod.transliterate("ala bala",
    { a = "b", l = "m", b = "a", [" "] = false }))
print(ex28mod.transliterate("ala bala",
    { a = false, l = false, b = false, [" "] = "*" }))
print(pcall(ex28mod.transliterate, "ala2bala",
    { " ", " ", " " }))
print(ex28mod.transliterate("ala bala", {}))
