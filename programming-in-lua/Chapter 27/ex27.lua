local ex27mod = require"ex27mod"

print(ex27mod.sumation())
print(ex27mod.sumation(2.3, 5.4))
print(ex27mod.sumation(2.3, 5.4, -34))
print(pcall(ex27mod.sumation, 2.3, 5.4, {}))

local function printTable(t)
    for k, v in pairs(t) do
        print(k, v);
    end
end

print()
printTable(ex27mod.pack())
print()
printTable(ex27mod.pack(2, {}))
print()
printTable(ex27mod.pack(3.14, {5, 6}, function() end))
print()

print(ex27mod.reverse())
print(ex27mod.reverse(1))
print(ex27mod.reverse(1, 2))
print(ex27mod.reverse(1, 3, 2))
print(ex27mod.reverse(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

print();
ex27mod.foreach({}, print)
ex27mod.foreach({x = 10, y = 20}, print)
print(pcall(ex27mod.foreach, {}, {}))
print()

function foo()
    ex27mod.foreach({x = 10, y = 20, z = 30}, function(key, value)
        io.write(key, " ");
        coroutine.yield();
        io.write(value, "\n");
    end)
end

local f = coroutine.wrap(foo)
for i = 1, 4 do f() end
