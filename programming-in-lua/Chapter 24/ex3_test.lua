local dbg = require"dbg"

local function sum(x, y)
    local z = x + y
    coroutine.yield(z)
    sum(y, z)
end

local co = coroutine.create(sum)

for i = 1, 10 do
    coroutine.resume(co, 1, 1)
    dbg.setvarvalue(co, "y", 0, 1)
    dbg.setvarvalue(co, "z", 0, 1)
    for j = i - 1, 0, -1 do
        print(dbg.getvarvalue(co, "z", j))
    end
    print()
end
