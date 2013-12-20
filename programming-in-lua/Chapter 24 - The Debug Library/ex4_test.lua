local dbg = require"dbg"

a = 5
b = 10
local function sum(x, y)
    local z = x + y
    coroutine.yield(z)
    sum(y, z)
end

local co = coroutine.create(sum)

for i = 1, 10 do
    print(select(2, coroutine.resume(co, 1, 1)))
    local vars = dbg.getallvars(co, 0)
    for k, v in pairs(vars) do
        print(k, v)
    end
end

local vars = dbg.getallvars(co, 0)
print(vars["a"])
print(vars["b"])
