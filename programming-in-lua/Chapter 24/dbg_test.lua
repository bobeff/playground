dbg = require"dbg"

local x = 5
y = 10

local function test(a, b)
    local c = a + b + x
    dbg._debug()
    return c
end

print(test(1, 2))
