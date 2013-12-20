local dbg = require"dbg"

local function foo(x, y)
    local z = x + y
    local a = y + z
    local b = z + a
    return b
end

local function bar(x, y)
    local a = x * y
    local b = a * y
    local c = foo(a, b)
    local d = c * c
    return d
end

local b1 = dbg.setbreakpoint(5)
local b2 = dbg.setbreakpoint(6)

print(foo(1, 2))

dbg.removebreakpoint(b2)

print(foo(1, 2))

local b3 = dbg.setbreakpoint(14)
local b4 = dbg.setbreakpoint(12)

print(bar(1, 1))
