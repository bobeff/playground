--[[
    Write an experiment to determine whether Lua actually implements
tables. If possible try your code in both Lua 5.1 and in Lua 5.2 to
see the difference.
]]

local mem = {}
setmetatable(mem, { __mode = "k" })

function factory(o)
    local res = mem[o]
    if not res then
        res = function() return o end
        mem[o] = res
    end
    return res
end

local function printTable(t)
    for k, v in pairs(t) do
        print(k, v)
    end
end

print(_VERSION)
local t = {}
local f = factory(t)
printTable(mem)
t, f = nil, nil
collectgarbage()
printTable(mem)
