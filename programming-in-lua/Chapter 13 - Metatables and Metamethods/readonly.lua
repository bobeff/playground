--[[
    Exercise 13.4: An alternative method to implemen tread-only tables might
use a function as the __index metamethod. That alternative makes accesses more
expensive, but the creation of read-only tables is cheaper, as all read-only
tables can share a single metatable. Rewrite function readOnly using this
approach.
]]

local index = {}

local mt = {
    __index = function(t, k)
        return t[index][k]
    end,

    __newindex = function(t, k, v)
        error("attempt to update read-only table", 2)
    end,

    __len = function(t)
        return #t[index]
    end,
}

function readOnly(t)
    local proxy = {}
    proxy[index] = t
    setmetatable(proxy, mt)
    return proxy
end

local t = readOnly{1, 2, 3}
setmetatable(t, mt)
for i = 1, #t do
    print(t[i])
end
t[2] = 5
