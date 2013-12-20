--[[
    Consider that you have to implement a memorizing table for a function from
strings to strings. Doing the table weak will not allow the removal of entries,
because weak tables do not consider strings as collectable objects. How can you
implement memorization in this case ?
]]

local mem = {}
setmetatable(mem, { __mode = "v" })

function toUpper(...)
    local t = {...}
    local res = {}
    for i = 1, #t do
        if type(t[i]) ~= "string" then
            error("Only string values expected.", 2)
        end
        local k = t[i]
        local v = mem[k]
        if not v then
            v = { string.upper(k) }
            mem[k] = v
        end
        res[#res + 1] = v[1]
    end
    return table.unpack(res)
end

local function printTable(t)
    for k, v in pairs(t) do
        print(k, v[1])
    end
end

print(toUpper("foo", "bar", "barfoo", "bar"))
print()
printTable(mem)
print()
collectgarbage()
printTable(mem)
