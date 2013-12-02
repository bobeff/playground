--[[
    Why the recursion in function getvarvalue is guaranteed to stop ?
]]

--[[
    Answer:
The recurssion will stop because the function always have upvalue called
"_ENV"
]]

function getvarvalue(name, level)
    local value
    local found = false

    level = (level or 1) + 1

    -- try local variables
    for i = 1, math.huge do
        local n, v = debug.getlocal(level, i)
        if not n then break end
        if n == name then
            value = v
            found = true
        end
    end
    if found then return value end

    -- try non local variables
    local func = debug.getinfo(level, "f").func
    for i = 1, math.huge do
        local n, v = debug.getupvalue(func, i)
        if not n then break end
        if n == name then return v end
    end

    -- not found; get value from the environment
    local env = getvarvalue("_ENV", level)
    return env[name]
end

print(getvarvalue("a", 0))
