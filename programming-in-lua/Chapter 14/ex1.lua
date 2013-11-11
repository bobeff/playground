--[[
    The 'getfield' function which we defined in  the beginning of this chapter
is too forgiving, as it accepts "fields" like math?sin or string!!!gsub.
Rewrite it so that it accepts only single dots as name separator.
]]

function getfield(f)
    local v = _G
    for w in string.gmatch(f, "([%w_]+%.)") do
        v = v[string.sub(w, 1, #w - 1)]
    end
    local i, j = string.find(f, "%.?[%w_]+$")
    local w = string.sub(f, i + 1, j)
    return v[w]
end

function setfield(f, v)
    local t = _G
    for w, d in string.gmatch(f, "([%w_]+)(%.?)") do
        if d == "." then
            t[w] = t[w] or {}
            t = t[w]
        else
            t[w] = v
        end
    end
end

setfield("t.x.y", 10)
print(t.x.y)
print(getfield("t.x.y"))
print(getfield("t?x!!!y"))
