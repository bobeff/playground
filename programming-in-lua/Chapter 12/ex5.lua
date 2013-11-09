--[[
    The approach of avoiding constructors when saving tables with cycles
is too redical. It is possible to save table in a more pleasent format
using constructors for the general case, and to use assignments later only
to fix sharing and loops.
    Reimplement function save using this approach. Add to it all the goodies
that you have implemented in the previous exercises (identation, record syntax,
and list syntax).
]]

local reservedWords =
{
    ["and"] = true, ["break"] = true, ["do"] = true, ["else"] = true,
    ["elseif"] = true, ["end"] = true, ["false"] = true, ["for"] = true,
    ["function"] = true, ["if"] = true, ["in"] = true, ["local"] = true,
    ["nil"] = true, ["not"] = true, ["or"] = true, ["repeat"] = true,
    ["return"] = true, ["then"] = true, ["true"] = true, ["until"] = true,
    ["while"] = true, ["goto"] = true
}

local function isIdentifier(s)
    return not reservedWords[s] and string.match(s, "[_%a][_%w]*")
end

local function serialize(name, o, saved, prevName, ident, toIdent)
    local saved = saved or {}
    local ident = ident or 1
    if type(o) == "number" then
        io.write(o)
    elseif type(o) == "string" then
        io.write(string.format("%q", o))
    elseif type(o) == "table" then
        if saved[o] then saved[o].name = name; return end
        saved[o] = { name = "", value = name }
        local identStr1 = string.rep("\t", ident)
        local identStr2 = string.rep("\t", ident - 1)
        io.write(toIdent and identStr2 or "", "{\n")
        for i = 1, #o do
            if type(o[i]) == "table" then
                local n = string.format("%s[%d]", name, i)
                serialize(n, o[i], saved, name, ident + 1, true)
            else
                io.write(identStr1, o[i], ",\n")
            end
        end
        for k, v in pairs(o) do
            if type(k) == "number" and k >= 1 and k <= #o then
                goto next_iteration
            end
            if saved[v] then
                local fmt
                if isIdentifier(tostring(k)) then
                    fmt = "%s.%s"
                elseif type(k) == "number" then
                    fmt = "%s[%d]"
                else
                    fmt = "%s[%q]"
                end
                saved[v].name = string.format(fmt, name, k)
                goto next_iteration
            end
            io.write(identStr1)
            if isIdentifier(tostring(k)) then
                io.write(k, " = ")
            else
                io.write("[")
                serialize(nil, k)
                io.write("] = ")
            end
            local n = string.format("%s[%s]", name, tostring(k))
            serialize(n, v, saved, name, ident + 1)
            if type(v) ~= "table" then
                io.write(",\n")
            end
            ::next_iteration::
        end
        io.write(identStr2, ident == 1 and "}\n" or "},\n")
    else
        error("cannot serialize a " .. type(o))
    end
end

function save(name, value, saved, used)
    local saved = saved or {}
    io.write(name, " = ")
    serialize(name, value, saved)
    for k, v in pairs(saved) do
        if v.name ~= "" then
            if not used or used and not used[v.name] then
                io.write(v.name, " = ", v.value, "\n")
            end
            if used then used[v.name] = true end
        end
    end
end

local a = {x = 1, y = 2, {3, 4, 5}, }
a[10] = a    -- cycle
a.z  = a[1]  -- shared subtable
save("a", a)

print()

local b = {{"one", "two"}, 3}
local c = {k = b[1]}

local t = {}
save("b", b, t)
save("c", c, t)

print()

local t = {}
local used = {}
local t0 = {1, b = 2, 3}
local t1 = { a = 12, b = 'Lua', key = "another \"one\"", t0}
local t2 = { t = t1, key = "yet another key", ["function"] = 5 }
save("t0", t0, t, used)
save("t1", t1, t, used)
save("t2", t2, t, used)
