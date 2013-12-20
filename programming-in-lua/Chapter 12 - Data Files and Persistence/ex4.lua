--[[
    Modify the code of the previous exercise so that it uses the constructor
syntax for lists whenever is possible. For instance, it should serialize the
table {14, 15, 19} as {14, 15, 19}, not as {[1] = 14, [2] = 15, [3] = 19}
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

function serialize(o, ident, toIdent)
    local ident = ident or 1
    if type(o) == "number" then
        io.write(o)
    elseif type(o) == "string" then
        io.write(string.format("%q", o))
    elseif type(o) == "table" then
        local identStr1 = string.rep("\t", ident)
        local identStr2 = string.rep("\t", ident - 1)
        io.write(toIdent and identStr2 or "", "{\n")
        for i = 1, #o do
            if type(o[i]) == "table" then
                serialize(o[i], ident + 1, true)
            else
                io.write(identStr1, o[i], ",\n")
            end
        end
        for k, v in pairs(o) do
            if type(k) == "number" and k >= 1 and k <= #o then
                goto next_iteration
            end
            io.write(identStr1)
            if isIdentifier(tostring(k)) then
                io.write(k, " = ")
            else
                io.write("[")
                serialize(k)
                io.write("] = ")
            end
            serialize(v, ident + 1)
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

local t0 = {1, b = 2, 3}
local t1 = { a = 12, b = 'Lua', key = "another \"one\"", t0}
local t2 = { t = t1, key = "yet another key", ["function"] = 5 }
serialize(t2)
print()
serialize{14, 15, 19}
print()
serialize{{1, {2, 3}, 4}, {a = 5, 6}, {7, {8, {9}, b = 10}, 11}}
