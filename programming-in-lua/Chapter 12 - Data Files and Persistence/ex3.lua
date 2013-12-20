--[[
    Modify the code of the previous exercise so that it uses the syntax
["key"] = value only when necessary.
]]

local reservedWords =
{
    ["and"] = true, ["break"] = true, ["do"] = true, ["else"] = true,
    ["elseif"] = true, ["end"] = true, ["false"] = true, ["for"] = true,
    ["function"] = true, ["if"] = true, ["in"] = true, ["local"] = true,
    ["nil"] = true, ["not"] = true, ["or"] = true, ["repeat"] = true,
    ["return"] = true, ["then"] = true, ["true"] = true, ["until"] = true,
    ["while"] = true
}

local function isIdentifier(s)
    return not reservedWords[s] and string.match(s, "[_%a][_%w]*")
end

function serialize(o, ident)
    local ident = ident or 1
    if type(o) == "number" then
        io.write(o)
    elseif type(o) == "string" then
        io.write(string.format("%q", o))
    elseif type(o) == "table" then
        local identStr1 = string.rep("\t", ident)
        local identStr2 = string.rep("\t", ident - 1)
        io.write("{\n")
        for k, v in pairs(o) do
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
        end
        io.write(identStr2, ident == 1 and "}\n" or "},\n")
    else
        error("cannot serialize a " .. type(o))
    end
end

local t0 = {1, b = 2, 3}
local t1 = { a = 12, b = 'Lua', key = "another \"one\"", c = t0 }
local t2 = { t = t1, key = "yet another key", ["function"] = 5 }
serialize(t2)
