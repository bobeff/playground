--[[
    Modify the Listing 12.2 so that it uses the syntax ["key"] = value, as
suggested in Section 12.2.
]]

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
            io.write("[")
            serialize(k)
            io.write("] = ")
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

local t0 = {1, 2, 3}
local t1 = { a = 12, b = 'Lua', key = "another \"one\"", c = t0 }
local t2 = { t = t1, key = "yet another key" }
serialize(t2)
