local lxp = require"lxp"

local count = 0

local callbacks =
{
    StartElement = function(parser, tagname, attributes)
        io.write("+ ", string.rep(" ", count), tagname, " ( ")
        for index, key in ipairs(attributes) do
            io.write(key, " = ", attributes[key], ", ")
        end
        io.write(")\n")
        count = count + 1
    end,

    EndElement = function(parser, tagname)
        count = count - 1
        io.write("- ", string.rep(" ", count), tagname, "\n")
    end,

    CharacterData = function(parser, data)
        io.write(string.rep(" ", count + 2), data, "\n")
    end,
}

local p = lxp.new(callbacks)
local f = io.open("test.xml", "r")

for line in f:lines() do
    assert(p:parse(line))
end
assert(p:parse())

p:close()
f:close()
