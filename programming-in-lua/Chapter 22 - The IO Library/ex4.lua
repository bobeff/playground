--[[
    Write a program that prints the last line of a text file. Try to avoid
reading the entire file when file is large and seekable.
]]

local file = io.open(arg[1], "r")
if not file then
    io.write("Can not open ", arg[1], "\n")
    os.exit(0)
end

local byte, line
local index = file:seek("end")
while byte ~= "\n" and index > 0 do
    index = index - 1
    file:seek("set", index)
    byte = file:read(1)
    if byte == "\n" then
        line = file:read("*L")
        if not line then
            byte = 0
        end
    end
end

if index == 0 then
    file:seek("set")
    line = file:read("*L")
end

io.write(line)
