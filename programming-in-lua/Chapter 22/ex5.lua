--[[
    Generalize the previous program so that it prints the last n lines of a
text file. Againg, try to avoid reading the entire file when the file is
large and seekable.
]]

local file = io.open(arg[1], "r")
if not file then
    io.write("Can not open ", arg[1], "\n")
    os.exit(0)
end

local n = tonumber(arg[2])
if not n then
    io.write("Invalid number of lines ", arg[2], "\n")
    os.exit(0)
end

local count = 0
local index = file:seek("end")
while count <= n and index > 0 do
    index = index - 1
    file:seek("set", index)
    byte = file:read(1)
    if byte == "\n" then count = count + 1 end
end

if index == 0 then
    file:seek("set")
end

for i = 1, n do
    local line = file:read("*L")
    if not line then break end
    io.write(i, "\t", line)
end
