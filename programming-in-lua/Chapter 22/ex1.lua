--[[
    Write a program that reads a text file and rewrites it with it lines
sorted in alphabetical order. When called with no arguments, it should read
from the standard input file and write to the standard output file. When
called with one file-name argument, it should read from that file and write
to the standard output file. When called with two file-name arguments, it
should read from the first file and write to the second one.
]]

if arg[1] then io.input(arg[1]) end
if arg[2] then io.output(arg[2]) end

local lines = {}
for line in io.lines() do
    lines[#lines + 1] = line
end

table.sort(lines)

for _, line in ipairs(lines) do
    io.write(line, "\n")
end
