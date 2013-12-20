--[[
    Compare the performance of Lua programs that copy the standard input
file to the standard output file in the following ways:
    * byte by byte
    * line by line
    * in chunks of 8 kB
    * the whole file at once
]]

for byte in io.lines(stdin, 1) do
    io.write(byte)
end
