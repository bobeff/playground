--[[
    Change the previous program so that it asks for a confirmation if the user
gives the name of an existing file for its output.
]]

if arg[2] then
    local file = io.open(arg[2], "r")
    if file then
        local answer
        while answer ~= "Y" and answer ~= "N" do
            io.write("Are you sure that you want to override ",
                arg[2], " ? (y/n): ")
            answer = string.upper(io.read())
        end
        if answer == "N" then
            os.exit(0)
        end
    end
end

if arg[1] then io.input(arg[1]) end

local lines = {}
for line in io.lines() do
    lines[#lines + 1] = line
end

if arg[2] then io.output(arg[2]) end

table.sort(lines)

for _, line in ipairs(lines) do
    io.write(line, "\n")
end
