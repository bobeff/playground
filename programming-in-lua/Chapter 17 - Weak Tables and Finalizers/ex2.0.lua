--[[
    Consider the first example of Section 17.6, which creates a table with a
finalizer that only prints a message when activated. What happens if the
program ends without a collection cycle ? What happens if program calls
os.exit ? What happens if the program ends with some error ?
]]

o = {x = "hi"}
setmetatable(o, { __gc = function(o) print(o.x) end })
o = nil
collectgarbage()
