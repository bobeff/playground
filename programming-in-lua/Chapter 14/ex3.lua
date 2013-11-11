--[[
    Explain in detail what happens in the following program and what its output
is.
]]

local print = print
function foo(_ENV, a)
    print(a + b)
end

foo({b = 14}, 12)
foo({b = 10}, 1)
