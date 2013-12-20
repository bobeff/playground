--[[
    Write a function to test whether a given integer is a power of two.
]]

function ispowerof2(x)
    if x < 1 then return false end
    return not bit32.btest(x, x - 1)
end

for i = 0, 32 do
    print(i, ispowerof2(i))
end
