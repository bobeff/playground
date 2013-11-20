--[[
    Write a function to test whether a given number is a power of two.
]]

function ispowerof2(x)
    if x < 1 then
        return false
    end
    while x > 1 do
        if x % 2 ~= 0 then
            return false
        end
        x = x / 2
    end
    return true
end

for i = 0, 32 do
    print(i, ispowerof2(i))
end
