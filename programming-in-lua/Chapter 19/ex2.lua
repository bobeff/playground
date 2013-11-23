--[[
    Wtite a function to compute the Hamming weight of a given integer.
(The Hamming weight of a number is the number of ones in its binary
representation.)
]]

function hamming(x)
    local count = 0
    local mask = 1
    for i = 0, 31 do
        if bit32.btest(x, mask) then
            count = count + 1
        end
        mask = bit32.lshift(mask, 1)
    end
    return count
end

for i = 0, 256 do
    print(i, hamming(i))
end
