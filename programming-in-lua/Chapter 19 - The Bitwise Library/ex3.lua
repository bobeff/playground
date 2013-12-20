--[[
    Write a function to test whether the binary representation of a number is
palindrome.
]]

local function numberofbits(x)
    local i = 1
    while true do
        if 2 ^ i - 1 >= x then
            return i - 1
        end
        i = i + 1
    end
end

local function printbinary(x)
    local i = numberofbits(x)
    while i >= 0 do
        io.write(bit32.btest(x, 2 ^ i) and 1 or 0)
        i = i - 1
    end
end

function ispalindrome(x)
    local i = 1
    local j = 2 ^ numberofbits(x)
    while i <= j do
        if bit32.btest(x, i) ~= bit32.btest(x, j) then
            return false
        end
        i = bit32.lshift(i, 1)
        j = bit32.rshift(j, 1)
    end
    return true
end

for i = 0, 255 do
    io.write(i, "\t")
    printbinary(i)
    io.write("\t", ispalindrome(i) and "true" or "false", "\n")
end
