--[[
    Define the shift operations and the bitwise not using the arithmetic
operators in Lua.
]]

function shl(x, n)
    return x * 2 ^ n
end

function shr(x, n)
    return math.floor(x / 2 ^ n)
end

function _not(x)
    return 2 ^ 32 - 1 - x
end

for i = 0, 256 do
    for j = 0, 8 do
        assert(shl(i, j) == bit32.lshift(i, j))
        assert(shr(i, j) == bit32.rshift(i, j))
    end
    assert(_not(i) == bit32.bnot(i))
end
